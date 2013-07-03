{-# LANGUAGE TupleSections, OverloadedStrings #-}

module App.Task where

import Import
import Database.Persist.Query.Join
import Data.List (head)
import Data.Maybe
import Data.Time
import System.Locale
import Text.Printf
import Data.Aeson (ToJSON)
import qualified Data.Text as T
import qualified Data.Conduit as C                                                                 
import qualified Data.Conduit.List as CL  

type Session = (TaskLogId, ZonedTime, ZonedTime)

data TaskInfo = TaskInfo { tid      :: TaskId
                         , name     :: Text
                         , tags     :: [Tag]
                         , sessions :: [Session]
                         , started  :: Maybe ZonedTime
                         }

instance ToJSON TaskInfo where
  toJSON info@(TaskInfo tid' name' tags' sessions' _) =
    object \
    [ ("id", toJSON $ tid')
    , ("name", toJSON $ name')
    , ("tags", toJSON $ map tagName tags')
    , ("sessions", toJSON $ map toSessionObject sessions')
    , ("total", toJSON $ showNominalDiffTime $ taskTotalTime info)
    ]
      where toSessionObject s@(_,from,to) = object $
              [ ("from", toJSON from), ("to", toJSON to)
              , ("duration", toJSON $ showNominalDiffTime $ sessionDiff s)]

sessionCompare :: Session -> Session -> Ordering
sessionCompare (_,a,_) (_,b,_) = compare (zonedTimeToUTC a) (zonedTimeToUTC b)

showNominalDiffTime :: NominalDiffTime -> String
showNominalDiffTime dt =
    let totalMins = dt / 60
        hours = floor $ totalMins / 60   :: Int
        mins  = round totalMins `mod` 60 :: Int
    in  printf "%02d:%02d" hours mins

sessionDiff :: Session -> NominalDiffTime
sessionDiff (_,s,e) = diffUTCTime (zonedTimeToUTC e) (zonedTimeToUTC s)

taskTotalTime :: TaskInfo -> NominalDiffTime
taskTotalTime = sum . map sessionDiff . sessions

fullDateFormat :: String
fullDateFormat = "%Y-%m-%d %H:%M"

formatToD :: ZonedTime -> String
formatToD = formatTime defaultTimeLocale "%H:%M"

formatDate :: ZonedTime -> String
formatDate = formatTime defaultTimeLocale "%Y-%m-%d"

parseZonedTime :: Text -> Maybe ZonedTime
parseZonedTime = Data.Time.parseTime defaultTimeLocale fullDateFormat . T.unpack

taskTagsText :: TaskInfo -> Text
taskTagsText = T.unwords . map (T.append "@" . tagName) . tags

--
-- Database access
--

getTagId :: Tag -> Handler TagId
getTagId tag' = do
  mtag <- runDB $ selectFirst [TagName ==. tagName tag'] []
  case mtag of
    Just etag -> return $ entityKey etag
    Nothing -> runDB $ insert tag'

setTaskTags :: TaskId -> [TagId] -> Handler ()
setTaskTags tid' tags' = do
  runDB $ deleteWhere [TaskTagTask ==. tid']
  mapM_ (runDB . insert . TaskTag tid') tags'

getTaskStart :: TaskId -> Handler (Maybe ZonedTime)
getTaskStart tid' = do
    mstart <- runDB $ selectFirst [TaskStartTask ==. tid'] []
    return $ fmap (taskStartStart . entityVal) mstart

getTaskInfo :: TaskId -> Handler TaskInfo
getTaskInfo tid' = do
    let logJoin = (selectOneMany (TaskLogTask <-.) taskLogTask) \
      { somIncludeNoMatch = True, somFilterOne = [TaskId ==. tid'] }
    let tagJoin = (selectOneMany (TaskTagTask <-.) taskTagTask) \
      { somIncludeNoMatch = True, somFilterOne = [TaskId ==. tid'] }
    taskWithLog <- fmap head $ runDB $ runJoin logJoin
    taskWithTags <- fmap head $ runDB $ runJoin tagJoin
    mstart <- getTaskStart tid'
    tags' <- getTags taskWithTags

    return $ TaskInfo tid' (taskName $ getTask taskWithLog) tags' (getSessions taskWithLog) mstart
      where getTask (etask,_) = entityVal etask
            getTags (_,tts) = mapM (fmap fromJust . runDB . get . taskTagTag . entityVal) tts
            getSessions (_,logs) = map h logs
            h l = (entityKey l, taskLogStart (entityVal l), taskLogEnd (entityVal l))

getAllTaskInfo :: UserId -> Handler [TaskInfo]
getAllTaskInfo uid =
  runDB $ (selectKeys [TaskUser ==. uid] [] C.$= CL.mapM (lift . getTaskInfo)) C.$$ CL.consume

