{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Text.Hamlet (hamletFile)
--import Database.Persist.Store
import Database.Persist.Query.Join
--import Database.Persist.GenericSql.Raw
import Data.List (sortBy, head)
import Data.Time
import Data.Time.Clock.POSIX
import System.Locale
import Data.Maybe
import Text.Printf
import Data.Aeson (ToJSON)
import Database.Persist.Store
import Yesod.Auth
import qualified Data.Text as T
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

type Session = (TaskLogId,ZonedTime,ZonedTime)
data TaskInfo = TaskInfo { tid :: TaskId, name :: Text, tags :: [Tag], sessions :: [Session], started :: Maybe ZonedTime }

instance ToJSON TaskInfo where
  toJSON info =
    object \
    [ ("id", toJSON $ tid info)
    , ("name", toJSON $ name info)
    , ("tags", toJSON $ map tagName $ tags info)
    , ("sessions", toJSON $ map addTotal $ sessions info)
    , ("total", toJSON $ showNominalDiffTime $ taskTotalTime info)
    ]
      where addTotal s@(_,z1,z2) = ((z1,z2), showNominalDiffTime $ sessionDiff s)

sessionCompare :: Session -> Session -> Ordering
sessionCompare (_,a,_) (_,b,_) = compare (zonedTimeToUTC a) (zonedTimeToUTC b)

showNominalDiffTime :: NominalDiffTime -> String
showNominalDiffTime dt =
    let totalMins = dt / 60
        hours = floor $ totalMins / 60 :: Int
        mins = mod (round totalMins) 60 :: Int
    in printf "%02d:%02d" hours mins

sessionDiff :: Session -> NominalDiffTime
sessionDiff (_,s,e) = diffUTCTime (zonedTimeToUTC e) (zonedTimeToUTC s)

taskTotalTime :: TaskInfo -> NominalDiffTime
taskTotalTime = sum . map sessionDiff . sessions

formatToD :: ZonedTime -> String
formatToD = formatTime defaultTimeLocale "%H:%M"

formatDate :: ZonedTime -> String
formatDate = formatTime defaultTimeLocale "%Y-%m-%d"

zonedTimeToMillies :: ZonedTime -> Int
zonedTimeToMillies zt =
  let secs = utcTimeToPOSIXSeconds $ zonedTimeToUTC zt
      millies = secs * 1000
  in round millies

taskTagsText :: TaskInfo -> Text
taskTagsText = T.unwords . map (T.append "@" . tagName) . tags

showKey :: Key a -> String
showKey key = case fromPersistValue $ unKey key of
    Left _ -> ""
    Right a -> a

partitionBy :: Ord b => (a -> b) -> [a] -> [(b,[a])]
partitionBy f = reduce' . sortBy cmp' . map map'
    where map' a = (f a, [a])
          cmp' (b,_) (b',_) = compare b b'
          reduce' [] = []
          reduce' [x] = [x]
          reduce' ((b,a):sec@((b',[a']):xs)) | b == b' = reduce' $ (b,a':a):xs
                                       | otherwise = (b,a):(reduce' sec)
          reduce' _ = []

partitionSessions :: [Session] -> [(String,[Session])]
partitionSessions = partitionBy h
    where h (_,a,_) = formatDate a

extractTags :: Text -> (Text, [Tag])
extractTags tname' =
  let (n:t) = T.splitOn " @" tname'
      tname = T.strip n
      tags'  = map (Tag . T.strip) t
  in (tname, tags')

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

secondsSince :: ZonedTime -> IO NominalDiffTime
secondsSince zt = do
  now <- getZonedTime
  return $ sessionDiff (undefined, now, zt)

getUser :: Handler UserId
getUser = do
  muser <- maybeAuthId
  case muser of
    Nothing -> permissionDenied "Login, please"
    Just u -> return u

checkTaskPermission :: Task -> Handler ()
checkTaskPermission task' = do
  uid <- getUser
  case (taskUser task') of
    tuid | tuid == uid -> return ()
         | otherwise -> permissionDenied "That's not your task!"

checkTaskIdPermission :: TaskId -> Handler ()
checkTaskIdPermission tid' = do
  task' <- runDB $ fmap fromJust $ get tid'
  checkTaskPermission task'

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

getAllTaskInfo :: Handler [TaskInfo]
getAllTaskInfo = do
  uid <- getUser
  runDB $ (selectKeys [TaskUser ==. uid] [] C.$= CL.mapM (lift . getTaskInfo)) C.$$ CL.consume

getPartialTaskHtml :: TaskId -> Handler RepHtml
getPartialTaskHtml tid' = do
  taskInfo <- getTaskInfo tid'
  hamletToRepHtml $(hamletFile "templates/taskInfoWidget.hamlet")

getHomeR :: Handler RepHtml
getHomeR = do
  defaultLayout $ do
    setTitle "TimeTrackR"
    taskInfos <- lift getAllTaskInfo
    let taskInfoWidgets = map (\taskInfo -> $(widgetFile "taskInfoWidget")) taskInfos
    $(widgetFile "homepage")


postAddTaskR :: Handler RepHtml
postAddTaskR = do
    mTaskName <- lookupPostParam "name"
    case mTaskName of
        Nothing -> invalidArgs ["name"]
        Just taskName' -> do
            -- Insert the new task
            uid <- getUser
            let (name', tags') = extractTags taskName'
            taskId <- runDB $ insert $ Task uid name'
            -- Set the associated tags
            tagIds <- mapM getTagId tags'
            setTaskTags taskId tagIds
            -- Return partial HTML
            getPartialTaskHtml taskId

deleteTaskR :: TaskId -> Handler RepPlain
deleteTaskR tid' = do
  checkTaskIdPermission tid'
  runDB $ delete tid'
  return $ RepPlain "deleted"

putTaskR :: TaskId -> Handler RepHtml
putTaskR tid' = do
  checkTaskIdPermission tid'
  mTaskName <- lookupPostParam "name"
  case mTaskName of
    Nothing -> invalidArgs ["name"]
    Just tname -> do
      -- Change the task name
      let (name', tags') = extractTags tname
      runDB $ update tid' [TaskName =. name']
      -- Change the associated tags
      tagIds <- mapM getTagId tags'
      setTaskTags tid' tagIds
      -- Return partial HTML
      getPartialTaskHtml tid'

postTaskStartR :: TaskId -> Handler RepHtml
postTaskStartR tid' = do
  now <- liftIO getZonedTime
  _ <- runDB $ insert $ TaskStart tid' now
  getPartialTaskHtml tid'

postTaskStopR :: TaskId -> Handler RepHtml
postTaskStopR tid' = do
  checkTaskIdPermission tid'
  now <- liftIO getZonedTime
  mstart <- runDB $ getBy $ UniqueTaskStart tid'
  case mstart of
    Nothing -> invalidArgs ["not started"]
    Just estart -> do
      let startTime = taskStartStart $ entityVal estart
      _ <- runDB $ insert $ TaskLog tid' startTime now
      _ <- runDB $ delete $ entityKey estart
      getPartialTaskHtml tid'

getTaskSummaryR :: TaskId -> Handler RepJson
getTaskSummaryR tid' = do
  checkTaskIdPermission tid'
  taskInfo <- getTaskInfo tid'
  jsonToRepJson $ taskInfo

getSummaryR :: Handler RepHtmlJson
getSummaryR = do
  mtasks <- lookupGetParam "tasks"
  case mtasks of
    Nothing -> invalidArgs ["tasks"]
    Just stasks -> do
      let taskIds = map (Key . PersistText) $ T.splitOn "-" stasks
      mapM_ checkTaskIdPermission taskIds
      taskInfos <- mapM getTaskInfo taskIds
      let total = sum $ map taskTotalTime taskInfos
          json = object \
                  [ ("tasks" :: Text, toJSON taskInfos)
                  , ("total" :: Text, toJSON $ showNominalDiffTime total)
                  ]
      defaultLayoutJson $(widgetFile "summary") json

deleteSessionR :: TaskLogId -> Handler RepHtml
deleteSessionR tlid = do
  session <- runDB $ fmap fromJust $ get tlid
  checkTaskIdPermission $ taskLogTask session
  _ <- runDB $ delete tlid
  getPartialTaskHtml $ taskLogTask session