{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Database.Persist.Store
import Database.Persist.Query.Join
import Database.Persist.GenericSql.Raw
import Data.List (sortBy)
import Data.Time
import System.Locale
import Text.Printf
import qualified Data.Text as T
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

type Session = (ZonedTime,ZonedTime)
data TaskInfo = TaskInfo { tid :: TaskId, name :: Text, tags :: [Tag], sessions :: [Session] }

sessionCompare :: Session -> Session -> Ordering
sessionCompare (a,_) (b,_) = compare (zonedTimeToUTC a) (zonedTimeToUTC b)

showNominalDiffTime :: NominalDiffTime -> String
showNominalDiffTime dt =
    let totalMins = dt / 60
        hours = round $ totalMins / 60 :: Int
        mins = mod (round totalMins) 60 :: Int
    in printf "%02d:%02d" hours mins

sessionDiff :: Session -> NominalDiffTime
sessionDiff (s,e) = diffUTCTime (zonedTimeToUTC e) (zonedTimeToUTC s)

taskTotalTime :: TaskInfo -> NominalDiffTime
taskTotalTime = sum . map sessionDiff . sessions

formatToD :: ZonedTime -> String
formatToD = formatTime defaultTimeLocale "%H:%M"

formatDate :: ZonedTime -> String
formatDate = formatTime defaultTimeLocale "%Y-%m-%d"

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
    where h (a,_) = formatDate a

getAllTaskInfo :: Handler [TaskInfo]
getAllTaskInfo = do
    let join = (selectOneMany (TaskLogTask <-.) taskLogTask) { somIncludeNoMatch = True }
    tasksWithLog <- runDB $ runJoin join
    let stmts = map getTagStmt tasksWithLog
    tasksTagData <- fmap (map concat) $ mapM (\stmt -> runDB $ stmt C.$$ CL.consume) stmts
    let tasksTags = map (map (\(PersistText t) -> Tag t)) tasksTagData
    let taskInfoData = zipWith (\(et,els) tgs -> (et,tgs,els)) tasksWithLog tasksTags
    return $ map (\(etask,tgs,elogs) -> TaskInfo (entityKey etask) (getName etask) tgs (getSessions elogs)) taskInfoData
        where getName = taskName . entityVal
              getSessions = sortBy sessionCompare . map ((splitPair taskLogStart taskLogEnd) . entityVal)
              splitPair f g v = (f v, g v)
              getTagStmt (et,_) = withStmt "SELECT t.name FROM task_tag tt, tag t WHERE tt.tag = t.id AND tt.task = ?" [unKey (entityKey et)]

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
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
            taskId <- runDB $ insert $ Task taskName'
            -- FIXME: no surrounding layout
            defaultLayout $ do
                let taskInfo = TaskInfo taskId taskName' [] []
                $(widgetFile "taskInfoWidget")

deleteTaskR :: TaskId -> Handler RepPlain
deleteTaskR tid' = do
    runDB $ delete tid'
    return $ RepPlain "deleted"
