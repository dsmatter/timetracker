{-# LANGUAGE TupleSections, OverloadedStrings #-}
module App.Util where

import Import
import Text.Hamlet (hamletFile)
import Data.List (sortBy)
import Data.Time
import Data.Time.Clock.POSIX
import Data.Maybe
import Yesod.Auth

import App.Task

zonedTimeToMillies :: ZonedTime -> Int
zonedTimeToMillies zt =
  let secs = utcTimeToPOSIXSeconds $ zonedTimeToUTC zt
      millies = secs * 1000
  in round millies

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

checkPostParameter :: Text -> Handler Text
checkPostParameter pname = do
  mparam <- lookupPostParam pname
  case mparam of
    Nothing -> invalidArgs [pname]
    Just p -> return p

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

getPartialTaskHtml :: TaskId -> Handler RepHtml
getPartialTaskHtml tid' = do
  taskInfo <- getTaskInfo tid'
  hamletToRepHtml $(hamletFile "templates/taskInfoWidget.hamlet")

