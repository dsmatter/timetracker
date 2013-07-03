{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Api where

import Import
import Data.Time
import Data.Maybe

import App.Task
import App.Tag
import App.Util

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
  -- Manual "on delete cascade"
  runDB $ deleteWhere [TaskTagTask ==. tid']
  runDB $ deleteWhere [TaskLogTask ==. tid']
  runDB $ deleteWhere [TaskStartTask ==. tid']
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

deleteSessionR :: TaskLogId -> Handler RepHtml
deleteSessionR tlid = do
  session <- runDB $ fmap fromJust $ get tlid
  checkTaskIdPermission $ taskLogTask session
  _ <- runDB $ delete tlid
  getPartialTaskHtml $ taskLogTask session

postSessionR :: TaskLogId -> Handler RepHtml
postSessionR tlid = do
  session <- runDB $ fmap fromJust $ get tlid
  checkTaskIdPermission $ taskLogTask session
  start <- checkPostParameter "start"
  end <- checkPostParameter "end"
  let mstartTime = parseZonedTime start
  let mendTime = parseZonedTime end
  case [mstartTime, mendTime] of
    [Just startTime, Just endTime] -> do
      runDB $ update tlid [TaskLogStart =. startTime, TaskLogEnd =. endTime]
      getPartialTaskHtml $ taskLogTask session
    _ -> invalidArgs ["start", "end"]
