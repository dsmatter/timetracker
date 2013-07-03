{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Summary where

import Import
import Database.Persist.Store
import qualified Data.Text as T

import App.Task
import App.Util

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

