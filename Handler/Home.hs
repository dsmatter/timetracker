{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

import App.Task
import App.Util

getHomeR :: Handler RepHtml
getHomeR = do
  defaultLayout $ do
    setTitle "TimeTrackR"
    uid <- lift $ getUser
    taskInfos <- lift $ getAllTaskInfo uid
    let taskInfoWidgets = map (\taskInfo -> $(widgetFile "taskInfoWidget")) taskInfos
    $(widgetFile "homepage")

