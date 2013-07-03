{-# LANGUAGE TupleSections, OverloadedStrings #-}

module App.Tag where

import Import
import qualified Data.Text as T

import App.Task

taskTagsText :: TaskInfo -> Text
taskTagsText = T.unwords . map (T.append "@" . tagName) . tags

extractTags :: Text -> (Text, [Tag])
extractTags tname' =
  let (n:t) = T.splitOn " @" tname'
      tname = T.strip n
      tags'  = map (Tag . T.strip) t
  in (tname, tags')

