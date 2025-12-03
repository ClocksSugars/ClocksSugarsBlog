--{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module SiteStructure.IndexPage (
  IndexedSection(..),
  IndexedChapter(..),
  ChapterIndex(..)
) where

import Data.Text (Text)


data IndexedSection = IndexedSection {
   address :: String,
   title :: String,
   description :: Text
}

data IndexedChapter = IndexedChapter {
   address :: String,
   title :: String,
   description :: Text,
   sections :: [IndexedSection]
}

data ChapterIndex = ChapterIndex {
   chapters :: [IndexedChapter]
}
