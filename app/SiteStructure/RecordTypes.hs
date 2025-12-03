{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module SiteStructure.RecordTypes (
   IndexedSection(..),
   IndexedChapter(..),
   ChapterIndex(..),
   SubChapter(..),
   Chapter(..),
   WrittenWorkBook(..)
) where

import Data.Text (Text)

---- Due to the Duplicate Records Field pragma misbehaving
--    when record types across different files share names,
--    we're gonna try moving them here

data IndexedSection = IndexedSection {
   address :: String,
   title :: Text,
   description :: Text
}

data IndexedChapter = IndexedChapter {
   address :: String,
   title :: Text,
   description :: Text,
   sections :: [IndexedSection]
}

data ChapterIndex = ChapterIndex {
   chapters :: [IndexedChapter]
}

data SubChapter = SubChapter {
   name :: String,
   title :: Text,
   description :: Text,
   depends :: [String]
}

data Chapter = Chapter {
   name :: String,
   title :: Text,
   description :: Text,
   sections :: [SubChapter]
}

data WrittenWorkBook = WrittenWorkBook {
   name :: String,
   title :: Text,
   chapters :: [Chapter]
}
