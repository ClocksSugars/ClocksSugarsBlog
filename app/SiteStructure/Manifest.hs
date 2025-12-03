{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
--{-# LANGUAGE DeriveGeneric #-}

module SiteStructure.Manifest where

--import GHC.Generics
--import aeson
import Data.Text (Text)

data SubChapter = SubChapter {
   name :: String,
   title :: Text,
   description :: Text,
   depends :: [String]
}

data Chapter = Chapter {
   name :: String,
   title :: Text,
   sections :: [SubChapter]
}

data WrittenWorkBook = WrittenWorkBook {
   name :: String,
   title :: Text,
   chapters :: [Chapter]
}


--- This should all be moved to a json file later!!
tempappliuni = WrittenWorkBook {
   name = "appliuni",
   title = "Application Unification",
   chapters = [
      Chapter {
         name = "prelims",
         title = "Preliminaries",
         sections = [
            SubChapter {
               name = "philofmath",
               title = "Nascent's Philosophy of Mathematics",
               description = "description goes here",
               depends = [
                  "bentcylinder.svg"
                  ]
               },
            SubChapter {
               name = "proptypes",
               title = "Propositional Logic: A Programming Inspired Approach",
               description = "description goes here x2",
               depends = []
               }
            ]
         }
      ]
   }
