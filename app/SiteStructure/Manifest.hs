{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
--{-# LANGUAGE DeriveGeneric #-}

module SiteStructure.Manifest (
   tempappliuni,
   ) where

--import GHC.Generics
--import aeson
import SiteStructure.RecordTypes
import Data.Text (Text)


--- This should all be moved to a json file later!!
tempappliuni :: WrittenWorkBook
tempappliuni = WrittenWorkBook {
   name = "appliuni",
   title = "Application Unification",
   chapters = [
      Chapter {
         name = "prelims",
         title = "Preliminaries",
         description = "chapter description here",
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
