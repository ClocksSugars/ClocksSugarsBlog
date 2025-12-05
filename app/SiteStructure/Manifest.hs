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

--- make this file turn a json into the structure below!

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
               },
            SubChapter {
               name = "maththink",
               title = "Rewrites and Sets: The Cognitive Weapons of Math",
               description = "description goes here x3",
               depends = []
               }
            ]
         }
      ]
   }
