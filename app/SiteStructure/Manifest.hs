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
               description = "A philosophical discussion of what a theory does for us, what mathematical thought is as a category for theories, and how to read the language of mathematical texts.",
               depends = [
                  "bentcylinder.svg"
                  ]
               },
            SubChapter {
               name = "proptypes",
               title = "Propositional Logic: A Programming Inspired Approach",
               description = "An introduction to dependent type theory and a basic discussion of proofs-as-programs, illustrating the reasoning style of propositional logic in a programmatic way.",
               depends = []
               },
            SubChapter {
               name = "maththink",
               title = "Rewrites and Sets: The Cognitive Weapons of Math",
               description = "in progress, better if you dont read it yet as i will be changing it a lot",
               depends = []
               }
            ]
         }
      ]
   }
