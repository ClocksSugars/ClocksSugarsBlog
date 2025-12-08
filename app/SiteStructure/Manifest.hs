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
         description = "This chapter acts as an introduction to every concept that mathematicians intrinsically know but never say out loud that I could think of. The ultimate goal of this chapter is that you should come out the other side with enough awareness of what mathematicians are doing that you could read a math book, namely, my math book.",
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
               description = "in draft 1.5 stage. a mostly philosophical engagement on mathematical thinking + some basic set theory and what we mean by equivalence in a non-constructive world",
               depends = []
               }
            ]
         },
         Chapter {
            name = "anatomyRn",
            title = "Anatomy of $\\mathbb{R}^n$: A Brief Introduction to Real Analysis",
            description = "In this chapter we discuss some topics from real analysis with a focus on how these topics reflect on the nature of $\\mathbb{R}^n$ as a space and its deeper, stranger, properties. Throughout the chapter, we develop tools to study mathematical objects in $\\mathbb{R}^n$ as well as increasingly describing properties you'd never thought to point out, and discussing what happens in a space without that property. In this way, we slowly build up what the real numbers are from what they would be if they were not.",
            sections = [
               SubChapter {
                  name = "realnumsaxioms",
                  title = "Real Numbers from Axioms: Why $\\mathbb{R^n}$ is Weirder Than You Thought, Part 1",
                  description = "A discussion of the properties of real numbers as derived by rewrites on their axioms, as well as our earliest focus on the concerns and mentality of real analysis.",
                  depends = []
                  }
               ]
            }
      ]
   }
