{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
--{-# LANGUAGE DeriveGeneric #-}

module SiteStructure.Manifest (
   makeAppliUniManifestFromFile,
   getAppliUniManifest,
   withManifest,
   getWithManifest,
   getArticleManifest,
   makeArticleManifestFromFile
   ) where

--import GHC.Generics
--import aeson
import System.IO
import SiteStructure.RecordTypes
-- import Data.Text (Text)
import Data.String
import Data.Aeson (decodeFileStrict,encodeFile)

--- obviously would prefer this to just import a byte string but im stuggling to
---   figure out which kind of byte string it uses

class SiteZoneManifest sitezone where
   getWithManifest :: (sitezone -> IO (Maybe a)) -> IO (Maybe a)
   withManifest :: (sitezone -> IO a) -> IO ()

instance SiteZoneManifest WrittenWorkBook where
   getWithManifest func = do
      mManifest <- getAppliUniManifest
      result <- case mManifest of
         Just manifest -> do
            putStrLn "Good appliuni Manifest"
            func manifest
         Nothing -> do
            putStrLn "Bad appliuni Manifest"
            return Nothing
      putStrLn "Goodbye"
      return result

   withManifest func = do
      mManifest <- getAppliUniManifest
      case mManifest of
         Just manifest -> do
            putStrLn "Good appliuni Manifest"
            _ <- func manifest
            return ()
         Nothing -> do
            putStrLn "Bad appliuni Manifest"
      putStrLn "Goodbye"

instance SiteZoneManifest AllMyArticles where
   getWithManifest func = do
      mManifest <- getArticleManifest
      result <- case mManifest of
         Just manifest -> do
            putStrLn "Good article Manifest"
            func manifest
         Nothing -> do
            putStrLn "Bad article Manifest"
            return Nothing
      putStrLn "Goodbye"
      return result

   withManifest func = do
      mManifest <- getArticleManifest
      case mManifest of
         Just manifest -> do
            putStrLn "Good article Manifest"
            _ <- func manifest
            return ()
         Nothing -> do
            putStrLn "Bad article Manifest"
      putStrLn "Goodbye"

getAppliUniManifest :: IO (Maybe WrittenWorkBook)
getAppliUniManifest = decodeFileStrict "APPLIUNIMANIFEST.json"

---contingency for if json parsing on handmade json keeps failing
makeAppliUniManifestFromFile :: IO ()
makeAppliUniManifestFromFile = encodeFile "APPLIUNIMANIFEST.json" appliunistruct

getArticleManifest :: IO (Maybe AllMyArticles)
getArticleManifest = decodeFileStrict "ARTICLEMANIFEST.json"

makeArticleManifestFromFile :: IO ()
makeArticleManifestFromFile = encodeFile "ARTICLEMANIFEST.json" blogArticles

appliunistruct :: WrittenWorkBook
appliunistruct = WrittenWorkBook {
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
               flags = [],
               description = "(11 PDF Pages) A philosophical discussion of what a theory does for us, what mathematical thought is as a category for theories, and how to read the language of mathematical texts.",
               depends = [
                  "bentcylinder.svg"
                  ]
               },
            SubChapter {
               name = "proptypes",
               title = "Propositional Logic: A Programming Inspired Approach",
               flags = [],
               description = "(28 PDF Pages) An introduction to dependent type theory and a basic discussion of proofs-as-programs, illustrating the reasoning style of propositional logic in a programmatic way.",
               depends = []
               },
            SubChapter {
               name = "maththink",
               title = "Rewrites and Sets: The Cognitive Weapons of Math",
               flags = [],
               description = "(24 PDF Pages) in draft 1.5 stage. a mostly philosophical engagement on mathematical thinking + some basic set theory and what we mean by equivalence in a non-constructive world. Towards the end we tie off some notational loose ends and explicitly specify some more conventions.",
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
                  name = "realnumsax",
                  title = "Real Numbers from Axioms",
                  flags = [],
                  description = "(17 PDF Pages) A discussion of the properties of real numbers as derived by rewrites on their axioms, as well as our earliest focus on the concerns and mentality of real analysis.",
                  depends = []
                  },
               SubChapter {
                  name = "seqlimsinR",
                  title = "Sequences and Limits in $\\mathbb{R}$",
                  flags = [],
                  description = "(19 PDF Pages) Here we introduce the notion of limits on sequences, taking great care to both establish them as formal objects and as intuitive ones. In this section we begin the process of thinking about the study of limits as the study of what happens when you look very close to a point. For our Section appendix we discuss the similarities between real numbers as a number system and convergent sequences, and how limits preserve these similarities.",
                  depends = ["basicconvergence.svg"]
                  },
               SubChapter {
                  name = "openlimsR",
                  title = "Intervals in $\\mathbb{R}$ and Limit Characterizations",
                  flags = [],
                  description = "(15 PDF Pages, no appendix yet) In this section we formally discuss intervals in $\\mathbb{R}$, introducing notions of open and closed intervals, and open and closed sets. This will be our very first little taste of (non-algebraic) topological concerns, and accordingly we introduce the topological limit characterization, before paying off earlier promised theorems such as Bolzano-Weierstraß and the convergence of Cauchy sequences. For our section appendix we give a much more informal discussion on cardinality, explaining uncountable infinity and when we need to be concerned about it.",
                  depends = ["metricopensetproperty.svg"]
                  },
               SubChapter {
                  name = "funclimsR",
                  title = "Limits on $\\mathbb{R} \\to \\mathbb{R}$ Functions",
                  flags = ["DoNotShowOnIndex"],
                  description = "(WIP)",
                  depends = []
                  }
               ]
            }
      ]
   }

blogArticles :: AllMyArticles
blogArticles = AllMyArticles [
   SubChapter {
      name = "0126-heateq",
      title = "Solving the Heat Equation in Rust WASM with WebGPU (Explicit FTCS RK2) + Numerical Analysis",
      flags = ["IndexTopOfPage"],
      description = "Detailing my experience and lessons-learned in writing a rust heat equation solver in WGPU to run on a browser, with a truncation error and fourier stability analysis derivation of the explicit RK2 FTCS method for the heat equation. Written in a pedagogical tone so it can be used as a tutorial.",
      depends = [
         "heatflux.svg",
         "wiki_forward_Euler_method.svg",
         "wiki_Hsl-rgb_models.svg"
         ]
      }
   ]
