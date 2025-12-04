{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module SiteStructure.ContentsPage (
   makeChapterIndex
) where

import Data.Text (Text)
import Data.String
import System.OsPath (makeRelative)

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal (MarkupM(Empty))

import SiteStructure.RecordTypes
import LatexToHtml.PageTemplate (subchapterPageHtml)
import SiteStructure.AddressManagement

--- Aim: Make a function thateats type ChapterIndex and spits out
--    a HTML corresponding to pageContents for a list with linked
--    pages and toggleable descriptions

makeChapterIndex :: ChapterIndex -> Html
makeChapterIndex chapterindex = let
   secsWorker :: [IndexedSection] -> [Html]
   secsWorker [] = []
   secsWorker (x:xs) = (\y -> y:secsWorker xs) . li $ do
      H.a ! href (fromString x.address) $ toHtml x.title
      H.p $ toHtml x.description -- make this toggleable later
   chapWorker :: [IndexedChapter] -> [Html]
   chapWorker [] = []
   chapWorker (x:xs) = (\y -> y:chapWorker xs) . li $ do
      H.a ! href (fromString x.address) $ toHtml x.title
      H.p $ toHtml x.description -- make this toggleable later
      ul $ foldr (>>) (Empty ()) (secsWorker x.sections)
   in ol $ foldr (>>) (Empty ()) (chapWorker chapterindex.chapters)




-- docTypeHtml $ do
--    H.head $ do
--       meta ! charset "utf-8"
--       meta ! name "viewport" ! content "width=device-width"
--       link ! rel "stylesheet" ! href "https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/katex.min.css"
--       link ! rel "stylesheet" ! href "styles.css"
--       script ! defer "" ! src "https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/katex.min.js" $ Empty ()
--       script katexArgs
--       script ! defer "" ! src "https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/contrib/auto-render.min.js"
--          ! onload "renderMathInElement(document.body, katexargs);" $ Empty ()
--       H.title "Application Unification"
--    body $ do
--       H.section ! class_ "pagebound" $ do
--          H.div ! A.id "barone" ! class_ "bar-one" $ do
--             header ! class_ "flex-col margin15" $ do
--                h1 ! class_ "marginsmall" $ "Application Unification"
--                p ! class_ "marginless" $ "A Serialized Online Textbook by ClocksSugars"
--          H.div ! A.id "bartwo" ! class_ "bar-two" $ do
--             p ! class_ "marginless" $ "appliuni/Index"
--          h2 "Table of Contents"
--          ol $ do
--             li "Preliminaries"
--             ul $ do
--                li "Nascent's Philosophy of Math"
--                li "Propositional Logic"
--             li "Anatomy of $\\mathbb{R}^n$"
--             ul $ do
--                li "Real Numbers from Axioms"
