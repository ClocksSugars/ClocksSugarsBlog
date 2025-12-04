{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO
import Data.String
import Data.Text (Text)
import Control.Monad.Trans.Except

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String as R
import Text.Blaze.Internal (MarkupM(Empty))
import System.Directory

import LatexToHtml.MainTools
import LatexToHtml.Utils
import SiteStructure.Manifest
import Text.LaTeX.Base.Parser
import Data.Either.Utils

main :: IO ()
main = do
   writeFile "test.html" $ R.renderHtml $ docTypeHtml $ do
      H.head $ do
         meta ! charset "utf-8"
         meta ! name "viewport" ! content "width=device-width"
         link ! rel "stylesheet" ! href "https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/katex.min.css"
         link ! rel "stylesheet" ! href "styles.css"
         script ! defer "" ! src "https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/katex.min.js" $ Empty ()
         --script katexArgs
         script ! defer "" ! src "https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/contrib/auto-render.min.js"
            ! onload "renderMathInElement(document.body, katexargs);" $ Empty ()
         H.title "Application Unification"
      body $ do
         H.section ! class_ "pagebound" $ do
            H.div ! A.id "barone" ! class_ "bar-one" $ do
               header ! class_ "flex-col margin15" $ do
                  h1 ! class_ "marginsmall" $ "Application Unification"
                  p ! class_ "marginless" $ "A Serialized Online Textbook by ClocksSugars"
            H.div ! A.id "bartwo" ! class_ "bar-two" $ do
               p ! class_ "marginless" $ "appliuni/Index"
            h2 "Table of Contents"
            ol $ do
               li "Preliminaries"
               ul $ do
                  li "Nascent's Philosophy of Math"
                  li "Propositional Logic"
               li "Anatomy of $\\mathbb{R}^n$"
               ul $ do
                  li "Real Numbers from Axioms"



-- main :: IO ()
-- main = do
--    handle <- openFile "latexraw/anatomyRn/proptypes.tex" ReadMode
--    xs <- hGetContents handle
--    let doc = extractDocument . fromRight . parseLaTeX . fromString $ xs
--    writeFile "inspect0.txt" $ show doc
--    let part1 = processOne doc
--    writeFile "inspect1.txt" $ show part1
--    let part2 = processTwo part1
--    writeFile "inspect2.txt" $ show part2
--    let (part3, index) = processThree "proptypes" part2 blankIndex
--    writeFile "test.html" $ R.renderHtml $ pageHTML "Propositional Logic: A Constructive Approach" part3 --"Nascent's Philosophy of Mathematics" part3
--    writeFile "CumulativeReferences.txt" $ show . references $ index
--    hClose handle


-- main :: IO ()
-- main = do
--    writeFile "test.html" $ R.renderHtml $ pageHTML "some bs im testing" $ do
--       H.div ! class_ "definition" $ do
--          H.div ! class_ "definitiontitle" $ "Definition 1: Test"
--          p "here we would put some definition text etc etc"
