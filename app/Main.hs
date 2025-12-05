{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO
-- import Data.String
-- import Data.Text (Text)
-- import Control.Monad.Trans.Except

-- import Text.Blaze.Html5 as H
-- import Text.Blaze.Html5.Attributes as A
-- import Text.Blaze.Html.Renderer.String as R
-- import Text.Blaze.Internal (MarkupM(Empty))
-- import System.Directory

import LatexToHtml.MainTools
import LatexToHtml.Utils
import SiteStructure.MainTools
import Text.LaTeX.Base.Parser
import System.Directory (copyFile, createDirectoryIfMissing)

main :: IO ()
main = do
   refsIfSuccess <- webBookFromManifest tempappliuni blankIndex
   -- do weird studies of this reference list at your leisure
   copyAssetDepends assetDepends
   putStrLn "Goodbye"

assetDepends :: [String]
assetDepends = [
   "tribackgrey.png"
   ]

copyAssetDepends :: [String] -> IO ()
copyAssetDepends stuff = let
   worker :: [String] -> IO ()
   worker [] = return ()
   worker (x:xs) = do
      copyFile ("assets/" <> x) ("public/assets/" <> x)
      copyAssetDepends xs
   in do
      createDirectoryIfMissing True "public/assets"
      worker stuff


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
