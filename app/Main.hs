{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO
import System.Environment ( getArgs )
-- import Data.String
-- import Data.Text (Text)
-- import Control.Monad.Trans.Except

-- import Text.Blaze.Html5 as H
-- import Text.Blaze.Html5.Attributes as A
-- import Text.Blaze.Html.Renderer.String as R
-- import Text.Blaze.Internal (MarkupM(Empty))
-- import System.Directory

import LatexToHtml.MainTools
import SiteStructure.MainTools
import LatexToPdf.MainTools

import LatexToHtml.Utils
import Text.LaTeX.Base.Parser
import System.Directory (copyFile, createDirectoryIfMissing)
import Data.Aeson (decodeFileStrict,encodeFile)
import Data.String (fromString)

main :: IO ()
main = do
   commandlineargs <- getArgs
   --putStrLn $ "Command line args were: " ++ show commandlineargs
   if "-h" `elem` commandlineargs || commandlineargs == [] then
      putStrLn "-h for help, -e to write default json, -m to make site from json file, -l to make latex pdf. -e and -m together always encodes json first."
      else return ()

   if "-e" `elem` commandlineargs then
      do
         makeAppliUniManifestFromFile
         putStrLn "wrote manifest json"
      else return ()

   if "-m" `elem` commandlineargs then
      makeSite
      else return ()

   if "-l" `elem` commandlineargs then
      withManifest pdfBookFromManifest
      else return ()


makeSite :: IO ()
makeSite = do
   _ <- getWithManifest $ \manifest -> do
      refsIfSuccess <- webBookFromManifest manifest blankIndex
      copyAssetDepends assetDepends
      return refsIfSuccess
   return ()


--- This is only for files which the entire site depends on
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
