{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore "Use when" "Use null" -}
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
      putStrLn "-h for help\n -eb to write default json for book\n -ea to write default json for articles\n -e to write both json files\n -m to make site from json file\n -mb to make just book part of site from json file\n -l to make latex for pdf\n\n -e and -m or -l together always does -e first."
      else return ()

   if "-e" `elem` commandlineargs || ("-ea" `elem` commandlineargs && "-eb" `elem` commandlineargs) then
      do
         makeAppliUniManifestFromFile
         makeArticleManifestFromFile
         putStrLn "wrote manifest jsons"
   else if "-eb" `elem` commandlineargs then do
      makeAppliUniManifestFromFile
      putStrLn "wrote appliuni manifest json"
   else if "-ea" `elem` commandlineargs then do
      makeArticleManifestFromFile
      putStrLn "wrote articles manifest json"
   else return ()

   if "-m" `elem` commandlineargs then do
      makeSite
      writeHomePage
   else if "-mb" `elem` commandlineargs then do
      _ <- getWithManifest $ \manifest -> do
         refsIfSuccess <- webBookFromManifest manifest blankIndex
         copyAssetDepends assetDepends
         writeHomePage
         return refsIfSuccess
      return ()
   else return ()

   if "-l" `elem` commandlineargs then
      withManifest pdfBookFromManifest
      else return ()


makeSite :: IO ()
makeSite = do
   mrefstate <- getWithManifest $ \manifest -> do
      refsIfSuccess <- webBookFromManifest manifest blankIndex
      copyAssetDepends assetDepends
      return refsIfSuccess
   _ <- case mrefstate of
      Just refstate -> getWithManifest $
         \manifest -> articlesFromManifest manifest refstate
      Nothing -> return Nothing
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
