{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module SiteStructure.MainTools (
   getAppliUniManifest,
   webBookFromManifest,
   articlesFromManifest,
   makeAppliUniManifestFromFile,
   makeArticleManifestFromFile,
   withManifest,
   getWithManifest,
   writeHomePage
) where

import System.Directory (copyFile)

import Text.Blaze.Html5 (Html)
import Text.Blaze.Html.Renderer.String (renderHtml)

import LatexToHtml.MainTools
import SiteStructure.AddressManagement
import SiteStructure.Manifest
import SiteStructure.RecordTypes (WrittenWorkBook (..), AllMyArticles(..), PageConstructInfo(..))
import SiteStructure.DefaultPage
import SiteStructure.WorkHorse
import SiteStructure.ContentsPage

webBookFromManifest :: WrittenWorkBook -> RefIndexState -> IO (Maybe RefIndexState)
webBookFromManifest workbook refstate = let
   (contentprogram, chapterindex) = parseBook workbook
   demaybe :: (RefIndexState -> Html -> IO (Maybe RefIndexState)) -> IO (Maybe RefIndexState)
   demaybe x = do
      mfinalrefs <- contentprogram refstate
      mgetTail <- parseTail
      case (mfinalrefs, mgetTail) of
         (Just finalrefs, Just thetail) -> do
            _ <- parsePreface finalrefs
            x finalrefs thetail
         _ -> do {putStrLn "Ended in Failure"; return Nothing}
   in demaybe $ \refs thetail -> do
      putStrLn "Pages Created Successfully in \"public\" folder"
      writeFile ("public/" <> workbook.name <> "/index.html") $
         renderHtml $ defaultPageHTML $  PageConstructInfo
               "../styles.css"
               "Application Unification"
               "Application Unification"
               "A Serialized Online Textbook by ClocksSugars"
               "Table of Contents"
               (addressListHtml [workbook.name])
               []
               (makeChapterIndex chapterindex >> thetail)
      copyFile "styles.css" "public/styles.css"
      return $ Just refs

articlesFromManifest :: AllMyArticles -> RefIndexState -> IO (Maybe RefIndexState)
articlesFromManifest thearticles refstate = let
   (contentprogram, articlesIndex) = parseArticles thearticles
   contentstablepage :: Html
   contentstablepage = makeArticlesIndexPage
      articlesIndex
      (addressListHtml ["articles"])
   demaybe :: (RefIndexState -> IO (Maybe RefIndexState)) -> IO (Maybe RefIndexState)
   demaybe x = do
      mfinalrefs <- contentprogram refstate
      case mfinalrefs of
         Nothing -> do {putStrLn "Ended in Failure"; return Nothing}
         Just finalrefs -> x finalrefs
   in demaybe $ \refs -> do
      putStrLn "Pages Created Successfully in \"public\" folder"
      writeFile "public/articles/index.html" $
         renderHtml contentstablepage
      return $ Just refs
