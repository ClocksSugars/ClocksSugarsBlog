{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module SiteStructure.MainTools (
   getAppliUniManifest,
   webBookFromManifest,
   articlesFromManifest,
   makeAppliUniManifestFromFile,
   makeArticleManifestFromFile,
   withManifest,
   getWithManifest
) where

import System.Directory (copyFile)

import Text.Blaze.Html5 (Html)
import Text.Blaze.Html.Renderer.String (renderHtml)

import LatexToHtml.MainTools
import SiteStructure.AddressManagement
import SiteStructure.Manifest
import SiteStructure.RecordTypes (WrittenWorkBook (..), AllMyArticles(..))
import SiteStructure.WorkHorse
import SiteStructure.ContentsPage

webBookFromManifest :: WrittenWorkBook -> RefIndexState -> IO (Maybe RefIndexState)
webBookFromManifest workbook refstate = let
   (contentprogram, chapterindex) = parseBook workbook
   contentstablepage :: Html
   contentstablepage = makeChapterIndexPage
      chapterindex
      (addressListHtml [workbook.name])
   demaybe :: (RefIndexState -> IO (Maybe RefIndexState)) -> IO (Maybe RefIndexState)
   demaybe x = do
      mfinalrefs <- contentprogram refstate
      case mfinalrefs of
         Nothing -> do {putStrLn "Ended in Failure"; return Nothing}
         Just finalrefs -> x finalrefs
   in demaybe $ \refs -> do
      putStrLn "Pages Created Successfully in \"public\" folder"
      writeFile ("public/" <> workbook.name <> "/index.html") $
         renderHtml contentstablepage
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

parsePreface :: RefIndexState -> IO (Maybe RefIndexState)
parsePreface refinds = let
   addressWeUse = ["preface","appliuni"]
   theprogram :: RefIndexState -> IO (Maybe RefIndexState)
   theprogram refinds = let
      resetAllButReferences = resetNoneMapInd refinds
      parseSuccessCase :: LaTeX -> IO RefIndexState
      parseSuccessCase doc = do
         let (thepagehtml,newrefs,logs) = writePage
               subchapter.name
               (folderPathRender docaddress)
               subchapter.flags
               (extractDocument doc)
               resetAllButReferences
         let thepage = renderHtml $ defaultPageHTML $ PageConstructInfo
               ((++ "../../styles.css") $ if isIndexStyle then "../" else "")
               "Application Unification"
               "Application Unification"
               tagline
               "Preface"
               (addressListHtml addressWeUse)
               []
               thepagehtml
         writeFileMakePath (docaddress ++ ["public"]) ".html" thepage
         writeFileMakePath (docaddress ++ ["logs"]) "0.txt" (logs !! 0)
         writeFile ("logs/" <> folderPathRender docaddress <> "1.txt") (logs !! 1)
         writeFile ("logs/" <> folderPathRender docaddress <> "2.txt") (logs !! 2)
         copyassets subchapter.depends
         putStrLn $ "Success on " ++ subchapter.name
         return newrefs
      in do
         xs <- readFileTex ("latexraw/appliuni_head.tex")
         case (parseLaTeX xs) of
            Left theerror -> do
               putStrLn ("Failure on preface")
               print theerror
               return Nothing
            Right doc -> Just <$> parseSuccessCase doc
   in (theprogram, theindex)
