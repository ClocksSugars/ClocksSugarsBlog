{-# LANGUAGE OverloadedRecordDot #-}
module SiteStructure.WorkHorse where

import System.IO
import System.Directory (copyFile)
-- import Data.Either.Utils
import Data.String

import SiteStructure.AddressManagement
import SiteStructure.RecordTypes
import LatexToHtml.MainTools

import Text.LaTeX.Base (LaTeX)
import Text.LaTeX.Base.Parser (parseLaTeX)

--- Here is the way this is intended to work:
--    The address argument (where present) is intended
--    to act as a location that is relative both for the input files
--    (the latexraw files) and the output files (public). There should
--    also be an output target for logs.
--- Moreover the design here is not to actually output anything here.
--    Most of the work here is in turning the manifest into a big IO
--    monad program that will only at the end do anything.



parseSubChapter ::
   FolderPath ->
   SubChapter ->
   (
      RefIndexState -> IO (Maybe RefIndexState),
      IndexedSection
   )
parseSubChapter address subchapter = let
   docaddress = (subchapter.name : address)
   theindex :: IndexedSection
   theindex = IndexedSection
         ("/" <> folderPathRender docaddress <> ".html")
         subchapter.title
         subchapter.description
   theprogram :: RefIndexState -> IO (Maybe RefIndexState)
   theprogram refinds = let
      resetAllButReferences = blankIndex {references = refinds.references}
      copyassets :: [String] -> IO ()
      copyassets [] = return ()
      copyassets (x:xs) = (>>) (do
            copyFile
               ("latexraw/" <> folderPathRender address <> "/" <> x)
               ("public/" <> folderPathRender address <> "/" <> x)
            ) $ copyassets xs
      parseSuccessCase :: LaTeX -> IO RefIndexState
      parseSuccessCase doc = do
         let (thepage,newrefs,logs) = writePage
               subchapter.title
               subchapter.name
               (addressListHtml docaddress)
               (extractDocument doc)
               resetAllButReferences
         writeFileMakePath (docaddress ++ ["public"]) ".html" thepage
         writeFileMakePath (docaddress ++ ["logs"]) "0.txt" (logs !! 0)
         writeFile ("logs/" <> folderPathRender docaddress <> "1.txt") (logs !! 1)
         writeFile ("logs/" <> folderPathRender docaddress <> "2.txt") (logs !! 2)
         copyassets subchapter.depends
         return newrefs
      in do
      handle <- openFile
         ("latexraw/" <> folderPathRender docaddress <> ".tex")
         ReadMode
      xs <- hGetContents handle
      outc <- case (parseLaTeX . fromString $ xs) of
         Left _ -> do
            putStrLn ("Failure on " <> folderPathRender docaddress)
            return Nothing
         Right doc -> Just <$> parseSuccessCase doc
      hClose handle
      return outc
   in (theprogram, theindex)

parseChapter ::
   FolderPath ->
   Chapter ->
   (
      RefIndexState -> IO (Maybe RefIndexState),
      IndexedChapter
   )
parseChapter address chapter = let
   chapaddress = chapter.name : address
   theindex :: [IndexedSection] -> IndexedChapter
   theindex = IndexedChapter
         ("/" <> folderPathRender chapaddress)
         chapter.title
         chapter.description
   sectionWorker :: [SubChapter] ->
      (RefIndexState -> IO (Maybe RefIndexState), [IndexedSection])
   sectionWorker [] = (\ x -> do {return (Just x)}, [])
   sectionWorker (x:xs) = let
      (programhead, indexedsechead) = parseSubChapter chapaddress x
      (programtail, indexedsectail) = sectionWorker xs
      theprogram :: RefIndexState -> IO (Maybe RefIndexState)
      theprogram refstate = do
         mayberefs <- programhead refstate
         case mayberefs of
            Nothing -> do {putStrLn "Exiting chapter"; return Nothing}
            Just refout -> do {programtail refout}
      in (theprogram , indexedsechead:indexedsectail)
   (endprogram, listofindexsections) = sectionWorker chapter.sections
   in (endprogram, theindex listofindexsections)

parseBook :: WrittenWorkBook ->
   (RefIndexState -> IO (Maybe RefIndexState), ChapterIndex)
parseBook book = let
   address = [book.name]
   chapterWorker :: [Chapter] ->
      (RefIndexState -> IO (Maybe RefIndexState), [IndexedChapter])
   chapterWorker [] = (\ x -> do {return (Just x)}, [])
   chapterWorker (x:xs) = let
      (programhead, indexedchaphead) = parseChapter address x
      (programtail, indexedchaptail) = chapterWorker xs
      theprogram :: RefIndexState -> IO (Maybe RefIndexState)
      theprogram refstate = do
         mayberefs <- programhead refstate
         case mayberefs of
            Nothing -> do {putStrLn "Exiting chapter"; return Nothing}
            Just refout -> do {programtail refout}
      in (theprogram , indexedchaphead:indexedchaptail)
   (endprogram, listofindexchapters) = chapterWorker book.chapters
   in (endprogram, ChapterIndex listofindexchapters)
