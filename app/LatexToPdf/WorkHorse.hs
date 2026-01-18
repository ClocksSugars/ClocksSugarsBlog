{-# LANGUAGE OverloadedRecordDot #-}
module LatexToPdf.WorkHorse where

import System.IO
import System.Directory (copyFile)
import Data.String (fromString)
import Text.LaTeX.Base.Syntax (LaTeX(..),TeXArg(..))
import Text.LaTeX.Base.Parser (parseLaTeX)

import SiteStructure.AddressManagement
import LatexToHtml.MainTools
import SiteStructure.RecordTypes

parseSubChapter :: FolderPath -> SubChapter -> IO (Maybe LaTeX)
parseSubChapter address subchapter = let
   docaddress = (subchapter.name : address)
   copyassets :: [String] -> IO()
   copyassets [] = return ()
   copyassets (x:xs) = (do
      copyFile ("latexraw/" <> folderPathRender address <> "/" <> x) ("latexvomit/" <> x)
      ) >> copyassets xs
   theprogram :: IO (Maybe LaTeX)
   theprogram = do
      handle <- openFile
         ("latexraw/" <> folderPathRender docaddress <> ".tex")
         ReadMode
      xs <- hGetContents handle
      outc <- case (parseLaTeX . fromString $ xs) of
         Left _ -> do
            putStrLn ("Failure on " <> folderPathRender docaddress)
            return Nothing
         Right doc -> do
            putStrLn ("Success on " ++ subchapter.name)
            copyassets subchapter.depends
            return $ Just $ extractDocument doc
      parsedTitle <- case (parseLaTeX $ subchapter.title) of
         Left _ -> do
            putStrLn (folderPathRender docaddress <> " had invalid title")
            return Nothing
         Right finetitle -> return $ Just finetitle
      hClose handle
      return $ case ("DoNotShowOnIndex" `elem` subchapter.flags , outc, parsedTitle) of
         (True, Just cont, Just finetitle) -> Just $ TeXSeq
            (TeXComm "section" [FixArg finetitle])
            cont
         (False, _, _) -> Just TeXEmpty
         _ -> Nothing
   in theprogram

parseChapter :: FolderPath -> Chapter -> IO (Maybe LaTeX)
parseChapter address chapter = let
   chapaddress = chapter.name : address
   sectionWorker :: [SubChapter] -> IO (Maybe LaTeX)
   sectionWorker [] = return $ Just TeXEmpty
   sectionWorker (x:xs) = do
      programhead <- parseSubChapter chapaddress x
      programtail <- sectionWorker xs
      case (programhead,programtail) of
         (Just finehead, Just finetail) ->
            return $ Just $ TeXSeq finehead finetail
         _ -> return Nothing
   in do
      chaptertitle <- case (parseLaTeX $ chapter.title) of
         Left _ -> do
            putStrLn (folderPathRender chapaddress <> " had invalid title")
            return Nothing
         Right finetitle -> return $ Just finetitle
      mchapter <- sectionWorker chapter.sections
      case (mchapter,chaptertitle) of
         (Just chapter,Just finetitle) -> return $ Just $
            (TeXComm "chapter" [FixArg finetitle]) <> chapter
         _ -> return Nothing

parseBook :: WrittenWorkBook -> IO (Maybe LaTeX)
parseBook book = let
   address = [book.name]
   chapterWorker :: [Chapter] -> IO (Maybe LaTeX)
   chapterWorker [] = return $ Just TeXEmpty
   chapterWorker (x:xs) = do
      programhead <- parseChapter address x
      programtail <- chapterWorker xs
      case (programhead,programtail) of
         (Just finehead, Just finetail) -> return $ Just $ TeXSeq finehead finetail
         _ -> return Nothing
   in do
      mbook <- chapterWorker book.chapters
      case mbook of
         Just book -> return $ Just $ TeXEnv "document" [] $
            (TeXCommS "CreateFirstPage") <> book
         Nothing -> return Nothing
