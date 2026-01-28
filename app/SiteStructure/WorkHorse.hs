{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
module SiteStructure.WorkHorse where

import System.IO
import System.Directory (copyFile)
-- import Data.Either.Utils
import Data.String

import SiteStructure.AddressManagement
import SiteStructure.RecordTypes
import SiteStructure.DefaultPage
import LatexToHtml.MainTools

import Text.LaTeX.Base (LaTeX, readFileTex)
import Text.LaTeX.Base.Parser (parseLaTeXWith, ParserConf(..), ParseError)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html (Html)

import Data.Text (Text)

-- we need a custom parser that will ignore lstlisting envs
ourParseConf :: ParserConf
ourParseConf = ParserConf {
   verbatimEnvironments = ["verbatim", "lstlisting"]
}

parseLaTeX :: Text -> Either ParseError LaTeX
parseLaTeX = parseLaTeXWith ourParseConf


--- Here is the way this is intended to work:
--    The address argument (where present) is intended
--    to act as a location that is relative both for the input files
--    (the latexraw files) and the output files (public). There should
--    also be an output target for logs.
--- Moreover the design here is not to actually output anything here.
--    Most of the work here is in turning the manifest into a big IO
--    monad program that will only at the end do anything.

---   TODO: SEPARATE WRITECALLS INTO SEPARATE PROGRAM SO WE CAN DO ANALYTICS
--

parseSubChapter ::
   FolderPath ->
   SubChapter ->
   Bool ->
   String ->
   Text ->
   Text ->
   (
      RefIndexState -> IO (Maybe RefIndexState),
      IndexedSection
   )
parseSubChapter address subchapter isIndexStyle pagetitle pageh1 tagline = let
   docaddress = if isIndexStyle then "index" : subchapter.name : address else subchapter.name : address
   addressWeUse = if isIndexStyle then subchapter.name:address else address
   theindex :: IndexedSection
   theindex = IndexedSection
         ("/" <> folderPathRender (subchapter.name : address) <>
            if isIndexStyle then "" else ".html")
         subchapter.title
         subchapter.description
   theprogram :: RefIndexState -> IO (Maybe RefIndexState)
   theprogram refinds = let
      resetAllButReferences = resetNoneMapInd refinds
      copyassets :: [String] -> IO ()
      copyassets [] = return ()
      copyassets (x:xs) = (>>) (do
            copyFile
               ("latexraw/" <> folderPathRender addressWeUse <> "/" <> x)
               ("public/" <> folderPathRender addressWeUse <> "/" <> x)
            ) $ copyassets xs
      parseSuccessCase :: LaTeX -> IO RefIndexState
      parseSuccessCase doc = do
         let (thepagehtml,newrefs,logs) = writePage
               subchapter.name
               (folderPathRender $ if isIndexStyle then addressWeUse else docaddress)
               subchapter.flags
               (extractDocument doc)
               resetAllButReferences
         let thepage = renderHtml $ defaultPageHTML $ PageConstructInfo
               ((++ "../../styles.css") $ if isIndexStyle then "../" else "")
               pagetitle
               pageh1
               tagline
               subchapter.title
               (addressListHtml (subchapter.name : address))
               subchapter.flags
               thepagehtml
         writeFileMakePath (docaddress ++ ["public"]) ".html" thepage
         writeFileMakePath (docaddress ++ ["logs"]) "0.txt" (logs !! 0)
         writeFile ("logs/" <> folderPathRender docaddress <> "1.txt") (logs !! 1)
         writeFile ("logs/" <> folderPathRender docaddress <> "2.txt") (logs !! 2)
         copyassets subchapter.depends
         putStrLn $ "Success on " ++ subchapter.name
         return newrefs
      in do
         xs <- readFileTex ("latexraw/" <> folderPathRender docaddress <> ".tex")
         case (parseLaTeX xs) of
            Left theerror -> do
               putStrLn ("Failure on " <> folderPathRender docaddress)
               print theerror
               return Nothing
            Right doc -> Just <$> parseSuccessCase doc
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
      (programhead, indexedsechead) = parseSubChapter chapaddress x False
         "Application Unification"
         "Application Unification"
         "A Serialized Online Textbook by ClocksSugars"
      (programtail, indexedsectail) = sectionWorker xs
      theprogram :: RefIndexState -> IO (Maybe RefIndexState)
      theprogram refstate = do
         mayberefs <- programhead refstate
         case mayberefs of
            Nothing -> do {putStrLn "Exiting chapter"; return Nothing}
            Just refout -> do {programtail refout}
      in (
         theprogram, --- We want to be able to make webpages without necessarily documenting them here
         if "DoNotShowOnIndex" `elem` x.flags then indexedsectail else indexedsechead:indexedsectail
      )
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

parseArticles :: AllMyArticles ->
   (RefIndexState -> IO (Maybe RefIndexState), AllMyArticlesIndex)
parseArticles (AllMyArticles thearticles) = let
   address = ["articles"]
   sectionWorker :: [SubChapter] ->
      (RefIndexState -> IO (Maybe RefIndexState), [IndexedSection])
   sectionWorker [] = (\ x -> do {return (Just x)}, [])
   sectionWorker (x:xs) = let
      (programhead, indexedarthead) = parseSubChapter address x True
         "ClocksSugars' Blog"
         "ClocksSugars' Blog"
         "My Blog and The home of Application Unification"
      (programtail, indexedarttail) = sectionWorker xs
      theprogram :: RefIndexState -> IO (Maybe RefIndexState)
      theprogram refstate = do
         mayberefs <- programhead refstate
         case mayberefs of
            Nothing -> do {putStrLn "Exiting chapter"; return Nothing}
            Just refout -> do {programtail refout}
      in (theprogram , indexedarthead:indexedarttail)
   (endprogram, listofindexchapters) = sectionWorker thearticles
   in (endprogram, AllMyArticlesIndex listofindexchapters)

parsePreface :: RefIndexState -> IO (Maybe RefIndexState)
parsePreface refinds = let
   resetAllButReferences = resetNoneMapInd refinds
   parseSuccessCase :: LaTeX -> IO RefIndexState
   parseSuccessCase doc = do
      let (thepagehtml,newrefs,logs) = writePage
            "preface"
            (folderPathRender ["preface","appliuni"])
            []
            (extractDocument doc)
            resetAllButReferences
      let thepage = renderHtml $ defaultPageHTML $ PageConstructInfo
            "../styles.css"
            "Application Unification"
            "Application Unification"
            "A Serialized Online Textbook by ClocksSugars"
            "Preface"
            (addressListHtml ["preface","appliuni"])
            []
            thepagehtml
      writeFileMakePath ["preface","appliuni","public"] ".html" thepage
      writeFileMakePath ["preface","appliuni","logs"] "0.txt" (logs !! 0)
      writeFile "logs/appliuni/preface1.txt" (logs !! 1)
      writeFile "logs/appliuni/preface2.txt" (logs !! 2)
      putStrLn "Success on preface"
      return newrefs
   in do
      xs <- readFileTex ("latexraw/appliuni/appliuni_head.tex")
      case (parseLaTeX xs) of
         Left theerror -> do
            putStrLn ("Failure on preface")
            print theerror
            return Nothing
         Right doc -> Just <$> parseSuccessCase doc

parseTail :: IO (Maybe Html)
parseTail = let
   parseSuccessCase :: LaTeX -> IO Html
   parseSuccessCase doc = do
      let (thepagehtml,newrefs,logs) = processPage
            "preface"
            ""
            (extractDocument doc)
            blankIndex
      -- writeFileMakePath ["preface","appliuni","public"] ".html" thepage
      writeFileMakePath ["tail","appliuni","logs"] "0.txt" (logs !! 0)
      writeFile "logs/appliuni/tail1.txt" (logs !! 1)
      writeFile "logs/appliuni/tail2.txt" (logs !! 2)
      putStrLn "Success on preface"
      return thepagehtml
   in do
      xs <- readFileTex ("latexraw/appliuni/appliuni_tail.tex")
      case (parseLaTeX xs) of
         Left theerror -> do
            putStrLn ("Failure on preface")
            print theerror
            return Nothing
         Right doc -> Just <$> parseSuccessCase doc
