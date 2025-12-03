{-# LANGUAGE DuplicateRecordFields #-}
module SiteStructure.MainTools where

import System.IO
import Data.Either.Utils

import SiteStructure.IndexPage (IndexedSection(IndexedSection))
import SiteStructure.Manifest (SubChapter(..))
import LatexToHtml.MainTools

import Text.LaTeX.Base (LaTeX)
import Text.LaTeX.Base.Parser (parseLaTeX)

--- Here is the way this is intended to work:
--    The address argument (where present) is intended
--    to act as a location that is relative both for the input files
--    (the latexraw files) and the output files (public). There should
--    also be an output target for logs.

-- writePage :: String -> String -> LaTeX -> IndexState -> (Html, IndexState, [String])
-- writePage pagetitle pagename pagecontents indstate = let

parseSubChapter ::
   String ->
   SubChapter ->
   (
      RefIndexState -> IO (Maybe RefIndexState),
      IndexedSection
   )
parseSubChapter address subchapter = let
   docaddress = address <> (name subchapter)
   theindex = IndexedSection {
      address = "public/" <> docaddress <> ".html",
      title = title subchapter,
      description = description subchapter
   }
   theprogram refinds = let
      parseSuccessCase :: LaTeX -> IO RefIndexState
      parseSuccessCase doc = do
         let (thepage,newrefs,logs) = writePage
               $  title subchapter
               $  name subchapter
               $  docaddress
               $  extractDocument doc
               $  refinds
         writeFile ("public/" <> docaddress <> ".html") thepage
         writeFile ("logs/" <> docaddress <> "0.txt") (logs !! 0)
         writeFile ("logs/" <> docaddress <> "1.txt") (logs !! 1)
         writeFile ("logs/" <> docaddress <> "2.txt") (logs !! 2)
         return newrefs
      in do
      handle <- openFile ("latexraw/" <> docaddress <> ".tex")
      xs <- hGetContents handle
      outc <- case (parseLatex . fromString $ xs) of
         Left _ -> do return Nothing
         Right doc -> lift Just parseSuccessCase
      hClose handle
      outc
   in (theprogram, theindex)




-- parseAppliUni :: WrittenWorkBook -> IO
-- parseAppliUni = _
