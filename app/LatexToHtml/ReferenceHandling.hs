{-# LANGUAGE OverloadedRecordDot #-}
module LatexToHtml.ReferenceHandling (
   RefIndexState(..),
   doAnInfoBox,
   blankIndex,
   subsectionMap,
   subsubsectionMap,
   resetNoneMapInd
) where

import Data.Map.Strict (Map, empty, insert, fromList, (!?))
import Text.Blaze.Html5 (Html)

data RefIndexState = RefIndexState
   {  theorems    :: Int
   ,  figures     :: Int
   ,  expressions :: Int -- i.e. eq number
   ,  subsection  :: (Int, Map String (Html, String, Int), String) -- CurrentSection, Map between ID/label (this can be pagename_num by default) and title, pagename, number it occurs at, then current section label
   ,  subsubsection  :: (Int, Map String (Html, String, String, Int)) -- CurrentSubsection, map between ID/label, title, page name, section-id it's under, subsection number
   ,  references  :: Map String (String, String, String) -- pageaddress pagename theorem-number
   }

blankIndex :: RefIndexState
blankIndex = RefIndexState {
      theorems    = 1
   ,  figures     = 1
   ,  expressions = 1
   ,  subsection  = (0, empty, "FakeSubSec") -- it is common to have a section 0 preamble so this must start at zero
   ,  subsubsection = (0, empty)
   ,  references  = empty
   }

subsectionMap :: RefIndexState -> Map String (Html, String, Int)
subsectionMap propind = (\(_,y,_)->y) $ subsection propind

subsubsectionMap :: RefIndexState -> Map String (Html, String, String, Int)
subsubsectionMap propind = snd $ subsubsection propind

resetNoneMapInd :: RefIndexState -> RefIndexState
resetNoneMapInd refind = blankIndex {
   subsection = (0, subsectionMap refind, "FakeSubSec"),
   subsubsection = (0, subsubsectionMap refind),
   references = refind.references
}

doAnInfoBox :: Bool  -- avoid updating theorem counter ?
   -> Maybe String   -- maybe label
   -> String         -- page address for linking
   -> String         -- pagename for outputting ref name
   -> RefIndexState  -- the counters and references
   -> (RefIndexState, Int)
doAnInfoBox updateTheoremCounter mlabel address pagename propind = case mlabel of
   Nothing -> let
      oldthmnum = theorems propind
      ind = propind {theorems = oldthmnum + 1}
      in if updateTheoremCounter
         then (ind, oldthmnum)
         else (propind, oldthmnum)
   Just label -> let
      oldthmnum = theorems propind
      ind = propind {
         theorems = oldthmnum + 1,
         references = insert label (address, pagename, show oldthmnum) $ references propind
         }
      in (ind, oldthmnum)
