module LatexToHtml.ReferenceHandling (
   RefIndexState(..),
   doAnInfoBox,
   blankIndex
) where

import Data.Map.Strict (Map, empty, insert, fromList, (!?))

data RefIndexState = RefIndexState
   {  theorems    :: Int
   ,  figures     :: Int
   ,  expressions :: Int -- i.e. eq number
   ,  subsection  :: Int -- Map String (String, (String, Int)) section-id (this can be pagename-number if none is given), title, (pagename, number)
   ,  references  :: Map String (String, String, String) -- pageaddress pagename theorem-number
   }

blankIndex :: RefIndexState
blankIndex = RefIndexState {
      theorems    = 1
   ,  figures     = 1
   ,  expressions = 1
   ,  subsection  = 0 -- it is common to have a section 0 preamble so this must start at zero
   ,  references  = empty
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
