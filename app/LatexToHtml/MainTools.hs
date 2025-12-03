{-# LANGUAGE OverloadedStrings #-}
module LatexToHtml.MainTools (
   -- processLatexToHtml,
   -- processThree,
   RefIndexState(RefIndexState),
   references,
   blankIndex,
   extractDocument,
   -- processOne,
   -- processTwo,
   Htmllatexinter,
   -- processOneTwo,
   inlineCommands,
   -- HtmlVers,
   writePage
) where

import LatexToHtml.ProcessingTypes
import LatexToHtml.TreeCleaner (
   extractDocument,
   processOne,
   processTwo,
   Htmllatexinter,
   processOneTwo,
   inlineCommands
   )
import LatexToHtml.Utils (
   myShow
   )
import LatexToHtml.PageTemplate

import Text.Blaze.Internal (MarkupM(Empty))
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.ListLike (fromText)
import Data.String
import Data.List (intercalate)
--import qualified Data.Map.Strict as Maps ((!))
import Data.Map.Strict (Map, empty, insert, fromList, (!?))
import Data.Text (Text, drop)

import Text.LaTeX.Base (LaTeX)

data RefIndexState = RefIndexState
   {  theorems    :: Int
   ,  figures     :: Int
   ,  expressions :: Int -- i.e. eq number
   ,  subsection  :: Int
   ,  references  :: Map Text [String] -- Need to change this once we reach the multi-doc stage to include link info
   }

blankIndex :: RefIndexState
blankIndex = RefIndexState {
      theorems    = 1
   ,  figures     = 1
   ,  expressions = 1
   ,  subsection  = 0 -- it is common to have a section 0 preamble so this must start at zero
   ,  references  = empty
   }


writePage :: Text -> String -> String -> LaTeX -> RefIndexState -> (String, RefIndexState, [String])
writePage pagetitle pagename pageaddress pagecontents indstate = let
   inspect0 = myShow pagecontents
   part1 = processOne pagecontents
   inspect1 = show part1
   part2 = processTwo part1
   inspect2 = show part2
   (part3, newindstate) = processThree pagename part2 indstate
   logs = [inspect0,inspect1,inspect2]
   thepage = pageHTML pagetitle pageaddress part3
   in (renderHtml thepage, newindstate, logs)


processLatexToHtml :: String -> LaTeX -> RefIndexState -> (Html, RefIndexState)
processLatexToHtml pagename x = processThree pagename $ processOneTwo x


-- BoxedSec String (Maybe Text) (Maybe [HtmlVers]) [HtmlVers]

processThree :: String -> [HtmlVers] -> RefIndexState -> (Html, RefIndexState)
processThree pagename content indexstate = let
   repeatCase :: [HtmlVers] -> RefIndexState -> (Html, RefIndexState)
   repeatCase [] ind = (Empty (), ind)
   repeatCase (x:xs) ind = let
      (resultx, newind) = worker x ind
      (resultxs, finind) = repeatCase xs newind
      in (resultx >> resultxs, finind)
   passFirst :: (Html -> Html) -> (Html, RefIndexState) -> (Html, RefIndexState)
   passFirst func (x,y) = (func x, y)
   boxInfoHandler :: String -> Maybe Text -> RefIndexState -> (Int, RefIndexState)
   boxInfoHandler kind mlabel propind = let
      (newind, thmnum) = case mlabel of
         Nothing -> let
            thmnumt = theorems propind
            ind = propind { theorems = thmnumt + 1 }
            in case kind of
               "Proof" -> (propind, thmnumt)
               _ -> (ind, thmnumt)
         Just label -> let
            thmnumt = theorems propind
            ind = propind {
               theorems = thmnumt + 1,
               references = insert label [pagename, show thmnumt] $ references propind
            }
            in (ind, thmnumt)
      in (thmnum, newind)
   worker :: HtmlVers -> RefIndexState -> (Html, RefIndexState)
   worker stuff propind = case stuff of
      RawText x -> (toHtml x, propind)
      Emphasize x -> (i $ toHtml x, propind)
      Bold x -> (b $ toHtml x, propind)
      ListItem xs -> passFirst li $ repeatCase xs propind
      Itemize xs -> passFirst ul $ repeatCase xs propind
      Enumerate _ xs -> passFirst ol $ repeatCase xs propind -- TODO change ol to (ol ! something) when order instructions available
      Paragraph xs -> passFirst p $ repeatCase xs propind

      ReferenceNum reflabel -> let
         droppedBoxTypeV  = Data.Text.drop 4 reflabel
         thenumber = references propind Data.Map.Strict.!? droppedBoxTypeV
         in case thenumber of
            Just found -> (toHtml $ intercalate "." found, propind)
            Nothing -> ("REFERENCE-ERROR", propind)

      Subheading x -> let
         newind = propind { subsection = subsection propind + 1 }
         secnumber = subsection newind
         (processedtitle, _) = processThree pagename x newind
         in (h3 ((>>) (toHtml $ show secnumber <> ". ") processedtitle) , newind)

      Figure location thing -> let
         fignumber = figures propind
         newind = propind { figures = fignumber + 1 }
         htmlelement = do
            H.figure ! class_ "flex-col" $ do
               img ! (src . fromString $ ("latexraw/anatomyRn/" ++ fromText location)) !
                  alt "A visual representation of the description above"
               figcaption . fromString . fromText $ ("Figure " <> (fromString . show $ fignumber) <> ": " <> thing)
         in (htmlelement, newind)

      BoxedSec kind mlabel mtitle content -> let
         (thmnum, newind1) = boxInfoHandler kind mlabel propind
         (processedContent, newind2) = processThree pagename content newind1
         htmlelement = do
            H.div ! class_ (fromString kind) $ do
               H.div ! class_ (fromString $ kind ++ "title") ! (A.id . fromString . show $ thmnum) $ toHtml $ case (mtitle, kind) of
                  (Just ttitle, "Proof") -> fst $ processThree pagename ttitle propind
                  (Just ttitle, _) -> (>>) (toHtml (kind ++ " " ++ pagename ++ "." ++ show thmnum)) $
                     (H.span ! A.style "padding: 1em" $ "—") >>
                        fst (processThree pagename (RawText "(" : ttitle ++ [RawText ")"]) propind)
                        -- this is SERIOUSLY illustrating that i need an index-neutral [HtmlVers] -> Html function
                  (Nothing, "Proof") -> "Proof."
                  (Nothing, _) -> toHtml(kind ++ " " ++ pagename ++ "." ++ show thmnum)
               processedContent
         in (htmlelement, newind2)

      MProofBox isopen mlabel mtitle content -> let
         (thmnum, newind1) = boxInfoHandler "Proof" mlabel propind
         (processedContent, newind2) = processThree pagename content newind1
         summarybox = H.summary ! class_ (fromString $ "Prooftitle") ! (A.id . fromString . show $ thmnum)
         summaryboxopen = if isopen then summarybox ! A.open "" else summarybox
         htmlelement = do
            H.details ! class_ (fromString "Proof") $ do
               summaryboxopen $ toHtml $ case mtitle of
                  Just ttitle -> fst $ processThree pagename ttitle propind
                  Nothing -> "Proof."
               processedContent
         in (htmlelement, newind2)
   in case (content,indexstate) of
      ([], ind) -> (Empty (), ind)
      (xs, ind) -> repeatCase xs ind
