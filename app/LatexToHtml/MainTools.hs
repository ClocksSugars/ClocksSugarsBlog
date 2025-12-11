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
   writePage,
   subchapterPageHtml
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
import LatexToHtml.ReferenceHandling
import LatexToHtml.InfoBoxType

import Text.Blaze.Internal (MarkupM(Empty))
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.ListLike (fromText)
import Data.String
import Data.List (intercalate, drop)
--import qualified Data.Map.Strict as Maps ((!))
import Data.Map.Strict (Map, empty, insert, fromList, (!?))
import Data.Text (Text)

import Text.LaTeX.Base (LaTeX)

linknewtab :: Html -> Html
linknewtab = H.a ! target "_blank" ! rel "noopener noreferrer"

writePage :: Text -> String -> Html -> String -> LaTeX -> RefIndexState -> (String, RefIndexState, [String])
writePage pagetitle pagename addressline addressliteral pagecontents indstate = let
   inspect0 = myShow pagecontents
   part1 = processOne pagecontents
   inspect1 = show part1
   part2 = processTwo part1
   inspect2 = show part2
   (part3, newindstate) = processThree pagename addressliteral part2 indstate
   logs = [inspect0,inspect1,inspect2]
   thepage = subchapterPageHtml pagetitle addressline part3
   in (renderHtml thepage, newindstate, logs)


processLatexToHtml :: String -> String -> LaTeX -> RefIndexState -> (Html, RefIndexState)
processLatexToHtml pagename pageaddress x = processThree pagename pageaddress $ processOneTwo x


-- BoxedSec String (Maybe Text) (Maybe [HtmlVers]) [HtmlVers]

inlineProcessThree :: [HtmlVers] -> RefIndexState -> Html
inlineProcessThree [] _ = Empty ()
inlineProcessThree (x:xs) propind = (\y -> y >> (inlineProcessThree xs propind)) $ case x of
   RawText x -> toHtml x
   Bold x -> b $ toHtml x
   Emphasize x -> i $ toHtml x
   HLink turl tx -> H.a ! href (fromString $ fromText turl) $ inlineProcessThree tx propind
   ReferenceNum reflabel -> let
      droppedBoxTypeV  = drop 4 reflabel
      thenumber = references propind Data.Map.Strict.!? droppedBoxTypeV
      in case thenumber of
         Just (pageaddress, pagename, thmnum) -> linknewtab !
            href (fromString $ "/" ++ pageaddress ++ ".html#" ++ droppedBoxTypeV) $
               toHtml $ pagename ++ "." ++ thmnum
         _ -> "REFERENCE-ERROR"
   _ -> "WAS TOLD TO INLINE SOMETHING THAT I CANNOT INLINE"

processThree :: String -> String -> [HtmlVers] -> RefIndexState -> (Html, RefIndexState)
processThree pagename theaddress content indexstate = let
   repeatCase :: [HtmlVers] -> RefIndexState -> (Html, RefIndexState)
   repeatCase [] ind = (Empty (), ind)
   repeatCase (x:xs) ind = let
      (resultx, newind) = worker x ind
      (resultxs, finind) = repeatCase xs newind
      in (resultx >> resultxs, finind)
   passFirst :: (Html -> Html) -> (Html, RefIndexState) -> (Html, RefIndexState)
   passFirst func (x,y) = (func x, y)
   worker :: HtmlVers -> RefIndexState -> (Html, RefIndexState)
   worker stuff propind = case stuff of
      RawText x -> (toHtml x, propind)
      Emphasize x -> (i $ toHtml x, propind)
      Bold x -> (b $ toHtml x, propind)
      HLink turl tx -> (H.a ! href (fromString $ fromText turl) $ inlineProcessThree tx propind, propind)
      ListItem xs -> passFirst li $ repeatCase xs propind
      Itemize xs -> passFirst ul $ repeatCase xs propind
      Enumerate listkind xs -> passFirst (ol ! A.type_ (fromString $ fromText listkind)) $ repeatCase xs propind
      -- TODO MAKE THE OL TYPE MORE ROBUST
      Paragraph xs -> passFirst p $ repeatCase xs propind

      HRefLink reflabel tx -> let
         droppedBoxTypeV  = drop 4 reflabel
         theref = references propind Data.Map.Strict.!? droppedBoxTypeV
         in case theref of
            Just (pageaddress, _, _) -> (, propind) $ linknewtab !
               href (fromString $ "/" ++ pageaddress ++ ".html#" ++ droppedBoxTypeV) $
                  inlineProcessThree tx propind
            Nothing -> ("REFERENCE-ERROR", propind)

      ReferenceNum reflabel -> let
         droppedBoxTypeV  = drop 4 reflabel
         theref = references propind Data.Map.Strict.!? droppedBoxTypeV
         in case theref of
            Just (pageaddress, _, thmnum) -> (, propind) $ linknewtab !
               href (fromString $ "/" ++ pageaddress ++ ".html#" ++ droppedBoxTypeV) $
                  toHtml $ pagename ++ "." ++ thmnum
            Nothing -> ("REFERENCE-ERROR", propind)

      Subheading x -> let
         newind = propind { subsection = subsection propind + 1 }
         secnumber = subsection newind
         processedtitle = inlineProcessThree x newind
         in (h3 ((>>) (toHtml $ show secnumber <> ". ") processedtitle) , newind)

      Figure location thing -> let
         fignumber = figures propind
         newind = propind { figures = fignumber + 1 }
         htmlelement = do
            H.figure ! class_ "flex-col" $ do
               img ! (src . fromString $ (fromText location)) !
                  alt "A visual representation of the description above"
               figcaption . fromString . fromText $ ("Figure " <> (fromString . show $ fignumber) <> ": " <> thing)
         in (htmlelement, newind)

      BoxedSec infobox mtitle content -> let
         mlabel = getMaybeLabelInfoBox infobox
         isProof = shouldUpdateThmCounter infobox
         (newind1, thmnum) = doAnInfoBox isProof mlabel theaddress pagename propind
         (processedContent, newind2) = processThree pagename theaddress content newind1
         pmtitle = case mtitle of
            Just ttitle -> Just $ inlineProcessThree ttitle propind
            Nothing -> Nothing
         (htmlelement, _) = boxToHtml infobox pmtitle pagename thmnum processedContent
         in (htmlelement, newind2)

   in case (content,indexstate) of
      ([], ind) -> (Empty (), ind)
      (xs, ind) -> repeatCase xs ind
