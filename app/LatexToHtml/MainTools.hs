{-# LANGUAGE OverloadedStrings #-}
module LatexToHtml.MainTools (
   -- processLatexToHtml,
   -- processThree,
   RefIndexState(RefIndexState),
   references,
   section,
   subsection,
   resetNoneMapInd,
   blankIndex,
   extractDocument,
   -- processOne,
   -- processTwo,
   Htmllatexinter,
   -- processOneTwo,
   inlineCommands,
   -- HtmlVers,
   writePage,
   subchapterPageHtml,

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
import Data.Map.Strict (Map, empty, insert, fromList, (!?), findWithDefault, toList)
import Data.Text (Text)
import Data.List (sortBy)

import Text.LaTeX.Base (LaTeX)

linknewtab :: Html -> Html
linknewtab = H.a ! target "_blank" ! rel "noopener noreferrer"

writePage :: String -> String -> [String] -> LaTeX -> RefIndexState -> (Html, RefIndexState, [String])
writePage pagename addressliteral pageFlags pagecontents indstate = let
   inspect0 = myShow pagecontents
   part1 = processOne pagecontents
   inspect1 = show part1
   part2 = processTwo part1
   inspect2 = show part2
   (part3, newindstate) = processThree pagename addressliteral part2 indstate
   final :: Html
   final = do
      if "IndexTopOfPage" `elem` pageFlags
         then (pageIndex newindstate pagename addressliteral)
         else Empty ()
      part3
   logs = [inspect0,inspect1,inspect2]
   in (final, newindstate, logs)


processLatexToHtml :: String -> String -> LaTeX -> RefIndexState -> (Html, RefIndexState)
processLatexToHtml pagename pageaddress x = processThree pagename pageaddress $ processOneTwo x

pageIndex :: RefIndexState -> String -> String -> Html
pageIndex refinds ourpagename ourpageaddress = let
   subsections = subsectionMap refinds
   subsubsections = subsubsectionMap refinds
   subsectionList :: [(Html, String, Int)] -- title, id, number (for sorting)
   subsectionList = sortBy (\ (_,_,a) (_,_,b) -> compare a b) [ let
      (title, _, number) = snd thissubsec
      in (
         toHtml (show number <> ". ") >> title,
         fst thissubsec,
         number
      )
      |  thissubsec <- toList subsections,
         (\(_,y,_)->y) (snd thissubsec) == ourpagename
      ]
   withsubsectionList :: [(Html, String, [(Html, String, Int)])]
   withsubsectionList = (`Prelude.map` subsectionList) $ \ (sectitle, secid, secnumber) -> let
      subsubsectionList = sortBy (\ (_,_,a) (_,_,b) -> compare a b) [ let
         (title, _, _, number) = snd thissubsubsec
         in (
            toHtml (show secnumber <> "." <> show number <> " ") >> title,
            fst thissubsubsec,
            number
         )
         |  thissubsubsec <- toList subsubsections,
            (\(_,y,_,_)->y) (snd thissubsubsec) == ourpagename,
            (\(_,_,y,_)->y) (snd thissubsubsec) == secid
         ]
      in (sectitle, secid, subsubsectionList)
   doSubSec :: (Html, String, Int) -> Html
   doSubSec (subsectitle, subsecid, _) =
      li $ H.a ! href (fromString $ ourpageaddress <> "#" <> subsecid) $ subsectitle
   doSec :: (Html, String, [(Html, String, Int)]) -> Html
   doSec (sectitle, secid, itssubsections) = do
      li $ H.a ! href (fromString $ ourpageaddress <> "#" <> secid) $ sectitle
      case itssubsections of
         [] -> Empty ()
         x:xs -> ul $ foldl (>>) (doSubSec x) $ Prelude.map doSubSec xs
   in case withsubsectionList of
      [] -> Empty ()
      x:xs -> do
         h3 "Page Index"
         ul $ foldl (>>) (doSec x) $ Prelude.map doSec xs

inlineProcessThree :: [HtmlVers] -> RefIndexState -> Html
inlineProcessThree [] _ = Empty ()
inlineProcessThree (x:xs) propind = (\y -> y >> (inlineProcessThree xs propind)) $ case x of
   Paragraph zs -> inlineProcessThree zs propind
   RawText x -> toHtml x
   Bold x -> b $ toHtml x
   Emphasize x -> i $ toHtml x
   HLink turl tx -> H.a ! href (fromString $ fromText turl) $ inlineProcessThree tx propind
   ReferenceNum reflabel -> let
      droppedBoxTypeV  = drop 4 reflabel
      thenumber = references propind Data.Map.Strict.!? droppedBoxTypeV
      in case thenumber of
         Just (pageaddress, refpagename, thmnum) -> linknewtab !
            href (fromString $ "/" ++ pageaddress ++ ".html#" ++ droppedBoxTypeV) $
               toHtml $ refpagename ++ "." ++ thmnum
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
      TTtext x -> (code $ toHtml x, propind)
      HLink turl tx -> (H.a ! href (fromString $ fromText turl) $ inlineProcessThree tx propind, propind)
      ListItem xs -> passFirst li $ repeatCase xs propind
      Itemize xs -> passFirst ul $ repeatCase xs propind
      Enumerate listkind xs -> passFirst (ol ! A.type_ (fromString $ fromText listkind)) $ repeatCase xs propind
      -- TODO MAKE THE OL TYPE MORE ROBUST
      Paragraph xs -> passFirst p $ repeatCase xs propind
      CodeBlock language thecode -> (
         pre $ code ! class_ (fromString language) $ toHtml thecode,
         propind
         )
      JsEmbed thefile -> (do
         H.div ! A.id (fromString thefile <> "_give_content") $ Empty ()
         script ! A.type_ "module" ! src (fromString $ "./" ++ thefile ++ ".js") ! defer "" $ Empty()
         ,
         propind
         )

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
            Just (pageaddress, refpagename, thmnum) -> (, propind) $ linknewtab !
               href (fromString $ "/" ++ pageaddress ++ ".html#" ++ droppedBoxTypeV) $
                  toHtml $ refpagename ++ "." ++ thmnum
            Nothing -> ("REFERENCE-ERROR", propind)

      Subheading mlabel x -> let
         secnumber = (1 +) $ (\ (y, _, _) -> y) $ subsection propind
         newlabel = case mlabel of
            Just label -> label
            Nothing -> pagename <> "_" <> show secnumber
         processedtitle = inlineProcessThree x propind
         newind = propind { subsection = (
            secnumber,
            insert newlabel (processedtitle, pagename, secnumber) (subsectionMap propind),
            newlabel
         )
         }
         in (h3 ! A.id (fromString newlabel) $ ((>>) (toHtml $ show secnumber <> ". ") processedtitle) , newind)

      Subsubheading mlabel x -> let
         oversecnumber = (\(y,_,_)->y) $ subsection propind
         secnumber = (1 +) $ fst $ subsubsection propind
         newlabel = case mlabel of
            Just label -> label
            Nothing -> pagename <> "_" <> show oversecnumber <> "_" <> show secnumber
         processedtitle = inlineProcessThree x propind
         secitsunder = (\(_,_,y)->y) $ subsection propind
         itsundernumber = (\(_,_,y)->y) $ findWithDefault (Empty (),"",-1) secitsunder (subsectionMap propind)
         newind = propind { subsubsection = (
            secnumber,
            insert newlabel (processedtitle, pagename, secitsunder, secnumber) (snd $ subsubsection propind)
         )
         }
         in (h4 ! A.id (fromString newlabel) $ ((>>) (toHtml $ show itsundernumber <> "." <> show secnumber <> " ") processedtitle) , newind)

      -- Subsubheading x -> let
      --    processedtitle = inlineProcessThree x propind
      --    in (h4 processedtitle , propind)

      Figure location thing -> let
         fignumber = figures propind
         newind = propind { figures = fignumber + 1 }
         htmlelement = do
            H.figure ! class_ "flex-col" $ do
               img ! (src . fromString $ (fromText location)) !
                  alt "A visual representation of the description above"
               figcaption $ (>>)
                  (fromString $ "Figure " <> show fignumber <> ": ")
                  $ inlineProcessThree thing propind
         in (htmlelement, newind)

      BoxedSec infobox mtitle content -> let
         mlabel = getMaybeLabelInfoBox infobox
         updateTheoremCounter = shouldUpdateThmCounter infobox
         (newind1, thmnum) = doAnInfoBox updateTheoremCounter mlabel theaddress pagename propind
         (processedContent, newind2) = processThree pagename theaddress content newind1
         pmtitle = case mtitle of
            Just ttitle -> Just $ inlineProcessThree ttitle propind
            Nothing -> Nothing
         (htmlelement, _) = boxToHtml infobox pmtitle pagename thmnum processedContent
         in (htmlelement, newind2)

   in case (content,indexstate) of
      ([], ind) -> (Empty (), ind)
      (xs, ind) -> repeatCase xs ind
