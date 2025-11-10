{-# LANGUAGE OverloadedStrings #-}
module LatexToHtml.MainTools (
   processLatexToHtml,
   processThree,
   IndexState(IndexState),
   references,
   blankIndex
) where

import LatexToHtml.ProcessingTypes
import LatexToHtml.TreeCleaner (processOneTwo)

import Text.Blaze.Internal (MarkupM(Empty))
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Data.ListLike (fromText)
import Data.String
import Data.Map.Strict (Map, empty)
import Data.Text (Text)

import Text.LaTeX.Base (LaTeX)

data IndexState = IndexState
   {  theorems    :: Int
   ,  figures     :: Int
   ,  expressions :: Int -- i.e. eq number
   ,  references :: Map String Text
   }

blankIndex = IndexState {
      theorems    = 1
   ,  figures     = 1
   ,  expressions = 1
   ,  references = empty
   }

processThree :: Text -> [HtmlVers] -> IndexState -> (Html, IndexState)
processThree pagename content indexstate = let
   repeatCase :: [HtmlVers] -> IndexState -> (Html, IndexState)
   repeatCase [] ind = (Empty (), ind)
   repeatCase (x:xs) ind = let
      (resultx, newind) = worker x ind
      (resultxs, finind) = repeatCase xs newind
      in (resultx >> resultxs, finind)
   passFirst :: (Html -> Html) -> (Html, IndexState) -> (Html, IndexState)
   passFirst func (x,y) = (func x, y)
   worker :: HtmlVers -> IndexState -> (Html, IndexState)
   worker stuff propind = case stuff of
      RawText x -> (toHtml x, propind)
      Emphasize x -> (i $ toHtml x, propind)
      Bold x -> (b $ toHtml x, propind)
      Subheading x -> (h3 $ toHtml x, propind)
      ListItem xs -> passFirst li $ repeatCase xs propind
      Itemize xs -> passFirst ul $ repeatCase xs propind
      Enumerate _ xs -> passFirst ol $ repeatCase xs propind -- TODO change ol to (ol ! something) when order instructions available
      Paragraph xs -> passFirst p $ repeatCase xs propind
      Figure location thing -> let
         fignumber = figures propind
         newind = propind { figures = fignumber + 1 }
         htmlelement = do
            H.figure ! class_ "flex-col" $ do
               img ! (src . fromString $ ("latexraw/anatomyRn/" ++ fromText location)) !
                  alt "A visual representation of the description above"
               figcaption . fromString . fromText $ ("Figure " <> (fromString . show $ fignumber) <> ": " <> thing)
         in (htmlelement, newind)
   in case (content,indexstate) of
      ([], ind) -> (Empty (), ind)
      (x:xs, ind) -> repeatCase xs ind

processLatexToHtml :: Text -> LaTeX -> IndexState -> (Html, IndexState)
processLatexToHtml pagename x = processThree pagename $ processOneTwo x
