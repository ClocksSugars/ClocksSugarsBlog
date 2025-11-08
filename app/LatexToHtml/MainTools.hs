{-# LANGUAGE OverloadedStrings #-}
module LatexToHtml.MainTools (
   processLatexToHtml,
   processThree
) where

import LatexToHtml.ProcessingTypes
import LatexToHtml.TreeCleaner (processOneTwo)

import Text.Blaze.Internal (MarkupM(Empty))
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Text.LaTeX.Base (LaTeX)

-- data HtmlVers =
--       RawText Text
--    |  Paragraph [HtmlVers]
--    |  Italicisize Text
--    |  Bold Text
--    |  Itemize [HtmlVers]
--    |  Enumerate Text [HtmlVers]
--    |  ListItem [HtmlVers]
--    |  Subheading Text -- <h2>
-- --   |  Env Text [HtmlVers]

processThree :: [HtmlVers] -> Html
processThree content = let
   repeatCase :: (Html -> Html) -> [HtmlVers] -> Html
   repeatCase func [] = func $ Empty ()
   repeatCase func (x:xs) = func $ foldl (>>) (worker x) (Prelude.map worker xs)
   worker :: HtmlVers -> Html
   worker stuff = case stuff of
      RawText x -> toHtml x
      Italicisize x -> i $ toHtml x
      Bold x -> b $ toHtml x
      Subheading x -> h2 $ toHtml x
      ListItem xs -> repeatCase li xs
      Itemize xs -> repeatCase ul xs
      Enumerate _ xs -> repeatCase ol xs -- TODO change ol to (ol ! something) when order instructions available
      Paragraph xs -> repeatCase p xs
   in case content of
      [] -> Empty ()
      x:xs -> foldl (>>) (worker x) (Prelude.map worker xs)

processLatexToHtml :: LaTeX -> Html
processLatexToHtml = processThree . processOneTwo
