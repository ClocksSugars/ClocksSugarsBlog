{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String as R

main :: IO ()
main = writeFile "ExamplePage.html" $ R.renderHtml pageHTML

pageHTML :: Html
pageHTML = docTypeHtml $ do
   H.head $ do
      meta ! charset "utf-8"
      meta ! name "viewport" ! content "width=device-width"
      link ! rel "stylesheet" ! href "styles.css"
      H.title "ClocksSugars' Blog"
   body $ do
      H.div ! A.id "bar-one" ! class_ "barone" $ do
         header ! class_ "flex-col" $ do
            h1 "ClocksSugars' Blog"
            p "The home of 'Application Unification'"
      p "some text here"
