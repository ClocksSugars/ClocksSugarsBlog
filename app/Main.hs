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
      H.section ! class_ "pagebound" $ do
         H.div ! A.id "barone" ! class_ "bar-one" $ do
            header ! class_ "flex-col margin15" $ do
               h1 ! class_ "marginsmall" $ "ClocksSugars' Blog"
               p ! class_ "marginless" $ "The home of 'Application Unification'"
         H.div ! A.id "bartwo" ! class_ "bar-two" $ do
            p ! class_ "marginless" $ "Home"
         p "some text here"
         img ! src "assets/diagram.svg" ! class_ "squareheight"
