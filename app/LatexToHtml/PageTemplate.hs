{-# LANGUAGE OverloadedStrings #-}

module LatexToHtml.PageTemplate (
   subchapterPageHtml
) where

import Data.Text (Text)

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal (MarkupM(Empty))

import SiteStructure.DefaultPage

subchapterPageHtml :: Text -> Html -> Html -> Html
subchapterPageHtml pageTitle pageaddress pageContent = defaultPageHTML
      "Application Unification"
      "Application Unification"
      "A Serialized Online Textbook by ClocksSugars"
      pageTitle
      pageaddress
      pageContent

-- docTypeHtml $ do
--    H.head $ do
--       meta ! charset "utf-8"
--       meta ! name "viewport" ! content "width=device-width"
--       link ! rel "stylesheet" ! href "https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/katex.min.css"
--       link ! rel "stylesheet" ! href "styles.css"
--       script ! defer "" ! src "https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/katex.min.js" $ Empty ()
--       script katexArgs
--       script ! defer "" ! src "https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/contrib/auto-render.min.js"
--          ! onload "renderMathInElement(document.body, katexargs);" $ Empty ()
--       H.title "ClocksSugars' Blog"
--    body $ do
--       H.section ! class_ "pagebound" $ do
--          H.div ! A.id "barone" ! class_ "bar-one" $ do
--             header ! class_ "flex-col margin15" $ do
--                h1 ! class_ "marginsmall" $ "ClocksSugars' Blog"
--                p ! class_ "marginless" $ "The home of 'Application Unification'"
--          H.div ! A.id "bartwo" ! class_ "bar-two" $ do
--             p ! class_ "marginless" $ toHtml ("Home/" <> pageaddress)
--          h2 . toHtml $ pageTitle
--          pageContent
