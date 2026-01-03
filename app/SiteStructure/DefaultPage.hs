{-# LANGUAGE OverloadedStrings #-}

module SiteStructure.DefaultPage (
   defaultPageHTML,
   placeholderContent
) where

import Data.Text (Text)
import Data.String

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal (MarkupM(Empty))

katexArgs :: Html
katexArgs = "const katexargs = { delimiters: ["
   <> "{left: \"$$\", right: \"$$\", display: true},"
   <> "{left: \"$\", right: \"$\", display: false},"
   -- <> "{left: \"\\(\", right: \"\\)\", display: false},"
   <> "{left: \"\\begin{equation}\", right: \"\\end{equation}\", display: true},"
   <> "{left: \"\\begin{align}\", right: \"\\end{align}\", display: true},"
   <> "{left: \"\\begin{align*}\", right: \"\\end{align*}\", display: true},"
   <> "{left: \"\\begin{alignat}\", right: \"\\end{alignat}\", display: true},"
   <> "{left: \"\\begin{gather}\", right: \"\\end{gather}\", display: true},"
   <> "{left: \"\\begin{gathered}\", right: \"\\end{gathered}\", display: true},"
   <> "{left: \"\\begin{CD}\", right: \"\\end{CD}\", display: true},"
   -- <> "{left: \"\\[\", right: \"\\]\", display: true}"
   <> "], throwOnError : false}"

defaultPageHTML :: String -> String -> Text -> Text -> Text -> Html -> Html -> Html
defaultPageHTML
   whereCSS
   pagetitle
   pageh1
   undertitletext
   pageh2
   pageaddresslinks
   pageContent
   = docTypeHtml $ do
   H.head $ do
      meta ! charset "utf-8"
      meta ! name "viewport" ! content "width=device-width"
      link ! rel "stylesheet" ! href "https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/katex.min.css"
      link ! rel "stylesheet" ! href (fromString whereCSS)

      link ! rel "stylesheet" ! href "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/styles/default.min.css"

      script ! defer "" ! src "https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/katex.min.js" $ Empty ()
      script katexArgs

      script ! src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/highlight.min.js" $ Empty ()
      script ! src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/languages/rust.min.js" $ Empty ()
      script ! src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/languages/haskell.min.js" $ Empty ()

      script ! defer "" ! src "https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/contrib/auto-render.min.js"
         ! (onload $
               "renderMathInElement(document.body, katexargs);"
            <> "hljs.highlightAll();"
         ) $ Empty ()

      H.title $ toHtml pagetitle
   body $ do
      H.section ! class_ "pagebound" $ do
         H.div ! A.id "barone" ! class_ "bar-one" $ do
            header ! class_ "flex-col margin15" $ do
               h1 ! class_ "marginsmall" $ toHtml pageh1
               p ! class_ "marginless" $ toHtml undertitletext
         H.div ! A.id "bartwo" ! class_ "bar-two" $ do
            p ! class_ "marginless" $ pageaddresslinks
         h2 . toHtml $ pageh2
         pageContent


placeholderContent :: Html
placeholderContent = do
   p "If you're seeing this than you've found a placeholder page. At some point this will probably be turned into some list of info about the content on this site."
