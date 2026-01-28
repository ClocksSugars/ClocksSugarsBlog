{-# LANGUAGE OverloadedStrings #-}

module SiteStructure.DefaultPage (
   defaultPageHTML,
   placeholderContent,
   writeHomePage
) where

import Data.Text (Text)
import Data.String

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal (MarkupM(Empty))

import SiteStructure.RecordTypes
import Text.Blaze.Html.Renderer.Pretty (renderHtml)

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

defaultPageHTML :: PageConstructInfo -> Html
defaultPageHTML PageConstructInfo {
   whereCSS = whereCSS,
   pagetitle = pagetitle,
   pageh1 = pageh1,
   undertitletext = undertitletext,
   pageh2 = pageh2,
   pageaddresslinks = pageaddresslinks,
   pageflags = pageflags,
   pageContent = pageContent
   } = docTypeHtml $ do
   H.head $ do
      meta ! charset "utf-8"
      meta ! A.name "viewport" ! content "width=device-width"
      link ! rel "stylesheet" ! href "https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/katex.min.css"
      link ! rel "stylesheet" ! href (fromString whereCSS)

      link ! rel "stylesheet" ! href "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/styles/default.min.css"

      script ! defer "" ! src "https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/katex.min.js" $ Empty ()
      script katexArgs

      if "LoadHighlightJs" `elem` pageflags then do
         script ! src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/highlight.min.js" $ Empty ()
         script ! src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/languages/rust.min.js" $ Empty ()
         script ! src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/languages/haskell.min.js" $ Empty ()
      else Empty ()

      script ! defer "" ! src "https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/contrib/auto-render.min.js"
         ! (onload $
               "renderMathInElement(document.body, katexargs);"
            <> if "LoadHighlightJs" `elem` pageflags then "hljs.highlightAll();" else ""
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

writeHomePage :: IO ()
writeHomePage = writeFile "public/index.html" $ renderHtml homePageHtml

homePageHtml :: Html
homePageHtml = docTypeHtml $ do
   H.head $ do
      meta ! charset "utf-8"
      meta ! A.name "viewport" ! content "width=device-width"
      link ! rel "stylesheet" ! href "https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/katex.min.css"
      link ! rel "stylesheet" ! href "./styles.css"

      link ! rel "stylesheet" ! href "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/styles/default.min.css"

      script ! defer "" ! src "https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/katex.min.js" $ Empty ()
      script katexArgs

      script ! defer "" ! src "https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/contrib/auto-render.min.js"
         ! (onload "renderMathInElement(document.body, katexargs);") $ Empty ()

      H.title $ "ClocksSugars' Blog"
   body $ do
      H.section ! class_ "pagebound" $ do
         H.div ! A.id "barone" ! class_ "bar-one" $ do
            header ! class_ "flex-col margin15" $ do
               h1 ! class_ "marginsmall" $ "ClocksSugars' Blog"
               p ! class_ "marginless" $ "My Blog and The home of Application Unification"
         H.div ! A.id "bartwo" ! class_ "bar-two" $ do
            p ! class_ "marginless" $ "Home"
         h2 "Learn Math. Do it."
         p $ do
            "Welcome to my site! I am a mathematician, pure math by training and applied math by aspiration,"
            " and documenting what I learn on this site as I try to become an applied mathematician. "
            "Here you will find my blog articles and "
            H.i "Application Unification"
            ", a serialized online math textbook written for anyone who can think a little about what they're reading, "
            "building the intuitions required to speedrun such a person to graduate mathematics"
            ". If you have any questions or criticisms about my writing, shoot me a message or @ me "
            H.a ! href "https://x.com/clocksSugars" $ "on twitter"
            ". "
         p $ do
            "My interests outside of mathematics include aspects of computer science, particularly programming language theory and type theory,"
            " as well as physics (my other major) where my expertise is largely around theoretical and quantum physics "
            "(presently nursing an interest in optics and control theory) and a cautious development of interest in philosophy. As such, you can"
            " expect these interests to show up in or influence my writing. Additionally you can find a forecast of mathematical topics I plan to"
            " write about in "
            H.i "Application Unification"
            " on its "
            H.a ! href "https://clockssugars.blog/appliuni/" $ "index page"
            ", and the history of this website and its edits "
            H.a ! href "https://github.com/ClocksSugars/ClocksSugarsBlog" $ "on the website's github"
            " commit history."
         H.nav ! A.class_ "flex-row" ! A.style "gap: 0.625%" $ do
            H.a ! A.class_ "bigbutton" ! href "./articles" $ "To Articles Page"
            H.a ! A.class_ "bigbutton" ! href "./appliuni" $ do
               "To "
               H.i "Application Unification"
         p $ do
            "This website is ostensibly a set of $\\LaTeX$ documents together with a set of rules to convert it to HTML. Consequently, the PDF version of "
            H.i "Application Unification"
            " can be found "
            H.a ! href "./appliunibook.pdf" $ "here"
            ". Make sure to check the date on the PDF as it will not be updated as often as this site; feel free to simply ask me to update it otherwise."
