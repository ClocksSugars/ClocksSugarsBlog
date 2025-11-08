{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO
import Data.String
import Control.Monad.Trans.Except

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String as R
import Text.Blaze.Internal (MarkupM(Empty))
import System.Directory

import LatexToHtml.MainTools
import LatexToHtml.ProcessingTypes
import LatexToHtml.TreeCleaner (extractDocument, processOne, processTwo)
import LaTeXUtils
import Text.LaTeX.Base.Parser
import Data.Either.Utils

main :: IO ()
main = do
   handle <- openFile "latexraw/anatomyRn/philofmath.tex" ReadMode
   xs <- hGetContents handle
   let doc = extractDocument . fromRight . parseLaTeX . fromString $ xs
   writeFile "inspect0.txt" $ show doc
   let part1 = processOne doc
   writeFile "inspect1.txt" $ show part1
   let part2 = processTwo part1
   writeFile "inspect2.txt" $ show part2
   let part3 = processThree part2
   writeFile "test.html" $ R.renderHtml . pageHTML $ part3
   hClose handle

-- main :: IO ()
-- main = do
--    handle <- openFile "latexraw/anatomyRn/philofmath.tex" ReadMode
--    xs <- hGetContents handle
--    writeFile "parsetreeexp3.txt" $ myShow . extractDocument . fromRight . parseLaTeX . fromString $ xs
--    hClose handle


innerErr :: (Show a) => Either a b -> (String -> FailureMode) -> Either FailureMode b
innerErr exs kind = case exs of
   Left err -> Left . kind . show $ err
   Right xs -> Right xs

-- outerErr :: ExceptT FailureMode IO () -> IO ()
-- outerErr eio = case eio of
--    Right _ -> return ()
--    Left err -> print err

-- obtainFile :: String -> IO (Either FailureMode String)
-- obtainFile filename = do
--    return (if
--       doesFileExist filename
--       then Right $
--          withFile filename ReadMode hGetContents
--       else Left $ NoSuchFile filename
--          )

data FailureMode =
      NoSuchFile String
   |  InvalidLatex String

--main = writeFile "ExamplePage.html" $ R.renderHtml pageHTML

katexArgs :: Html
katexArgs = "const katexargs = { delimiters: ["
   <> "{left: \"$$\", right: \"$$\", display: true},"
   <> "{left: \"$\", right: \"$\", display: false},"
   -- <> "{left: \"\\(\", right: \"\\)\", display: false},"
   <> "{left: \"\\begin{equation}\", right: \"\\end{equation}\", display: true},"
   <> "{left: \"\\begin{align}\", right: \"\\end{align}\", display: true},"
   <> "{left: \"\\begin{alignat}\", right: \"\\end{alignat}\", display: true},"
   <> "{left: \"\\begin{gather}\", right: \"\\end{gather}\", display: true},"
   <> "{left: \"\\begin{CD}\", right: \"\\end{CD}\", display: true},"
   <> "{left: \"\\[\", right: \"\\]\", display: true}"
   <> "], throwOnError : false}"

pageHTML :: Html -> Html
pageHTML pageContent = docTypeHtml $ do
   H.head $ do
      meta ! charset "utf-8"
      meta ! name "viewport" ! content "width=device-width"
      link ! rel "stylesheet" ! href "https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/katex.min.css"
      link ! rel "stylesheet" ! href "styles.css"
      script ! defer "" ! src "https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/katex.min.js" $ Empty ()
      script katexArgs
      script ! defer "" ! src "https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/contrib/auto-render.min.js"
         ! onload "renderMathInElement(document.body, katexargs);" $ Empty ()
      H.title "ClocksSugars' Blog"
   body $ do
      H.section ! class_ "pagebound" $ do
         H.div ! A.id "barone" ! class_ "bar-one" $ do
            header ! class_ "flex-col margin15" $ do
               h1 ! class_ "marginsmall" $ "ClocksSugars' Blog"
               p ! class_ "marginless" $ "The home of 'Application Unification'"
         H.div ! A.id "bartwo" ! class_ "bar-two" $ do
            p ! class_ "marginless" $ "Home"
         pageContent
         --img ! src "assets/diagram.svg" ! class_ "squareheight"
