{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO
import Data.String
import Control.Monad.Trans.Except

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String as R
import System.Directory

import Latexcombinators
import LaTeXUtils
import Text.LaTeX.Base.Parser
import Data.Either.Utils

main :: IO ()
main = do
   handle <- openFile "latexraw/anatomyRn/philofmath.tex" ReadMode
   xs <- hGetContents handle
   writeFile "parsetreeexp3.txt" $ myShow . extractDocument . fromRight . parseLaTeX . fromString $ xs
   hClose handle
   --print $ fromRight . parseLaTeX . fromString $ xs
-- main = outerErr $ do
--    let filename = "latexraw/anatomyRn/philofmath.tex"
--    filetext <- obtainFile filename
--    parseTree <- innerErr (parseLaTeX filetext) InvalidLatex
--    return ()

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
