{-# LANGUAGE OverloadedStrings #-}
module LatexToHtml.TikzToSvg where

import LatexToHtml.ProcessingTypes
import Pantry.SHA256 (toHexText, hashBytes)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient,encodeUtf8)

import SiteStructure.AddressManagement
import Data.ListLike (fromText)
import System.Process (callCommand)
-- import System.Directory (copyFile)

makeTikzStandalone :: Text -> String -> (String , IO ())
makeTikzStandalone content theaddress = let
   filename :: String
   filename =  fromText . toHexText . hashBytes . encodeUtf8 $ content
   finalTop = "\\documentclass[tikz,convert={outfile=\\jobname.svg}]{standalone}\n\\input{../latexraw/symbolmacros.tex}\n\\usepackage{tikz-cd}\\begin{document}\n"
   final = fromText $ finalTop <> content <> "\n\\end{document}"
   in (filename, ) $ do
      writeFileMakePath ([filename, "latexvomit"]) ".tex" final
      callCommand $ "cd ./latexvomit && pdflatex -quiet " <> filename <> ".tex &&"
         <> " pdf2svg " <> filename <> ".pdf " <> filename <> ".svg &&"
         <> " cp " <> filename <> ".svg ../public/" <> theaddress <> "/"
