{-# LANGUAGE OverloadedStrings #-}
module LatexToPdf.MainTools (pdfBookFromManifest) where

import System.IO
import Data.String (fromString)
import Text.LaTeX.Base.Syntax (LaTeX(..),TeXArg(..))
import Text.LaTeX.Base.Parser (parseLaTeX)
import Text.LaTeX.Base.Render (render)
import Data.ListLike (fromText)
import System.Directory (createDirectoryIfMissing)

import SiteStructure.RecordTypes (WrittenWorkBook)
import SiteStructure.AddressManagement

import LatexToPdf.WorkHorse

pdfBookFromManifest :: WrittenWorkBook -> IO ()
pdfBookFromManifest workbook = let
   demaybe :: (LaTeX -> IO ()) -> IO ()
   demaybe x = do
      createDirectoryIfMissing False "latexvomit"
      mcontents <- parseBook workbook
      case mcontents of
         Nothing -> do {putStrLn "Ended in Failure"}
         Just contents -> x contents
   in demaybe $ \contents -> do
      writeFile "latexvomit/appliunibook.tex" $ fromText . render $
         (TeXComm "documentclass" [OptArg $ TeXRaw "11pt", FixArg $ TeXRaw "book"])
            <> (TeXComm "author" [FixArg $ TeXRaw "clockssugars.blog/appliuni"])
            <> (TeXComm "title" [FixArg $ TeXComm "textbf" [FixArg $ TeXRaw "Application Unification"]])
            <> (TeXComm "date" [FixArg $
               (TeXCommS "the") <> (TeXCommS "month") <> (TeXRaw "/")
                  <> (TeXCommS "the") <> (TeXCommS "day") <> (TeXRaw "/")
                  <> (TeXCommS "the") <> (TeXCommS "year")
               ])
            <> (TeXComm "input" [FixArg $ TeXRaw "../latexraw/appliuni/pdf.tex"])
            <> (TeXComm "input" [FixArg $ TeXRaw "../latexraw/appliuni/pdf_only.tex"])
            <> (TeXComm "input" [FixArg $ TeXRaw "../latexraw/appliuni/webmacros.tex"])
            <> (TeXComm "input" [FixArg $ TeXRaw "../latexraw/appliuni/amsthmstuff.tex"])
            <> contents
      putStrLn "tex file written successfully"
      return ()
