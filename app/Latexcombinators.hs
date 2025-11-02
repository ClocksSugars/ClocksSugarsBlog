{-# LANGUAGE OverloadedStrings #-}
module Latexcombinators where

import Data.String
import Data.Maybe
import Text.LaTeX.Base
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Render
import Control.Monad

-- This all exists under the assumption that
--  the document being processed can produce
--  a pdf. If it passes the LaTeX Syntax
--  requirements then it should work here
--  is what we're aiming for.

type Parser = Parsec () LaTeX

data HtmlVers =
      RawText Text
   |  Paragraph [HtmlVers]
   |  Italicisize Text
   |  Bold Text
   |  Itemize [HtmlVers]
   |  Enumerate Text [HtmlVers]
   |  ListItem [HtmlVers]
   |  Heading Text -- <h1>
   |  Subheading Text -- <h2>
   |  Env Text [HtmlVers]

data HtmlInter =
      Raw ProcessStatus
   |  Paragraph [ProcessStatus]
   |  Itemize [ProcessStatus]
   |  Enumerate Text [ProcessStatus]
   |  ListItem [HtmlInter]
   |  Heading Text -- <h1>
   |  Subheading Text -- <h2>
   |  Env Text [HtmlInter]

data ProcessStatus =
      Unprocessed LaTeX
   |  Processed HtmlVers
   |  Semiprocessed HtmlInter
   |  PSConc ProcessStatus ProcessStatus

instance Semigroup ProcessStatus where
   exs1 <> exs2 = PSConc exs1 exs2

extractDocument :: LaTeX -> LaTeX
extractDocument restOfIt = let
   interiorDoc :: LaTeX -> Maybe LaTeX
   interiorDoc (TeXEnv "document" _ content) = Just content
   interiorDoc (TeXSeq exs1 exs2) = interiorDoc exs1 <|> interiorDoc exs2
   interiorDoc _ = Nothing
   in fromMaybe restOfIt (interiorDoc restOfIt)

 -- Things that should cause the document to be sectioned in order of priority:
 --   - Section Heading
 --   - Environments (itemize and enumerate)
 --   - paragraphs
 --   - Environments (math ones) but these dont actually need to be processed most of the time

partitionSection :: LaTeX -> [HtmlInter]
partitionSection (TeXComm "section" [FixArg (TeXRaw title)] ) = Processed $ Subheading title
partitionSection (TeXComm "section" a ) = Processed $
   Paragraph [RawText $ fromString ("Failed to read section: " ++ show a)]
partitionSection (TeXSeq exs1 exs2) = (partitionSection exs1) ++ (partitionSection exs2)
partitionSection exs = Unprocessed exs

--layer2PartitionEnvs = ["itemize","enumerate"]

partitionLayer2 :: ProcessStatus -> ProcessStatus
partitionLayer2 (Processed a) = Processed a
partitionLayer2 (Unprocessed a) = let
      getListItems :: LaTeX -> ProcessStatus
      getListItems (TeXCommS "item") = Processed $ ListItem []
      getListItems (TeXSeq exs1 exs2) = (getListItems exs1) <> (getListItems exs2)
      getListItems exs = Unprocessed exs
      processLaTeX :: LaTeX -> ProcessStatus
      processLaTeX (TeXEnv envkind args content) =
         case (envkind, args) of
            ("itemize", _) -> Semiprocessed (Itemize []) (getListItems content)
            (_, _) -> Processed $ RawText $ render (TeXEnv envkind args content) -- A TEMPORARY SOLUTION
      processLaTeX (TeXSeq exs1 exs2) = (processLaTeX exs1) <> (processLaTeX exs2)
      processLaTeX b = Unprocessed b
   in processLaTeX a
partitionLayer2 (Semiprocessed a b) = Semiprocessed a b
partitionLayer2 (PSConc a b) = (partitionLayer2 a) <> (partitionLayer2 b)

partitionParagraphs :: ProcessStatus -> [ProcessStatus]
partitionParagraphs
