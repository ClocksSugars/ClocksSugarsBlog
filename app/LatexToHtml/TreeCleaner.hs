{-# LANGUAGE OverloadedStrings #-}
module LatexToHtml.TreeCleaner (
   extractDocument,
   processOne,
   processTwo,
   Htmllatexinter,
   processOneTwo,
   inlineCommands,
   attachRightMostLaTeX,
   spanLaTeX
) where

import Data.ListLike (fromText)
import Data.Text (splitOn)
import Data.List (intersperse)
-- import Data.String
import Data.Maybe
import Text.LaTeX.Base
-- import Text.Megaparsec
-- import Text.Megaparsec.Char
import Text.LaTeX.Base.Syntax
-- import Text.LaTeX.Base.Render
-- import Control.Monad
import Control.Applicative

import LatexToHtml.ProcessingTypes

-- This all exists under the assumption that
--  the document being processed can produce
--  a pdf. If it passes the LaTeX Syntax
--  requirements then it should work here
--  is what we're aiming for.

-- type Parser = Parsec () LaTeX


extractDocument :: LaTeX -> LaTeX
extractDocument restOfIt = let
   interiorDoc :: LaTeX -> Maybe LaTeX
   interiorDoc (TeXEnv "document" _ content) = Just content
   interiorDoc (TeXSeq exs1 exs2) = interiorDoc exs1 <|> interiorDoc exs2
   interiorDoc _ = Nothing
   in fromMaybe restOfIt (interiorDoc restOfIt)

-- climbs down the tree and attaches something at maximum right-most depth
attachRightMostLaTeX :: LaTeX -> LaTeX -> LaTeX
attachRightMostLaTeX (TeXSeq ex exs) attachment = TeXSeq ex (
   attachRightMostLaTeX exs attachment
   )
attachRightMostLaTeX exs attachment = TeXSeq exs attachment

-- This function assumes that the LaTeX TeXSeq tree branches right only
spanLaTeX :: (LaTeX -> Bool) -> LaTeX -> (LaTeX, LaTeX)
spanLaTeX cond content = let
   worker :: (LaTeX, LaTeX) -> (LaTeX, LaTeX)
   worker (taken, TeXSeq ex exs) = if cond ex
      then worker (attachRightMostLaTeX taken ex, exs)
      else (taken, exs)
   worker (taken, ex) = if cond ex
      then (attachRightMostLaTeX taken ex, TeXEmpty)
      else (taken, TeXEmpty)
   removeCap (TeXSeq TeXEmpty exs, remainder) = (exs, remainder)
   removeCap (exs, remainder) = (exs, remainder)
   in removeCap . worker $ (TeXEmpty, content)

splitByDelimiterLaTeX :: LaTeX -> LaTeX -> [LaTeX]
splitByDelimiterLaTeX delimiter content = let
   cond :: LaTeX -> Bool
   cond x = x /= delimiter
   worker :: [LaTeX] -> LaTeX -> [LaTeX]
   worker xs TeXEmpty = xs
   worker xs exs = let
      (taken, remainder) = spanLaTeX cond exs
      in worker (xs ++ [taken]) remainder
   in worker [] content

 -- Things that should cause the document to be sectioned in order of priority:
 --   - Section Heading
 --   - Environments (itemize and enumerate)
 --   - paragraphs
 --   - Environments (math ones) but these dont actually need to be processed most of the time

data Htmllatexinter =
      RawPrint Text
   |  RawLaTeX LaTeX
   |  Prose Text
   |  InLineEffect LaTeX -- for commands that belong in a paragraph
   |  Section Text
   |  List Text (Maybe [TeXArg]) [[Htmllatexinter]] -- instead of having items, we put item stuff in each element
--   |  Figure Text
   |  LineBreak
   deriving (Eq, Show)

inlineCommands :: [String]
inlineCommands = ["emph","textbf"]

processOne :: LaTeX -> [Htmllatexinter]
processOne arg = let
   subprocess (TeXRaw xs) = Right $ Prose xs
   -- isWhiteSpace :: String -> Bool
   -- isWhiteSpace [] = True
   -- isWhiteSpace (' ':xs) = isWhiteSpace xs
   -- isWhiteSpace ('\n':xs) = isWhiteSpace xs
   -- isWhiteSpace ('\t':xs) = isWhiteSpace xs
   -- isWhiteSpace _ = False
   -- subprocess :: LaTeX -> Either [Htmllatexinter] Htmllatexinter
   -- subprocess (TeXRaw xs) = if isWhiteSpace . fromText $ xs
   --    then Left []
   --    else Right (Prose xs)
   subprocess (TeXComment _) = Left []
   subprocess (TeXSeq exs1 exs2) = Left $ case (subprocess exs1, subprocess exs2) of
      (Left a, Left b) -> a ++ b
      (Left a, Right b) -> a ++ [b]
      (Right a, Left b) -> a:b
      (Right a, Right b) -> [a,b]
   subprocess (TeXComm "section" [FixArg (TeXRaw title)]) = Right $ Section title
   subprocess (TeXEnv kind texargs content) = case (kind, texargs) of
         ("itemize", _) -> Right $ List "itemize" Nothing $
            -- must drop 1 bc content preceeding first \item should be ignored. we can also guarentee
            -- there is 2 since this should have compiled in latex thus having at least one \item
            map processOne (drop 1 $ splitByDelimiterLaTeX (TeXCommS "item") content)
         (_, _) -> Right . RawPrint $ render (TeXEnv kind texargs content) -- This is under the assumption that it is a math env
   subprocess (TeXMath sign content) = Right . InLineEffect $ TeXMath sign content
   subprocess (TeXBraces content) = case content of
      TeXEmpty -> Left []
      exs -> subprocess exs
   subprocess (TeXComm command args) = if (elem command inlineCommands)
      then Right . InLineEffect $ TeXComm command args
      else Right . RawLaTeX $ TeXComm command args
   subprocess (TeXCommS command) = Right . RawLaTeX $ TeXCommS command
   -- ^ this will need to be changed if i add inline non-math commands
   subprocess TeXEmpty = Left []
   subprocess (TeXLineBreak _ _) = Right LineBreak
   --subprocess ex = Right . RawPrint $ render ex
   in case subprocess arg of
      Right thing -> [thing]
      Left thing -> thing

addLineBreaks :: [Htmllatexinter] -> [Htmllatexinter]
addLineBreaks stuff = let
   worker :: [Htmllatexinter] -> [Htmllatexinter]
   worker ((Prose content):xs) = intersperse LineBreak (
      map Prose (splitOn "\n\n" content)
      ) ++ worker xs
   worker (x:xs) = x: worker xs
   worker [] = []
   pruneEmpty :: [Htmllatexinter] -> [Htmllatexinter]
   pruneEmpty ((Prose ""):xs) = pruneEmpty xs
   pruneEmpty (x:xs) = (x : pruneEmpty xs)
   pruneEmpty [] = []
   in pruneEmpty . worker $ stuff


-- Things that cause or dont cause a new paragraph
--    - A new section/heading negates a linebreak but causes a paragraph to start
--    - The beginning or end of a list item negates a linebreak but causes a paragraph to start
--

processTwo :: [Htmllatexinter] -> [HtmlVers]
processTwo [] = []
processTwo ((RawPrint content):xs) = RawText content : processTwo xs
processTwo ((RawLaTeX content):xs) = (RawText $ render content) : processTwo xs
processTwo ((Section content):xs) = Subheading content : processTwo xs
processTwo ((List kind margs items):xs) = case kind of
   "itemize" -> Itemize (map (ListItem . processTwo) items) : processTwo xs
   _ -> Paragraph [RawText "failed here"] : processTwo xs
processTwo arg = let
   paragraphRelevant :: Htmllatexinter -> Bool
   paragraphRelevant testitem = case testitem of
      Prose _ -> True
      LineBreak -> True
      InLineEffect _ -> True
      _ -> False
   cond :: Htmllatexinter -> Bool
   cond x= (x /= LineBreak)
   inLineTranslation :: Htmllatexinter -> HtmlVers
   inLineTranslation (InLineEffect (TeXComm "emph" [FixArg (TeXRaw content)])) = Italicisize content
   inLineTranslation (InLineEffect (TeXComm "textbf" [FixArg (TeXRaw content)])) = Bold content
   inLineTranslation (InLineEffect (TeXMath sign content)) = RawText . render $ TeXMath sign content
   inLineTranslation (InLineEffect otherwise) = RawText . render $ otherwise -- Just displays invalid commands so we can see
   inLineTranslation (Prose xs) = RawText xs
   inLineTranslation _ = RawText "failcase" -- this should NEVER HAPPEN since
   -- it is applied to the takeWhile in the below span which has as its
   -- condition that the constructor is Prose or InLineEffect
   worker :: [Htmllatexinter] -> [HtmlVers]
   worker (LineBreak:xs) = worker xs
   worker [] = []
   worker xs = let
      (paragraph, remainder2) = span cond xs
      in (Paragraph (map inLineTranslation paragraph) : worker remainder2)
   (prose, remainder) = span paragraphRelevant arg
   in (worker . addLineBreaks $ prose) ++ processTwo remainder

processOneTwo :: LaTeX -> [HtmlVers]
processOneTwo = processTwo . processOne
