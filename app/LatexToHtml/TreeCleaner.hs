{-# LANGUAGE OverloadedStrings #-}
module LatexToHtml.TreeCleaner (
   extractDocument,
   processOne,
   processTwo,
   Htmllatexinter,
   processOneTwo,
   inlineCommands,
) where

import Data.ListLike (fromText)
import Data.Text (splitOn)
import Data.List (intersperse)
-- import Data.String
import Data.Maybe
import Data.Map.Strict (Map)
import Text.LaTeX.Base
-- import Text.Megaparsec
-- import Text.Megaparsec.Char
import Text.LaTeX.Base.Syntax
-- import Text.LaTeX.Base.Render
-- import Control.Monad
import Control.Applicative

import LatexToHtml.ProcessingTypes
import LatexToHtml.NewCommand
import LatexToHtml.Utils
import LatexToHtml.InfoBoxType (
   InfoBox(..),
   getInfoBox
   )
-- This all exists under the assumption that
--  the document being processed can produce
--  a pdf. If it passes the LaTeX Syntax
--  requirements then it should work here
--  is what we're aiming for.


--- When adding a new intermediary process type constructor
--    make CERTAIN to include it in processTwo or else the
--    program will not terminate



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

data Htmllatexinter =
      RawPrint Text
   |  RawLaTeX LaTeX
   |  Prose Text
   |  InLineEffect LaTeX -- for commands that belong in a paragraph
   |  Section [Htmllatexinter]
   |  SubSection [Htmllatexinter]
   |  List Text (Maybe [TeXArg]) [[Htmllatexinter]] -- instead of having items, we put item stuff in each element
   |  IFigure Text [Htmllatexinter]
   |  IBoxedSec InfoBox (Maybe [Htmllatexinter]) [Htmllatexinter]
   |  LineBreak
   |  IReference String
   |  ILink Text [Htmllatexinter]
   |  IRefLink String [Htmllatexinter]
   |  ICodeBlock String Text
   deriving (Eq, Show)

inlineCommands :: [String]
inlineCommands = ["emph","textbf","texttt"]

processOne :: LaTeX -> [Htmllatexinter]
processOne arg = let
   subprocess :: LaTeX -> Either [Htmllatexinter] Htmllatexinter
   subprocess (TeXRaw xs) = Right $ Prose xs
   subprocess (TeXComment _) = Left []
   subprocess (TeXSeq exs1 exs2) = Left $ case (subprocess exs1, subprocess exs2) of
      (Left a, Left b) -> a ++ b
      (Left a, Right b) -> a ++ [b]
      (Right a, Left b) -> a:b
      (Right a, Right b) -> [a,b]
   subprocess (TeXComm "subsection" [FixArg stuff]) = Right . Section $ processOne stuff
   subprocess (TeXComm "subsubsection" [FixArg stuff]) = Right . SubSection $ processOne stuff
   subprocess (TeXComm "figuresvgwithcaption" [FixArg (TeXRaw location), FixArg content]) =
      Right $ IFigure (location <> ".svg") $ processOne content
   subprocess (TeXComm "ref" [FixArg (TeXRaw referenceName)]) = Right $ IReference $ fromText referenceName
   subprocess (TeXComm "href" [FixArg (TeXRaw turl), FixArg tx]) = Right $ ILink turl $ processOne tx
   subprocess (TeXComm "hyperref" [OptArg (TeXRaw tref), FixArg tx]) = Right $ IRefLink (fromText tref) $ processOne tx
   subprocess (TeXComm "dquote" [FixArg dquotearg]) = subprocess $
      TeXSeq (TeXRaw "\"") $ attachRightMostLaTeX dquotearg $ TeXRaw"\""
   subprocess (TeXComm "squote" [FixArg dquotearg]) = subprocess $
      TeXSeq (TeXRaw "'") $ attachRightMostLaTeX dquotearg $ TeXRaw"'"
   subprocess (TeXEnv kind texargs content) = let
      fallbackcase = Right . RawPrint $ render (TeXEnv kind texargs content) -- This is under the assumption that it is a math env
      in case (kind, texargs) of
         ("itemize", _) -> Right $ List "itemize" Nothing $
            map processOne (drop 1 $ splitByDelimiterLaTeX (TeXCommS "item") content)
            -- must drop 1 bc content preceeding first \item should be ignored. we can also guarentee
            -- there is 2 since this should have compiled in latex thus having at least one \item
         ("enumerate", _) -> Right $ List "enumerate" Nothing $
            map processOne (drop 1 $ splitByDelimiterLaTeX (TeXCommS "item") content)
         ("lstlisting", [OptArg (TeXRaw "language=Rust")]) -> Right $ ICodeBlock "language-rust" $ render content
         ("lstlisting", [OptArg (TeXRaw "language=Haskell")]) -> Right $ ICodeBlock "language-haskell" $ render content

         ("gather*", _) -> Right . RawPrint . render $
            TeXMath DoubleDollar $ TeXEnv kind texargs $ applyMathCommands content
         ("align*", _) -> Right . RawPrint . render $
            TeXMath DoubleDollar $ TeXEnv kind texargs $ applyMathCommands content

         -- when there is a title it always appears first
         (_, OptArg pretitle : restoftexargs) -> case getInfoBox kind restoftexargs of
            Just infobox -> Right $
               IBoxedSec infobox (Just $ processOne pretitle) $ processOne content
            Nothing -> fallbackcase
         (_, _) -> case getInfoBox kind texargs of
            Just infobox -> Right $ IBoxedSec infobox Nothing $ processOne content
            Nothing -> fallbackcase

   subprocess (TeXMath sign content) = Right . InLineEffect $
      TeXMath sign $ applyMathCommands content
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

inLineTranslation :: Htmllatexinter -> HtmlVers
inLineTranslation (InLineEffect (TeXComm "emph" [FixArg (TeXRaw content)])) = Emphasize content
inLineTranslation (InLineEffect (TeXComm "textbf" [FixArg (TeXRaw content)])) = Bold content
inLineTranslation (InLineEffect (TeXComm "texttt" [FixArg (TeXRaw content)])) = TTtext content
inLineTranslation (InLineEffect (TeXMath sign content)) = RawText . render $ TeXMath sign content
inLineTranslation (InLineEffect otherwise) = RawText . render $ otherwise -- Just displays invalid commands so we can see
inLineTranslation (Prose xs) = RawText xs
inLineTranslation (RawPrint xs) = RawText xs
inLineTranslation (IReference reference) = ReferenceNum reference
inLineTranslation (ILink turl tx) = HLink turl $ map inLineTranslation tx
inLineTranslation (IRefLink tref tx) = HRefLink tref $ map inLineTranslation tx
inLineTranslation _ = RawText "failcase" -- this should NEVER HAPPEN since
-- it is applied to the takeWhile in the below span which has as its
-- condition that the constructor is Prose or InLineEffect


processTwo :: [Htmllatexinter] -> [HtmlVers]
processTwo [] = []
processTwo ((RawPrint content):xs) = RawText content : processTwo xs
processTwo ((RawLaTeX content):xs) = (RawText $ render content) : processTwo xs
processTwo ((Section content):xs) = Subheading (map inLineTranslation content) : processTwo xs
processTwo ((SubSection content):xs) = Subsubheading (map inLineTranslation content) : processTwo xs
processTwo ((IFigure location content):xs) = Figure location (processTwo content) : processTwo xs
processTwo ((ICodeBlock lang content):xs) = CodeBlock lang content : processTwo xs
-- processTwo ((IReference reference):xs) = ReferenceNum reference : processTwo xs
processTwo ((List kind margs items):xs) = case kind of
   "itemize" -> Itemize (map (ListItem . processTwo) items) : processTwo xs
   "enumerate" -> (\x -> Enumerate x (map (ListItem . processTwo) items) : processTwo xs) $
      case margs of
         Just [FixArg (TeXRaw x)] -> x
         _ -> "a"
   _ -> Paragraph [RawText "failed here"] : processTwo xs
processTwo ((IBoxedSec infobox mtitle content):xs) = BoxedSec infobox (
   fmap (map inLineTranslation) mtitle
   ) (processTwo content) : processTwo xs
processTwo arg = let
   paragraphRelevant :: Htmllatexinter -> Bool
   paragraphRelevant testitem = case testitem of
      Prose _ -> True
      RawPrint _ -> True
      LineBreak -> True
      InLineEffect _ -> True
      IReference _ -> True
      ILink _ _ -> True
      IRefLink _ _ -> True
      _ -> False
   cond :: Htmllatexinter -> Bool
   cond x= (x /= LineBreak)
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
