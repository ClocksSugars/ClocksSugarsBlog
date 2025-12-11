{-# LANGUAGE OverloadedStrings #-}
module LatexToHtml.InfoBoxType where

import Data.ListLike (fromText)

import Text.LaTeX.Base
import Text.LaTeX.Base.Syntax

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A


--- The structure of this file is purposefully verbose so as to be easily changed


-- Boxes with Maybe Bool are those that are minimizable
-- We have Maybe String for labels but we do not include titles here
--    since they need to be processed externally. Luckily titles are
--    optional and thus always the first argument so we pluck them off
--    process them there.
data InfoBox =
      DefinitionBox (Maybe String)
   |  TheoremBox (Maybe String)
   |  PropositionBox (Maybe String)
   |  LemmaBox (Maybe String)
   |  ProofBox (Maybe Bool) (Maybe String)
   |  CorollaryBox (Maybe String)
   |  NotationBox (Maybe String)
   |  ExampleBox (Maybe Bool) (Maybe String)
   |  RemarkBox (Maybe Bool) (Maybe String)
   deriving (Eq, Show)

getInfoBox :: String -> [TeXArg] -> Maybe InfoBox
getInfoBox kind texargs = let

   getLabelAlone :: [TeXArg] -> Maybe String
   getLabelAlone targs = case targs of
      [FixArg (TeXRaw x)] -> Just $ fromText x
      _ -> Nothing

   getMinMaybeLabel :: [TeXArg] -> (Maybe Bool, Maybe String)
   getMinMaybeLabel targs = case targs of
      [FixArg TeXEmpty, FixArg (TeXRaw x)] -> (Nothing, Just $ fromText x)
      [FixArg (TeXRaw "m"), FixArg (TeXRaw x)] -> (Just False, Just $ fromText x)
      [FixArg (TeXRaw "mo"), FixArg (TeXRaw x)] -> (Just True, Just $ fromText x)
      [FixArg (TeXRaw "m")] -> (Just False, Nothing)
      [FixArg (TeXRaw "mo")] -> (Just True, Nothing)
      _ -> (Nothing, Nothing)

   in case kind of
      "definition" -> Just $ DefinitionBox Nothing
      "theorem" -> Just $ TheoremBox Nothing
      "proposition" -> Just $ PropositionBox Nothing
      "lemma" -> Just $ LemmaBox Nothing
      "corollary" -> Just $ CorollaryBox Nothing
      "notation" -> Just $ NotationBox Nothing

      "label definition" -> Just $ DefinitionBox $ getLabelAlone texargs
      "label theorem" -> Just $ TheoremBox $ getLabelAlone texargs
      "label proposition" -> Just $ PropositionBox $ getLabelAlone texargs
      "label lemma" -> Just $ LemmaBox $ getLabelAlone texargs
      "label corollary" -> Just $ CorollaryBox $ getLabelAlone texargs
      "label notation" -> Just $ NotationBox $ getLabelAlone texargs

      minimizable -> case (minimizable, getMinMaybeLabel texargs) of
         ("my proof", (minstate, _)) -> Just $ ProofBox minstate Nothing
         ("label proof", (minstate, mlabel)) -> Just $ ProofBox minstate mlabel
         ("example", (minstate, _)) -> Just $ ExampleBox minstate Nothing
         ("label example", (minstate, mlabel)) -> Just $ ExampleBox minstate mlabel
         ("remark", (minstate, _)) -> Just $ RemarkBox minstate Nothing
         ("label remark", (minstate, mlabel)) -> Just $ RemarkBox minstate mlabel

         _ -> Nothing


--- make boxNum be zero for proof
boxToHtml :: InfoBox -> Maybe Html -> String -> Int -> Html -> (Html, String)
boxToHtml infobox mtitle pagename boxNum content = let
   boxName :: String
   maybeMinimizable :: Maybe Bool
   maybeLabel :: Maybe String
   (boxName, maybeMinimizable, maybeLabel) = case infobox of
      DefinitionBox x -> ("Definition", Nothing, x)
      TheoremBox x -> ("Theorem", Nothing, x)
      PropositionBox x -> ("Proposition", Nothing, x)
      LemmaBox x -> ("Lemma", Nothing, x)
      CorollaryBox x -> ("Corollary", Nothing, x)
      NotationBox x -> ("Notation", Nothing, x)

      ProofBox x y -> ("Proof", x, y)
      ExampleBox x y -> ("Example", x, y)
      RemarkBox x y -> ("Remark", x, y)

   standardBox :: Html
   standardBox = let
      boxhead = H.div ! class_ (fromString $ boxName ++ "title")
      maybeLabelBoxHead = case maybeLabel of
         Just thelabel -> boxhead ! (A.id . fromString $ thelabel)
         Nothing -> boxhead
      topBarText = case (mtitle, boxName) of
         (Just ttitle, "Proof") -> ttitle
         (Just ttitle, _) -> (>>) (toHtml $ boxName ++ " " ++ pagename ++ "." ++ show boxNum) $
            (H.span ! A.style "padding: 1em" $ "—") >> ttitle
         (Nothing, "Proof") -> "Proof."
         (Nothing, _) -> toHtml (boxName ++ " " ++ pagename ++ "." ++ show boxNum)
      in do
         H.div ! class_ (fromString boxName) $ do
            maybeLabelBoxHead topBarText
            content

   minimizeBox :: Bool -> Html
   minimizeBox openstate = let
      boxhead = H.summary ! class_ (fromString $ boxName ++ "title")
      maybeLabelBoxHead = case maybeLabel of
         Just thelabel -> boxhead ! (A.id . fromString $ thelabel)
         Nothing -> boxhead
      detailsHead = H.details ! class_ (fromString boxName)
      maybeOpenDetailsHead = if openstate
         then detailsHead ! A.open ""
         else detailsHead
      topBarText = case (mtitle, boxName) of
         (Just ttitle, "Proof") -> ttitle
         (Just ttitle, _) -> (>>) (toHtml $ boxName ++ " " ++ pagename ++ "." ++ show boxNum) $
            (H.span ! A.style "padding: 1em" $ "—") >> ttitle
         (Nothing, "Proof") -> "Proof."
         (Nothing, _) -> toHtml (boxName ++ " " ++ pagename ++ "." ++ show boxNum)
      in do
         maybeOpenDetailsHead $ do
            maybeLabelBoxHead topBarText
            content

   in (\thehtml -> (thehtml, boxName)) $ case maybeMinimizable of
      Nothing -> standardBox
      Just isminimized -> minimizeBox isminimized


getMaybeLabelInfoBox :: InfoBox -> Maybe String
getMaybeLabelInfoBox infobox = case infobox of
   DefinitionBox x -> x
   TheoremBox x -> x
   PropositionBox x -> x
   LemmaBox x -> x
   ProofBox _ x -> x
   CorollaryBox x -> x
   NotationBox x -> x
   ExampleBox _ x -> x
   RemarkBox _ x -> x


shouldUpdateThmCounter :: InfoBox -> Bool
shouldUpdateThmCounter (ProofBox _ _) = False
shouldUpdateThmCounter _ = True
