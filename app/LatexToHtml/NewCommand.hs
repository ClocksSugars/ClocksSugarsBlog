{-# LANGUAGE OverloadedStrings #-}
module LatexToHtml.NewCommand where

import LatexToHtml.Utils (
   attachRightMostLaTeX,
   spanLaTeX
   )

import Data.Map.Strict (Map, empty, fromList, (!?))
import Text.LaTeX.Base
import Text.LaTeX.Base.Syntax

zeroArgMathCommands :: Map String LaTeX
zeroArgMathCommands = let
   in fromList [
      ("nats", TeXComm "mathbb" [FixArg (TeXRaw "N")]),
      ("powerset",TeXComm "mathcal" [FixArg (TeXRaw "P")]),
      ("inv",TeXRaw "-1"),
      ("integers", TeXComm "mathbb" [FixArg (TeXRaw "Z")]),
      ("rationals", TeXComm "mathbb" [FixArg (TeXRaw "Q")])
      ]

argMathCommands :: Map String ([TeXArg] -> LaTeX)
argMathCommands = let
   squotefunc :: [TeXArg] -> LaTeX
   squotefunc args = case args of
      [FixArg sometex] -> TeXSeq (TeXRaw "`") $ attachRightMostLaTeX sometex (TeXRaw "'")
      _ -> TeXRaw "error occurred here"
   dquotefunc :: [TeXArg] -> LaTeX
   dquotefunc args = case args of
      [FixArg sometex] -> TeXSeq (TeXRaw "``") $ attachRightMostLaTeX sometex (TeXRaw "\"")
      _ -> TeXRaw "error occurred here"
   in fromList [
      ("squote",squotefunc),
      ("dquote",dquotefunc)
   ]

applyMathCommands :: LaTeX -> LaTeX
applyMathCommands = let
   applyInArgs :: TeXArg -> TeXArg
   applyInArgs arg = case arg of
      FixArg l -> FixArg $ worker l
      OptArg l -> OptArg $ worker l
      MOptArg ls -> MOptArg $ map worker ls
      SymArg l -> SymArg $ worker l
      MSymArg ls -> MSymArg $ map worker ls
      ParArg l -> ParArg $ worker l
      MParArg ls -> MParArg $ map worker ls
   worker :: LaTeX -> LaTeX
   worker content = case content of
      TeXEmpty -> TeXEmpty
      TeXRaw x -> TeXRaw x
      TeXComm x args -> let
         iffunc = argMathCommands !? x
         in case iffunc of
            Just func -> func $ map applyInArgs args
            Nothing -> TeXComm x $ map applyInArgs args
      TeXCommS x -> let
         ifthing = zeroArgMathCommands !? x
         in case ifthing of
            Just thing -> thing
            Nothing -> TeXCommS x
      TeXMath kind x -> TeXMath kind $ TeXSeq (TeXRaw "I wonder if this will ever happen? ") $ worker x
      TeXLineBreak x y -> TeXLineBreak x y
      TeXBraces x -> TeXBraces $ worker x
      TeXComment x -> TeXComment x
      TeXSeq x y -> TeXSeq (worker x) (worker y)
      TeXEnv x args y -> TeXEnv x (map applyInArgs args) (worker y)
   in worker

zeroArgTextCommands :: Map String LaTeX
zeroArgTextCommands = let
   in fromList [

   ]

argTextCommands :: Map String ([TeXArg] -> LaTeX)
argTextCommands = let
   squotefunc :: [TeXArg] -> LaTeX
   squotefunc args = case args of
      [FixArg sometex] -> TeXSeq (TeXRaw "'") $ attachRightMostLaTeX sometex (TeXRaw "'")
      _ -> TeXRaw "error occurred here"
   dquotefunc :: [TeXArg] -> LaTeX
   dquotefunc args = case args of
      [FixArg sometex] -> TeXSeq (TeXRaw "\"") $ attachRightMostLaTeX sometex (TeXRaw "\"")
      _ -> TeXRaw "error occurred here"
   in fromList [
      ("squote",squotefunc),
      ("dquote",dquotefunc)
   ]

applyTextCommands :: LaTeX -> LaTeX
applyTextCommands = let
   applyInArgs :: TeXArg -> TeXArg
   applyInArgs arg = case arg of
      FixArg l -> FixArg $ worker l
      OptArg l -> OptArg $ worker l
      MOptArg ls -> MOptArg $ map worker ls
      SymArg l -> SymArg $ worker l
      MSymArg ls -> MSymArg $ map worker ls
      ParArg l -> ParArg $ worker l
      MParArg ls -> MParArg $ map worker ls
   worker :: LaTeX -> LaTeX
   worker content = case content of
      TeXEmpty -> TeXEmpty
      TeXRaw x -> TeXRaw x
      TeXComm x args -> let
         iffunc = argTextCommands !? x
         in case iffunc of
            Just func -> func $ map applyInArgs args
            Nothing -> TeXComm x $ map applyInArgs args
      TeXCommS x -> let
         ifthing = zeroArgTextCommands !? x
         in case ifthing of
            Just thing -> thing
            Nothing -> TeXCommS x
      TeXMath kind x -> TeXRaw $ render $ TeXMath kind $ worker x
      TeXLineBreak x y -> TeXLineBreak x y
      TeXBraces x -> TeXBraces $ worker x
      TeXComment x -> TeXComment x
      TeXSeq x y -> TeXSeq (worker x) (worker y)
      TeXEnv x args y -> TeXEnv x (map applyInArgs args) (worker y)
   in worker
