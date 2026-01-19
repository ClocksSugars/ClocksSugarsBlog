module LatexToHtml.Utils where

import Text.LaTeX.Base.Syntax


flattenTeXTree :: LaTeX -> [LaTeX]
flattenTeXTree tex = case tex of
   TeXSeq x y -> flattenTeXTree x ++ flattenTeXTree y
   x -> [x]

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

myShow4 :: [TeXArg] -> String
myShow4 [] = ""
myShow4 (ex:exs) = (myShow2 ex) ++ ", " ++ myShow4 exs

myShow3 :: [LaTeX] -> String
myShow3 [] = ""
myShow3 (ex:exs) = (myShow ex) ++ ", " ++ myShow3 exs

myShow2 :: TeXArg -> String
myShow2 texarg = case texarg of
   FixArg ex -> "FixArg (" ++ myShow ex ++ ")"   -- ^ Fixed argument.
   OptArg ex -> "OptArg (" ++ myShow ex ++ ")"   -- ^ Optional argument.
   MOptArg exs -> "MOptArg (" ++ myShow3 exs ++ ")" -- ^ Multiple optional argument.
   SymArg ex -> "SymArg (" ++ myShow ex ++ ")" -- ^ An argument enclosed between @\<@ and @\>@.
   MSymArg exs -> "MSymArg (" ++ myShow3 exs ++ ")" -- ^ Version of 'SymArg' with multiple options.
   ParArg ex -> "ParArg (" ++ myShow ex ++ ")" -- ^ An argument enclosed between @(@ and @)@.
   MParArg exs -> "MParArg (" ++ myShow3 exs ++ ")" -- ^ Version of 'ParArg' with multiple options.


myShow :: LaTeX -> String
myShow exs = case exs of
   TeXRaw xs -> "TeXRaw (" ++ show xs ++ ")"-- ^ Raw text.
   TeXComm xs texArgs -> "TeXComm (" ++ show xs ++ ", [" ++ myShow4 texArgs ++ "])" -- ^ Constructor for commands.
   TeXCommS xs -> "TeXCommS (" ++ show xs ++ ")"-- ^ Constructor for commands with no arguments.
   TeXEnv xs texargs ex -> "TeXEnv (" ++ show xs ++ ", " ++ myShow4 texargs ++ ", " ++  myShow ex ++ ")"-- ^ Constructor for environments.
   TeXMath math ex -> "TeXMath (" ++ show math ++ ", " ++ myShow ex ++ ")" -- ^ Mathematical expressions.
   TeXLineBreak meybe yn -> "TeXLineBreak (" ++ show meybe ++ ", " ++ show yn ++ ")" -- ^ Line break command.
   TeXBraces ex -> "TeXBraces (" ++ myShow ex ++ ")"-- ^ A expression between braces.
   TeXComment xs -> "TeXComment (" ++ show xs ++ ")"-- ^ Comments.
   TeXSeq ex1 ex2 -> myShow ex1 ++ ", " ++ myShow ex2 -- ^ Sequencing of 'LaTeX' expressions.
   TeXEmpty -> "TeXEmpty" -- ^ An empty block.
