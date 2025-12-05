{-# LANGUAGE OverloadedStrings #-}

module SiteStructure.AddressManagement where

import Data.String
import Data.List (intercalate)
import System.Directory
import System.IO

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal (MarkupM(Empty))


targetHomePage :: String
targetHomePage = "index.html"

outputfolder :: String
outputfolder = "public"


type FolderPath = [String] -- stored backwards.
-- also intended to treat last object as a folder to which a file extension
--    can be added but really we should just set up index.html things

writeFileMakePath :: FolderPath -> String -> String -> IO ()
writeFileMakePath [] _ _ = putStrLn "was told to write file with no address"
writeFileMakePath (x:y:fxs) fileext xs = do
   createDirectoryIfMissing True (folderPathRender $ y:fxs)
   writeFile (folderPathRender (x:y:fxs) ++ fileext) xs
writeFileMakePath (x:[]) fileext xs = writeFile (x ++ fileext) xs


folderPathRender :: FolderPath -> String
folderPathRender fpath = intercalate "/" $ reverse fpath

addressListHtml :: FolderPath -> Html
addressListHtml fpath = let
   makeEachLink :: FolderPath -> [Html] -- this will come out backwards. also skips first item on purpose
   makeEachLink [] = [do {a ! href "/" $ "Home"}]
   makeEachLink (_:y:xs) =
      [H.a ! href (fromString $ folderPathRender (y:xs)) $ toHtml y]
         <> makeEachLink (y:xs)
   makeEachLink (_:_) = makeEachLink []
   reversedaddresses = case fpath of
      [] -> [] --[do {a ! href "/" $ "Home"}]
      x:xs -> toHtml x : makeEachLink (x:xs)
   assembleAddressLine :: [Html] -> Html
   assembleAddressLine [] = Empty ()
   assembleAddressLine (x:xs) = (>>) x $ (>>) "/" $ assembleAddressLine xs
   in assembleAddressLine (reverse reversedaddresses)

-- do
--    H.a ! (href . toHtml $ ""
