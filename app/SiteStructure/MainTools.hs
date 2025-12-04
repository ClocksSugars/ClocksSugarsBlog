{-# LANGUAGE OverloadedRecordDot #-}
module SiteStructure.MainTools (
  tempappliuni,
  parseBook
) where

import LatexToHtml.MainTools
import SiteStructure.DefaultPage
import SiteStructure.Manifest
import SiteStructure.AddressManagement
import SiteStructure.WorkHorse
import SiteStructure.RecordTypes (WrittenWorkBook(..))

-- webBookFromManifest :: WrittenWorkBook -> RefIndexState -> IO (Maybe RefIndexState)
-- webBookFromManifest workbook = _
