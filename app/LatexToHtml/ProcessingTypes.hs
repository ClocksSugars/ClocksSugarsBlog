module LatexToHtml.ProcessingTypes (HtmlVers(..)) where

import Data.Text

import LatexToHtml.InfoBoxType (
   InfoBox(..),
   getInfoBox
   )

data HtmlVers =
      RawText Text
   |  Paragraph [HtmlVers]
   |  Emphasize Text
   |  Bold Text
   |  Itemize [HtmlVers]
   |  Enumerate Text [HtmlVers]
   |  ListItem [HtmlVers]
   |  Subheading [HtmlVers] -- <h2>
   |  Figure Text Text
   |  BoxedSec InfoBox (Maybe [HtmlVers]) [HtmlVers]
   |  ReferenceNum String
   |  HLink Text [HtmlVers]
   |  HRefLink String [HtmlVers]
   deriving ( Show )
--   |  Env Text [HtmlVers]
