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
   |  TTtext Text
   |  Itemize [HtmlVers]
   |  Enumerate Text [HtmlVers]
   |  ListItem [HtmlVers]
   |  Subheading (Maybe String) [HtmlVers] -- <h3>
   |  Subsubheading (Maybe String) [HtmlVers] -- <h4>
   |  Figure Text [HtmlVers]
   |  BoxedSec InfoBox (Maybe [HtmlVers]) [HtmlVers]
   |  ReferenceNum String
   |  HLink Text [HtmlVers]
   |  HRefLink String [HtmlVers]
   |  CodeBlock String Text
   |  JsEmbed String
   |  BreakLine
   |  CenteredParagraph [HtmlVers]
   deriving ( Show )
--   |  Env Text [HtmlVers]
