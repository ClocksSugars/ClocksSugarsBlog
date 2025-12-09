module LatexToHtml.ProcessingTypes (HtmlVers(..)) where

import Data.Text

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
   |  BoxedSec String (Maybe Text) (Maybe [HtmlVers]) [HtmlVers]
   |  MProofBox Bool (Maybe Text) (Maybe [HtmlVers]) [HtmlVers]
   |  ReferenceNum Text
   |  HLink Text Text
   deriving ( Show )
--   |  Env Text [HtmlVers]
