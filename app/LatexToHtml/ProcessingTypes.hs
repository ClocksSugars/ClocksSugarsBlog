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
   |  Subheading Text -- <h2>
   |  Figure Text Text
   |  BoxedSec String (Maybe Text) (Maybe [HtmlVers]) [HtmlVers]
   deriving ( Show )
--   |  Env Text [HtmlVers]
