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
   deriving ( Show )
--   |  Env Text [HtmlVers]
