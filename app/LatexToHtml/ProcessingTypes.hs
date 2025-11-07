module LatexToHtml.ProcessingTypes where

import Data.Text

data HtmlVers =
      RawText Text
   |  Paragraph [HtmlVers]
   |  Italicisize Text
   |  Bold Text
   |  Itemize [HtmlVers]
   |  Enumerate Text [HtmlVers]
   |  ListItem [HtmlVers]
   |  Subheading Text -- <h2>
--   |  Env Text [HtmlVers]
