module MarkdownPreviewerMDC3 (markdownPreviewerMDC3) where

import Prelude (Unit, show, (#), ($), (<>), (>>>))

import Data.Variant (match)
import Effect (Effect)
import MarkdownPreviewerViewModel (parseMarkdown, welcomeDocument)
import PUI (PUI, mvu)
import PUI.Web (Web, dynamic, each, el, shown, staticString, (:=))
import PUI.Web.HTML (blockquote, code, em, li, p, strong, ul)
import PUI.Web.MDC3 (body, filledTextArea, layoutCell, layoutGrid)
import QualifiedDo.Semigroupoid as Semigroupoid

markdownPreviewerMDC3 :: Effect Unit
markdownPreviewerMDC3 =
  body $
    layoutGrid $ ( Semigroupoid.do
      layoutCell 6 $ filledTextArea @"Source" { columns: 60, rows: 24 }
      layoutCell 6 $ ( dynamic documentView ) # shown
    ) # mvu @( "Source" :: String ) welcomeDocument

documentView :: { "Source" :: String } -> PUI Web {} {}
documentView document = each (parseMarkdown document) blockView

blockView :: [ heading :: { level :: Int, inlines :: Array [ plain :: String, bold :: String, italic :: String, code :: String ] }, paragraph :: Array [ plain :: String, bold :: String, italic :: String, code :: String ], bullets :: Array (Array [ plain :: String, bold :: String, italic :: String, code :: String ]), quote :: Array [ plain :: String, bold :: String, italic :: String, code :: String ] ] -> PUI Web {} {}
blockView = match
  { heading: \h -> el ("h" <> show h.level) (inlineViews h.inlines)
  , paragraph: \is -> p (inlineViews is)
  , bullets: \items -> ul (each items \is -> li (inlineViews is))
  , quote: \is -> blockquote >>> "style" := "border-left: 4px solid #ccc; margin-left: 0; padding-left: 12px; color: #555;" $ inlineViews is
  }

inlineViews :: Array [ plain :: String, bold :: String, italic :: String, code :: String ] -> PUI Web {} {}
inlineViews is = each is $ match
  { plain: staticString
  , bold: \s -> strong (staticString s)
  , italic: \s -> em (staticString s)
  , code: \s -> code >>> "style" := "background: #f0f0f0; padding: 1px 4px; border-radius: 3px;" $ staticString s
  }
