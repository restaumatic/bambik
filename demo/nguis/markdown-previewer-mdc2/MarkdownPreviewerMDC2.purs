module MarkdownPreviewerMDC2 (markdownPreviewerMDC2) where

import Prelude (Unit, show, (#), ($), (<>), (>>>))

import Data.Variant (match)
import Effect (Effect)
import MarkdownPreviewerLogic (parseMarkdown, welcomeDocument)
import PUI (PUI, atField, mvu)
import PUI.Web (Web)
import PUI.Web.HTML (shown, blockquote, code, dynamic, each, el, em, li, p, staticText, strong, ul, (:=))
import PUI.Web.MDC2 (body, card, filledTextArea, layoutCell, layoutGrid)
import QualifiedDo.Category as Category

markdownPreviewerMDC2 :: Effect Unit
markdownPreviewerMDC2 =
  body $
    card $
      layoutGrid $ ( Category.do
        layoutCell { span: 6 } $ filledTextArea @"Source" { columns: 60, rows: 24 }
        layoutCell { span: 6 } $ ( dynamic \source -> each (parseMarkdown source) blockView ) # atField @"Source" # shown
      ) # mvu welcomeDocument

blockView :: [ heading :: { level :: Int, inlines :: Array [ plain :: String, bold :: String, italic :: String, code :: String ] }, paragraph :: Array [ plain :: String, bold :: String, italic :: String, code :: String ], bullets :: Array (Array [ plain :: String, bold :: String, italic :: String, code :: String ]), quote :: Array [ plain :: String, bold :: String, italic :: String, code :: String ] ] -> PUI Web {} {}
blockView = match
  { heading: \h -> el ("h" <> show h.level) (inlineViews h.inlines)
  , paragraph: \is -> p (inlineViews is)
  , bullets: \items -> ul (each items \is -> li (inlineViews is))
  , quote: \is -> blockquote >>> "style" := "border-left: 4px solid #ccc; margin-left: 0; padding-left: 12px; color: #555;" $ inlineViews is
  }

inlineViews :: Array [ plain :: String, bold :: String, italic :: String, code :: String ] -> PUI Web {} {}
inlineViews is = each is $ match
  { plain: staticText
  , bold: \s -> strong (staticText s)
  , italic: \s -> em (staticText s)
  , code: \s -> code >>> "style" := "background: #f0f0f0; padding: 1px 4px; border-radius: 3px;" $ staticText s
  }
