module MarkdownPreviewerMDC2 (markdownPreviewerMDC2) where

import Prelude (identity, Unit, show, (#), ($), (<>), (>>>))

import Data.Variant (match)
import Effect (Effect)
import MarkdownPreviewerLogic (parseMarkdown, welcomeDocument)
import PUI (atField, completed, mvu)
import PUI.Web.HTML (shownAs, blockquote, body, code, dynamic, each, el, em, li, p, staticText, strong, ul, (:=))
import PUI.Web.MDC2 (card, elevation20, filledTextArea, layoutCell, layoutGrid)
import QualifiedDo.Semigroupoid as Semigroupoid

markdownPreviewerMDC2 :: Effect Unit
markdownPreviewerMDC2 =
  body $
    elevation20 $
      card $
        layoutGrid $ ( Semigroupoid.do
            layoutCell { span: 6 } $ filledTextArea @"Source" { columns: 60, rows: 24 } # completed
            layoutCell { span: 6 } $ shownAs identity $ ( dynamic \source ->
                each (parseMarkdown source) \block ->
                  let
                    inline = match
                      { plain: staticText
                      , bold: \s -> strong (staticText s)
                      , italic: \s -> em (staticText s)
                      , code: \s -> code >>> "style" := "background: #f0f0f0; padding: 1px 4px; border-radius: 3px;" $ staticText s
                      }
                    inlines is = each is inline
                  in block # match
                    { heading: \h -> el ("h" <> show h.level) (inlines h.inlines)
                    , paragraph: \is -> p (inlines is)
                    , bullets: \items -> ul (each items \is -> li (inlines is))
                    , quote: \is -> blockquote >>> "style" := "border-left: 4px solid #ccc; margin-left: 0; padding-left: 12px; color: #555;" $ inlines is
                    } ) # atField @"Source"
        ) # mvu welcomeDocument
