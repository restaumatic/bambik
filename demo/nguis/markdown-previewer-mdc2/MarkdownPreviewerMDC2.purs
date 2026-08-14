module MarkdownPreviewerMDC2 (markdownPreviewerMDC2) where

import Prelude (Unit, show, (#), ($), (<>), (>>>))

import Data.Variant (match)
import Effect (Effect)
import MarkdownPreviewerLogic (parseMarkdown, welcomeDocument)
import PUI (atField, completed, displayed, mvu)
import PUI.Web.HTML (blockquote, body, code, dynamic, each, el, em, li, p, staticText, strong, ul, (:=))
import PUI.Web.MDC2 (card, elevation20, filledTextArea, layoutCell, layoutGrid)
import QualifiedDo.Semigroupoid as Semigroupoid

markdownPreviewerMDC2 :: Effect Unit
markdownPreviewerMDC2 =
  body $
    elevation20 $
      card { caption: "Markdown Previewer" } $
        layoutGrid $ ( Semigroupoid.do
            layoutCell { span: 6 } $ filledTextArea @"source" { columns: 60, rows: 24 } # completed
            layoutCell { span: 6 } $ displayed $ ( dynamic \source ->
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
                    } ) # atField @"source"
        ) # mvu welcomeDocument
