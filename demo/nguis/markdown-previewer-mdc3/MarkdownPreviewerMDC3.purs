module MarkdownPreviewerMDC3 (markdownPreviewerMDC3) where

import Prelude (Unit, otherwise, show, ($), (#), (&&), (+), (/=), (<#>), (<<<), (<>), (==), (>), (>>>))

import Data.Array (cons, span, uncons)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, trim)
import Data.String.CodeUnits (drop, indexOf, length, stripPrefix, take)
import Data.String.Common (joinWith)
import Data.Variant (match)
import Effect (Effect)
import PUI (asField, atField, completed, displayed, mvu)
import PUI.HTML (blockquote, body, code, dynamic, each, el, em, li, p, staticText, strong, ul, (:=))
import PUI.MDC3 (card, elevation5, filledTextArea, layoutCell, layoutGrid)
import QualifiedDo.Semigroupoid as Semigroupoid

markdownPreviewerMDC3 :: Effect Unit
markdownPreviewerMDC3 =
  body $
    elevation5 $
      card { caption: "Markdown Previewer" } $
        layoutGrid $ ( Semigroupoid.do
            layoutCell { span: 6 } $ filledTextArea { columns: 60, rows: 24 } # asField @"source" # completed
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
                    }
            ) # atField @"source"
        ) # mvu welcomeDocument

welcomeDocument :: { source :: String }
welcomeDocument =
  { source: """# Markdown Previewer

## What it does

Type markdown on the left, see it rendered on the right.

### Supported constructs

This paragraph shows **bold**, *italic* and `inline code` text.

- headings, three levels
- bullet lists like this one
- blank-line-separated paragraphs

> Blockquotes work too - and raw HTML like <b>this</b> stays literal text.
"""
  }

parseMarkdown
  :: String
  -> Array
       [ heading :: { level :: Int, inlines :: Array [ plain :: String, bold :: String, italic :: String, code :: String ] }
       , paragraph :: Array [ plain :: String, bold :: String, italic :: String, code :: String ]
       , bullets :: Array (Array [ plain :: String, bold :: String, italic :: String, code :: String ])
       , quote :: Array [ plain :: String, bold :: String, italic :: String, code :: String ]
       ]
parseMarkdown source = blocks (split (Pattern "\n") source)

blocks
  :: Array String
  -> Array
       [ heading :: { level :: Int, inlines :: Array [ plain :: String, bold :: String, italic :: String, code :: String ] }
       , paragraph :: Array [ plain :: String, bold :: String, italic :: String, code :: String ]
       , bullets :: Array (Array [ plain :: String, bold :: String, italic :: String, code :: String ])
       , quote :: Array [ plain :: String, bold :: String, italic :: String, code :: String ]
       ]
blocks ls = case uncons ls of
  Nothing -> []
  Just { head: l, tail }
    | trim l == "" -> blocks tail
    | Just t <- stripPrefix (Pattern "### ") l -> cons (.heading { level: 3, inlines: parseInlines t }) (blocks tail)
    | Just t <- stripPrefix (Pattern "## ") l -> cons (.heading { level: 2, inlines: parseInlines t }) (blocks tail)
    | Just t <- stripPrefix (Pattern "# ") l -> cons (.heading { level: 1, inlines: parseInlines t }) (blocks tail)
    | isBullet l ->
        let grouped = span isBullet (cons l tail)
        in cons (.bullets (grouped.init <#> \item -> parseInlines (drop 2 item))) (blocks grouped.rest)
    | isQuote l ->
        let grouped = span isQuote (cons l tail)
        in cons (.quote (parseInlines (joinWith " " (grouped.init <#> dropQuoteMark)))) (blocks grouped.rest)
    | otherwise ->
        let grouped = span isPlainLine (cons l tail)
        in cons (.paragraph (parseInlines (joinWith " " grouped.init))) (blocks grouped.rest)

isBullet :: String -> Boolean
isBullet l = case stripPrefix (Pattern "- ") l of
  Just _ -> true
  Nothing -> false

isQuote :: String -> Boolean
isQuote l = case stripPrefix (Pattern "> ") l of
  Just _ -> true
  Nothing -> false

dropQuoteMark :: String -> String
dropQuoteMark = drop 2

isPlainLine :: String -> Boolean
isPlainLine l = trim l /= "" && case stripPrefix (Pattern "#") l of
  Just _ -> false
  Nothing -> case isBullet l, isQuote l of
    false, false -> true
    _, _ -> false

parseInlines :: String -> Array [ plain :: String, bold :: String, italic :: String, code :: String ]
parseInlines s
  | s == "" = []
  | otherwise = case earliestSpan s of
      Nothing -> [ .plain s ]
      Just { at, open, close, make } ->
        let afterOpen = drop (at + length open) s
            before = if at > 0 then [ .plain (take at s) ] else []
        in case indexOf (Pattern close) afterOpen of
          Just end | end > 0 ->
            before <> cons (make (take end afterOpen)) (parseInlines (drop (end + length close) afterOpen))
          _ ->
            before <> cons (.plain open) (parseInlines afterOpen)

earliestSpan
  :: String
  -> Maybe
       { at :: Int
       , open :: String
       , close :: String
       , make :: String -> [ plain :: String, bold :: String, italic :: String, code :: String ]
       }
earliestSpan s = pick (candidate "**" "**" (.bold)) (pick (candidate "*" "*" (.italic)) (candidate "`" "`" (.code)))
  where
  candidate open close make = indexOf (Pattern open) s <#> \at -> { at, open, close, make }
  pick ma mb = case ma, mb of
    Just a, Just b -> Just (if a.at > b.at then b else a)
    Just a, Nothing -> Just a
    Nothing, mb' -> mb'
