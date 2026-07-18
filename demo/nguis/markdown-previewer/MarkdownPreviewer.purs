module MarkdownPreviewer (markdownPreviewer) where

import Prelude (Unit, otherwise, show, ($), (#), (&&), (+), (/=), (<#>), (<<<), (<>), (==), (>), (>>>))

import Data.Array (cons, span, uncons)
import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap)
import Data.String (Pattern(..), split, trim)
import Data.String.CodeUnits (drop, indexOf, length, stripPrefix, take)
import Data.String.Common (joinWith)
import Effect (Effect)
import PUI (PUI, asField, completed, displayed, mvu)
import PUI.HTML (blockquote, body, code, div, each, el, em, foreachWith, li, p, staticText, strong, ul, (:=))
import PUI.Web (Web)
import PUI.MDC (card, elevation20, filledTextArea, layoutCell, layoutGrid)
import QualifiedDo.Semigroupoid as Semigroupoid

markdownPreviewer :: Effect Unit
markdownPreviewer =
  body $
    elevation20 $
      card { caption: "Markdown Previewer" } $
        layoutGrid $ ( Semigroupoid.do
            layoutCell { span: 6 } $ filledTextArea { columns: 60, rows: 24 } # asField @"source" # completed
            layoutCell { span: 6 } $
              div >>> "class" := "markdown-preview" >>> "style" := "border: 1px solid #ccc; border-radius: 4px; padding: 0 16px; min-height: 200px; overflow: auto;" $
                (lcmap (parseMarkdown <<< _.source) (foreachWith renderBlock) # displayed)
        ) # mvu welcomeDocument

renderBlock :: Block -> PUI Web {} {}
renderBlock (Heading level inlines) = el ("h" <> show level) (inlinesW inlines)
renderBlock (Paragraph inlines) = p (inlinesW inlines)
renderBlock (Bullets items) = ul (each items \inlines -> li (inlinesW inlines))
renderBlock (Quote inlines) =
  blockquote >>> "style" := "border-left: 4px solid #ccc; margin-left: 0; padding-left: 12px; color: #555;" $ inlinesW inlines

inlinesW :: Array Inline -> PUI Web {} {}
inlinesW inlines = each inlines renderInline

renderInline :: Inline -> PUI Web {} {}
renderInline (Plain s) = staticText s
renderInline (Bold s) = strong (staticText s)
renderInline (Italic s) = em (staticText s)
renderInline (Code s) = code >>> "style" := "background: #f0f0f0; padding: 1px 4px; border-radius: 3px;" $ staticText s

type Document = { source :: String }

welcomeDocument :: Document
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

data Block
  = Heading Int (Array Inline)
  | Paragraph (Array Inline)
  | Bullets (Array (Array Inline))
  | Quote (Array Inline)

data Inline
  = Plain String
  | Bold String
  | Italic String
  | Code String

parseMarkdown :: String -> Array Block
parseMarkdown source = blocks (split (Pattern "\n") source)

blocks :: Array String -> Array Block
blocks ls = case uncons ls of
  Nothing -> []
  Just { head: l, tail }
    | trim l == "" -> blocks tail
    | Just t <- stripPrefix (Pattern "### ") l -> cons (Heading 3 (parseInlines t)) (blocks tail)
    | Just t <- stripPrefix (Pattern "## ") l -> cons (Heading 2 (parseInlines t)) (blocks tail)
    | Just t <- stripPrefix (Pattern "# ") l -> cons (Heading 1 (parseInlines t)) (blocks tail)
    | isBullet l ->
        let grouped = span isBullet (cons l tail)
        in cons (Bullets (grouped.init <#> \item -> parseInlines (drop 2 item))) (blocks grouped.rest)
    | isQuote l ->
        let grouped = span isQuote (cons l tail)
        in cons (Quote (parseInlines (joinWith " " (grouped.init <#> dropQuoteMark)))) (blocks grouped.rest)
    | otherwise ->
        let grouped = span isPlainLine (cons l tail)
        in cons (Paragraph (parseInlines (joinWith " " grouped.init))) (blocks grouped.rest)

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

parseInlines :: String -> Array Inline
parseInlines s
  | s == "" = []
  | otherwise = case earliestSpan s of
      Nothing -> [ Plain s ]
      Just { at, open, close, make } ->
        let afterOpen = drop (at + length open) s
            before = if at > 0 then [ Plain (take at s) ] else []
        in case indexOf (Pattern close) afterOpen of
          Just end | end > 0 ->
            before <> cons (make (take end afterOpen)) (parseInlines (drop (end + length close) afterOpen))
          _ ->
            before <> cons (Plain open) (parseInlines afterOpen)

type Span = { at :: Int, open :: String, close :: String, make :: String -> Inline }

earliestSpan :: String -> Maybe Span
earliestSpan s = pick (candidate "**" "**" Bold) (pick (candidate "*" "*" Italic) (candidate "`" "`" Code))
  where
  candidate open close make = indexOf (Pattern open) s <#> \at -> { at, open, close, make }
  pick ma mb = case ma, mb of
    Just a, Just b -> Just (if a.at > b.at then b else a)
    Just a, Nothing -> Just a
    Nothing, mb' -> mb'
