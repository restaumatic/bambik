module MarkdownPreviewer (Block, Document, Inline, parseMarkdown, welcomeDocument) where

import Prelude (not, otherwise, (&&), (+), (/=), (<#>), (<<<), (<>), (==), (>), (||))

import Data.Array (any, cons, span, uncons)
import Data.Maybe (Maybe(..), isJust)
import Data.String (Pattern(..), split, trim)
import Data.String.CodeUnits (drop, indexOf, length, stripPrefix, take)
import Data.String.Common (joinWith)

type Document = { "Source" :: String }

type Block =
  [ bullets :: Array (Array Inline)
  , heading :: { inlines :: Array Inline, level :: Int }
  , paragraph :: Array Inline
  , quote :: Array Inline
  ]

type Inline = [ bold :: String, code :: String, italic :: String, plain :: String ]

welcomeDocument :: Document
welcomeDocument =
  { "Source": """# Markdown Previewer

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

parseMarkdown :: Document -> Array Block
parseMarkdown document = blocks (split (Pattern "\n") document."Source")

blocks :: Array String -> Array Block
blocks ls = case uncons ls of
  Nothing -> []
  Just { head: l, tail }
    | trim l == "" -> blocks tail
    | Just t <- stripPrefix (Pattern "### ") l -> cons (.heading { level: 3, inlines: parseInlines t }) (blocks tail)
    | Just t <- stripPrefix (Pattern "## ") l -> cons (.heading { level: 2, inlines: parseInlines t }) (blocks tail)
    | Just t <- stripPrefix (Pattern "# ") l -> cons (.heading { level: 1, inlines: parseInlines t }) (blocks tail)
    | isBullet l ->
      let grouped = span isBullet (cons l tail)
      in cons (.bullets (grouped.init <#> parseInlines <<< dropMarker)) (blocks grouped.rest)
    | isQuote l ->
      let grouped = span isQuote (cons l tail)
      in cons (.quote (parseInlines (joinWith " " (grouped.init <#> dropMarker)))) (blocks grouped.rest)
    | otherwise ->
      let grouped = span isPlainLine (cons l tail)
      in cons (.paragraph (parseInlines (joinWith " " grouped.init))) (blocks grouped.rest)

isBullet :: String -> Boolean
isBullet = isJust <<< stripPrefix (Pattern "- ")

isQuote :: String -> Boolean
isQuote = isJust <<< stripPrefix (Pattern "> ")

dropMarker :: String -> String
dropMarker = drop 2

isHeading :: String -> Boolean
isHeading l = any (\marker -> isJust (stripPrefix (Pattern marker) l)) [ "# ", "## ", "### " ]

isPlainLine :: String -> Boolean
isPlainLine l = trim l /= "" && not (isHeading l || isBullet l || isQuote l)

parseInlines :: String -> Array Inline
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
  -> Maybe { at :: Int, open :: String, close :: String, make :: String -> Inline }
earliestSpan s = pick (candidate "**" "**" (.bold)) (pick (candidate "*" "*" (.italic)) (candidate "`" "`" (.code)))
  where
  candidate open close make = indexOf (Pattern open) s <#> \at -> { at, open, close, make }
  pick ma mb = case ma, mb of
    Just a, Just b -> Just (if a.at > b.at then b else a)
    Just a, Nothing -> Just a
    Nothing, mb' -> mb'
