-- | Typed, tuple-free, string-free markup for custom-leaf render functions
-- | (the render half of `PUI.HTML`'s `view`). An element-function DSL —
-- | `div [ cl "row" ] [ text "hi" ]` — that renders straight to real DOM
-- | nodes (`createElementNS`/`setAttribute`/`createTextNode`), never to an
-- | HTML string: injection-proof by construction (the DOM escapes text and
-- | attribute values), no `escapeHtml` at call sites. Not a virtual DOM:
-- | `view` still rebuilds the container's children wholesale per value fed.
-- |
-- | `Markup` and `Attr` are opaque — build with the tag helpers (`div`,
-- | `table`, `circle`, …, or the generic `el` for dynamic/SVG tags) and the
-- | attribute helpers (`cl`, `style`, `dataKey`, …, or the generic `attr`).
-- | Namespace is a render concern, not a construction one: `svg`/`circle` are
-- | ordinary constructors; the renderer switches to the SVG namespace for the
-- | subtree of any `svg` element.
module PUI.Markup
  ( Markup
  , Attr
  , text
  , el
  , div
  , span
  , p
  , ul
  , li
  , table
  , tr
  , th
  , td
  , strong
  , em
  , code
  , blockquote
  , img
  , svg
  , circle
  , attr
  , cl
  , style
  , dataKey
  , src
  , alt
  , title
  , href
  , id
  , buildNode
  , buildInto
  , htmlNS
  , svgNS
  ) where

import Prelude

import Data.Foldable (for_)
import Effect (Effect)
import PUI.Web (Node, appendChild, createElementNS, createTextNode, removeAllChildren, setAttribute)

data Attr = Attr String String

data Markup
  = Element String (Array Attr) (Array Markup)
  | Text String

text :: String -> Markup
text = Text

el :: String -> Array Attr -> Array Markup -> Markup
el = Element

div :: Array Attr -> Array Markup -> Markup
div = Element "div"

span :: Array Attr -> Array Markup -> Markup
span = Element "span"

p :: Array Attr -> Array Markup -> Markup
p = Element "p"

ul :: Array Attr -> Array Markup -> Markup
ul = Element "ul"

li :: Array Attr -> Array Markup -> Markup
li = Element "li"

table :: Array Attr -> Array Markup -> Markup
table = Element "table"

tr :: Array Attr -> Array Markup -> Markup
tr = Element "tr"

th :: Array Attr -> Array Markup -> Markup
th = Element "th"

td :: Array Attr -> Array Markup -> Markup
td = Element "td"

strong :: Array Attr -> Array Markup -> Markup
strong = Element "strong"

em :: Array Attr -> Array Markup -> Markup
em = Element "em"

code :: Array Attr -> Array Markup -> Markup
code = Element "code"

blockquote :: Array Attr -> Array Markup -> Markup
blockquote = Element "blockquote"

img :: Array Attr -> Array Markup -> Markup
img = Element "img"

svg :: Array Attr -> Array Markup -> Markup
svg = Element "svg"

circle :: Array Attr -> Array Markup -> Markup
circle = Element "circle"

attr :: String -> String -> Attr
attr = Attr

cl :: String -> Attr
cl = Attr "class"

style :: String -> Attr
style = Attr "style"

dataKey :: String -> Attr
dataKey = Attr "data-key"

src :: String -> Attr
src = Attr "src"

alt :: String -> Attr
alt = Attr "alt"

title :: String -> Attr
title = Attr "title"

href :: String -> Attr
href = Attr "href"

id :: String -> Attr
id = Attr "id"

htmlNS :: String
htmlNS = "http://www.w3.org/1999/xhtml"

svgNS :: String
svgNS = "http://www.w3.org/2000/svg"

-- | Build a `Markup` fragment into a real DOM subtree, in the HTML namespace
-- | (an `svg` element switches its subtree to the SVG namespace).
buildNode :: Markup -> Effect Node
buildNode = go htmlNS

go :: String -> Markup -> Effect Node
go _ (Text s) = createTextNode s
go ns (Element tag attrs children) = do
  let ns' = if tag == "svg" then svgNS else ns
  node <- createElementNS ns' tag
  for_ attrs \(Attr name value) -> setAttribute node name value
  for_ children \child -> do
    childNode <- go ns' child
    appendChild childNode node
  pure node

-- | Replace a container node's children with a freshly built fragment, built
-- | in the given namespace (a `view` passes its container's child namespace).
buildInto :: String -> Node -> Array Markup -> Effect Unit
buildInto ns node children = do
  removeAllChildren node
  for_ children \child -> do
    childNode <- go ns child
    appendChild childNode node
