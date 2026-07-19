-- | SVG element oculars. `element` (via `el`) is namespace-aware: `svg` opens
-- | the SVG namespace and its children inherit it, so `circle`/`path`/`text`
-- | used inside an `svg` are created as SVG-namespaced nodes. `text` lives here
-- | rather than in `PUI.HTML` because that name is the channel-fed text leaf
-- | there; import this module qualified (`import PUI.SVG as SVG`) when a widget
-- | needs both the HTML `text` leaf and the SVG `<text>` element.
module PUI.SVG
  ( circle
  , path
  , svg
  , text
  ) where

import Data.Lens.Extra.Types (Ocular)
import PUI (PUI)
import PUI.HTML (el)
import PUI.Web (Web)

svg :: Ocular (PUI Web)
svg = el "svg"

circle :: Ocular (PUI Web)
circle = el "circle"

path :: Ocular (PUI Web)
path = el "path"

text :: Ocular (PUI Web)
text = el "text"
