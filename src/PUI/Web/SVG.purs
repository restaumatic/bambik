-- | The drawing vocabulary: shapes for charts, diagrams and canvases, used
-- | exactly like the `PUI.Web.HTML` elements — decorated with `attr`/`:=` for
-- | anything fixed and `attrWith` for anything that follows the data, so a
-- | sparkline or a scatter of circles is drawn once and moved in place as
-- | values arrive.
-- |
-- | Import it qualified (`import PUI.Web.SVG as SVG`) when a widget needs both
-- | the SVG `<text>` shape and the HTML `text` leaf that shows a string.
module PUI.Web.SVG
  ( circle
  , path
  , svg
  , text
  ) where

import Data.Lens.Extra.Types (Ocular)
import PUI (PUI)
import PUI.Web.HTML (el)
import PUI.Web (Web)

-- | The drawing surface everything else goes inside. Its `viewBox` sets the
-- | coordinate system the shapes are written in — and the coordinates
-- | `onClickedXY` reports back, so a drawing and the gestures on it speak
-- | the same units regardless of how large it is on screen.
svg :: Ocular (PUI Web)
svg = el "svg"

-- | A circle, placed by `cx`/`cy` and sized by `r` — the point of a scatter
-- | plot, a node of a diagram, a drawn handle.
circle :: Ocular (PUI Web)
circle = el "circle"

-- | An arbitrary outline, given by the `d` attribute — the line of a chart,
-- | an arc, an icon. `attrWith "d"` redraws it from the data.
path :: Ocular (PUI Web)
path = el "path"

-- | Text inside a drawing, placed by `x`/`y`: an axis tick, a data label.
-- | (For text in a page, `PUI.Web.HTML`'s `text` and `staticText` are the
-- | leaves.)
text :: Ocular (PUI Web)
text = el "text"
