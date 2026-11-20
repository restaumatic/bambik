-- | Diner's own components, over MDC3: what every station draws the same way.
module Diner.Design
  ( amountCell
  , amountHeader
  , controls
  , dense
  , entry
  , floor
  , glyph
  , heading
  , headline
  , legend
  , note
  , pager
  , pair
  , priceField
  , scrolling
  , shelf
  , station
  , surfaceTile
  , totalAmountCell
  , totalCell
  , tray
  ) where

import Prelude ((#), ($), (<>), (>>>))

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Symbol (class IsSymbol)
import PUI (fold, Ocular, PUI, stepped)
import PUI.Web (Web, attrWith, staticText, text, (:=))
import PUI.Web.HTML (div, span)
import PUI.Web.MDC3 (bodyLarge, columnHeader, dataCell, filledTextField, icon, iconButton', labelLarge, tile, titleMedium)
import Prim.Row (class Cons)
import QualifiedDo.Semigroupoid as Semigroupoid

-- | The pane a station draws in: a column no wider than a table reads
-- | well, its parts as wide as the column and set apart from each other
-- | and from the tab bar above.
station :: Ocular (PUI Web)
station = div >>> "style" := "max-width: 960px; margin: 0 auto; padding-top: 16px; display: flex; flex-direction: column; gap: 16px;"

-- | The hall's tables side by side, as many to a row as fit.
floor :: Ocular (PUI Web)
floor = div >>> "style" := "display: grid; grid-template-columns: repeat(auto-fill, minmax(40px, 1fr)); gap: 4px;"

-- | A row's buttons side by side, whatever stands between them.
controls :: Ocular (PUI Web)
controls = div >>> "style" := "display: flex; align-items: center;"

-- | What a colour or a sign stands for, entry after entry on as many lines
-- | as they need; an entry is a patch and its name.
legend :: Ocular (PUI Web)
legend = div >>> "style" := "display: flex; flex-wrap: wrap; align-items: center; gap: 4px 16px;"

entry :: Ocular (PUI Web)
entry = span >>> "style" := "display: inline-flex; align-items: center; gap: 6px;"

-- | A small sign beside or under a word.
glyph :: forall r. String -> PUI Web { | r } { | r }
glyph name = icon name # ("style" := "--md-icon-size: 16px;")

-- | A card's head on one line: what it is at one end, how it stands at the
-- | other.
headline :: Ocular (PUI Web)
headline = div >>> "style" := "display: flex; align-items: baseline; justify-content: space-between; gap: 8px; align-self: stretch;"

-- | A list drawn tight: short rows, no room above or below.
dense :: Ocular (PUI Web)
dense = "style" := "--md-list-item-one-line-container-height: 40px; --md-list-item-top-space: 4px; --md-list-item-bottom-space: 4px; padding: 0;"

-- | A box of one height for content that grows and shrinks, so what stands
-- | under it stays where it is; the part of it marked `scrolling` takes the
-- | room the rest leaves and scrolls inside.
tray :: Ocular (PUI Web)
tray = div >>> "style" := "height: 256px; display: flex; flex-direction: column; gap: 8px;"

scrolling :: Ocular (PUI Web)
scrolling = "style" := "flex: 1 1 0; min-height: 0; overflow-y: auto;"

-- | Tiles side by side, as many to a row as fit at a readable width, each
-- | row as tall as its tallest tile.
shelf :: Ocular (PUI Web)
shelf = div >>> "style" := "display: grid; grid-template-columns: repeat(auto-fill, minmax(150px, 1fr)); gap: 8px;"

-- | A tile to tap, as wide as its cell: its content stacked on a tinted
-- | surface, reporting field `k` of its row as case `l`. While `disabled`
-- | holds for the row it shows, it is drawn dimmed and takes no tap; while
-- | `marked` holds, its surface is the colour of something picked.
surfaceTile
  :: forall @l @k r o b1 key v
   . IsSymbol l
  => IsSymbol k
  => Cons k key b1 r
  => Cons l key () v
  => { disabled :: { | r } -> Boolean, marked :: { | r } -> Boolean }
  -> PUI Web { | r } o
  -> PUI Web { | r } [ | v ]
surfaceTile { disabled, marked } content = ( tile @l @k $ surface $ content ) # attrWith "aria-disabled" flag # attrWith "style" face
  where
  surface = div >>> "style" :=
    ( "height: 100%; box-sizing: border-box; display: flex; flex-direction: column; justify-content: center; gap: 2px; "
        <> "padding: 8px 12px; min-height: 48px; text-align: left;"
    )
  flag row = if disabled row then "true" else "false"
  face row =
    "border-radius: 12px; background: "
      <> (if marked row then "#cfe4ff; " else "var(--md-sys-color-surface-container-high, #ece6f0); ")
      <> (if disabled row then "opacity: 0.38; pointer-events: none;" else "")

-- | A price to type, in złoty: field `l` holds it as written, for the
-- | business to read.
priceField :: forall @l r b. IsSymbol l => Cons l String b r => PUI Web { | r } { | r }
priceField = filledTextField @l # ("suffix-text" := "zł") # ("inputmode" := "decimal")

-- | Two fields side by side under what they belong to, sharing the width
-- | they are given.
pair :: Ocular (PUI Web)
pair = div >>> "style" := "display: grid; grid-template-columns: minmax(0, 1fr) minmax(0, 1fr); gap: 8px; margin-top: 4px;"

-- | The row under a list that says where its page stands and turns it:
-- | `line` is what it says, `isFirst` and `isLast` hold while there is no
-- | page before or after, and `previous` and `next` are the rows a turn
-- | back or on leaves.
pager
  :: forall r
   . ({ | r } -> String)
  -> { isFirst :: { | r } -> Boolean
     , isLast :: { | r } -> Boolean
     , next :: { | r } -> { | r }
     , previous :: { | r } -> { | r }
     }
  -> PUI Web { | r } { | r }
pager line { isFirst, isLast, next, previous } =
  div >>> "style" := "display: flex; align-items: center; justify-content: flex-end; gap: 4px;" $ Semigroupoid.do
    labelLarge $ text line
    ( Semigroupoid.do
      RecordToVariant.do
        iconButton' @"Poprzednia strona" "chevron_left" { disabled: isFirst }
        iconButton' @"Następna strona" "chevron_right" { disabled: isLast }
      VariantToRecord.do
        fold @"Poprzednia strona" previous
        fold @"Następna strona" next ) # stepped

-- | What a station shows where it has nothing to list.
note :: forall r. String -> PUI Web { | r } { | r }
note copy = bodyLarge >>> "style" := "margin: 0;" $ staticText copy

-- | What a station is showing, over the part it heads.
heading :: forall r. ({ | r } -> String) -> PUI Web { | r } { | r }
heading copy = titleMedium >>> "style" := "margin: 0;" $ text copy

-- | A column of amounts: its heading and its cells set against the right
-- | edge, so the amounts line up by their grosze.
amountHeader :: forall r. String -> PUI Web { | r } { | r }
amountHeader title = columnHeader title # ("style" := "text-align: right;")

amountCell :: Ocular (PUI Web)
amountCell = dataCell >>> "style" := "text-align: right;"

-- | A total in the row that sums a column up, and one in a column of
-- | amounts.
totalCell :: forall r. ({ | r } -> String) -> PUI Web { | r } { | r }
totalCell copy = dataCell $ labelLarge $ text copy

totalAmountCell :: forall r. ({ | r } -> String) -> PUI Web { | r } { | r }
totalAmountCell copy = amountCell $ labelLarge $ text copy
