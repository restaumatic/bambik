module DashboardControlsMDC3
  ( board
  , gauge
  , leaderboard
  , rangePicker
  , statTile
  , trendChart
  ) where

import Prelude (class Eq, otherwise, show, (#), ($), (*), (-), (/), (<), (<<<), (<>), (==), (>>>))

import ConvertableOptions (class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Array (foldl, length, mapWithIndex)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe)
import Data.Number (max)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row (class Cons, class Union)
import PUI (Ocular, PUI, blank, foreach, muted)
import PUI.Web (OptCaption(..), Web)
import PUI.Web.HTML (attrWith, div, shown, staticText, text, (:=))
import PUI.Web.MDC3 (displaySmall, labelLarge, labelMedium, linearProgress, list, listItem, segmentedButton)
import PUI.Web.SVG as SVG
import QualifiedDo.Category as Category
import Type.Proxy (Proxy(..))

board :: Ocular (PUI Web)
board = div >>> "style" := "display: flex; flex-wrap: wrap; gap: 16px; align-items: stretch;"

statTile :: forall @l r. IsSymbol l => { unit :: String } -> ({ | r } -> String) -> PUI Web { | r } {}
statTile config f =
  tile >>> "aria-label" := reflectSymbol (Proxy @l) $ Category.do
    ( labelMedium $ staticText (reflectSymbol (Proxy @l)) ) # shown
    div >>> "style" := "display: flex; align-items: baseline; gap: 6px;" $ Category.do
      displaySmall (text f)
      labelMedium $ staticText config.unit

gauge :: forall @l r. IsSymbol l => Union r () r => ({ | r } -> Number) -> PUI Web { | r } {}
gauge f =
  tile $ ( Category.do
    ( labelMedium $ staticText (reflectSymbol (Proxy @l)) ) # shown
    linearProgress @l f # shown
    ( labelLarge $ text (percentLine <<< f) ) # shown ) # muted

trendChart :: forall @l r. IsSymbol l => ({ | r } -> Array Number) -> PUI Web { | r } {}
trendChart f =
  tile >>> "aria-label" := reflectSymbol (Proxy @l) $ Category.do
    ( labelMedium $ staticText (reflectSymbol (Proxy @l)) ) # shown
    SVG.svg >>> "viewBox" := "0 0 120 40" >>> "preserveAspectRatio" := "none" >>> "style" := "width: 100%; height: 40px;" $
      SVG.path >>> "fill" := "none" >>> "stroke" := "var(--md-sys-color-primary, #6750a4)" >>> "stroke-width" := "2"
        >>> "stroke-linejoin" := "round" >>> "vector-effect" := "non-scaling-stroke"
        >>> attrWith "d" (sparkline <<< f) $ blank

leaderboard :: forall @l r. IsSymbol l => ({ | r } -> Array { name :: String, score :: String }) -> PUI Web { | r } {}
leaderboard f =
  tile >>> "aria-label" := reflectSymbol (Proxy @l) $ Category.do
    ( labelMedium $ staticText (reflectSymbol (Proxy @l)) ) # shown
    list ( ( listItem $ text entryLine ) # foreach @"name" f ) # muted

rangePicker :: forall @l provided a ri ro. IsSymbol l => Cons l (Maybe a) () ri => Cons l a () ro => Eq a => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> Array { value :: a, label :: String } -> PUI Web { | ri } { | ro }
rangePicker provided options =
  div >>> "style" := "display: flex; flex-direction: column; gap: 8px;" $ Category.do
    ( labelMedium $ staticText config.label ) # shown
    segmentedButton @l options
  where
  config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided :: { label :: String }

percentLine :: Number -> String
percentLine fraction = show (round (fraction * 100.0)) <> "%"

entryLine :: { name :: String, score :: String } -> String
entryLine { name, score } = name <> " — " <> score

tile :: Ocular (PUI Web)
tile = div >>> "style" := "display: flex; flex-direction: column; gap: 10px; padding: 16px; border: 1px solid var(--md-sys-color-outline-variant, #cac4d0); border-radius: 12px; background: var(--md-sys-color-surface-container-low, #f7f2fa); flex: 1 1 200px; min-width: 200px; box-sizing: border-box;"

sparkline :: Array Number -> String
sparkline trend
  | length trend < 2 = "M 0 38 L 120 38"
  | otherwise =
    let n = length trend
        peak = foldl max 1.0 trend
        x i = 120.0 * toNumber i / toNumber (n - 1)
        y v = 38.0 - 36.0 * v / peak
    in joinWith " " (mapWithIndex (\i v -> (if i == 0 then "M " else "L ") <> fmt (x i) <> " " <> fmt (y v)) trend)

fmt :: Number -> String
fmt n = show (toNumber (round (n * 10.0)) / 10.0)
