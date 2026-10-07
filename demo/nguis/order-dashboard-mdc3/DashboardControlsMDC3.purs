module DashboardControlsMDC3
  ( board
  , gauge
  , leaderboard
  , rangePicker
  , statTile
  , trendChart
  ) where

import Prelude (otherwise, show, (#), ($), (*), (-), (/), (<), (<<<), (<>), (==), (>>>))

import ConvertableOptions (class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Array (foldl, length, mapWithIndex)
import Data.Int (round, toNumber)
import Data.Number (max)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row (class Cons)
import PUI (Ocular, PUI, blank, foreach, muted)
import PUI.Web (OptCaption(..), Web, attrWith, shown, staticText, staticText, text, (:=))
import PUI.Web.HTML (div)
import PUI.Web.MDC3 (displaySmall, labelLarge, labelMedium, linearProgress, list, listItem, segmentedButton)
import PUI.Web.SVG as SVG
import QualifiedDo.Semigroupoid as Semigroupoid
import Type.Proxy (Proxy(..))

board :: Ocular (PUI Web)
board = div >>> "style" := "display: flex; flex-wrap: wrap; gap: 16px; align-items: stretch;"

statTile :: forall r. String -> ({ | r } -> String) -> PUI Web { | r } {}
statTile l f =
  tile >>> "aria-label" := l $ Semigroupoid.do
    ( labelMedium $ staticText l ) # shown
    displaySmall (text f)

gauge :: forall r. String -> ({ | r } -> Number) -> PUI Web { | r } {}
gauge l f =
  tile $ ( Semigroupoid.do
    ( labelMedium $ staticText l ) # shown
    linearProgress f # shown
    ( labelLarge $ text (percentLine <<< f) ) # shown ) # muted

trendChart :: forall r. String -> ({ | r } -> Array Number) -> PUI Web { | r } {}
trendChart l f =
  tile >>> "aria-label" := l $ Semigroupoid.do
    ( labelMedium $ staticText l ) # shown
    SVG.svg >>> "viewBox" := "0 0 120 40" >>> "preserveAspectRatio" := "none" >>> "style" := "width: 100%; height: 40px;" $
      SVG.path >>> "fill" := "none" >>> "stroke" := "var(--md-sys-color-primary, #6750a4)" >>> "stroke-width" := "2"
        >>> "stroke-linejoin" := "round" >>> "vector-effect" := "non-scaling-stroke"
        >>> attrWith "d" (sparkline <<< f) $ blank

leaderboard :: forall r. String -> ({ | r } -> Array { name :: String, score :: String }) -> PUI Web { | r } {}
leaderboard l f =
  tile >>> "aria-label" := l $ Semigroupoid.do
    ( labelMedium $ staticText l ) # shown
    list ( ( listItem $ text entryLine ) # foreach @"name" f ) # muted

rangePicker :: forall @l provided a rest r. IsSymbol l => Cons l a rest r => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> Array { value :: a, label :: String } -> PUI Web { | r } { | r }
rangePicker provided options =
  div >>> "style" := "display: flex; flex-direction: column; gap: 8px;" $ Semigroupoid.do
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
