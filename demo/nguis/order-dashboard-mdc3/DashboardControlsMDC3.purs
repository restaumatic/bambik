module DashboardControlsMDC3
  ( board
  , gauge
  , leaderboard
  , rangePicker
  , statTile
  , trendChart
  ) where

import Prelude (class Eq, identity, show, (#), ($), (*), (-), (/), (<>), (==), (>>>))

import Data.Array (foldl, length, mapWithIndex)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe)
import Data.Number (max)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row (class Cons, class Lacks)
import PUI (muted, Ocular, PUI, asField, atField, blank, foreach, projected)
import PUI.Web.HTML (attrWith, div, staticText, text, (:=))
import PUI.Web.MDC3 (displaySmall, labelLarge, labelMedium, linearProgress, list, listItem, segmentedButton)
import PUI.Web.SVG as SVG
import PUI.Web (OptCaption(..), Web)
import Type.Proxy (Proxy(..))
import ConvertableOptions (class ConvertOptionsWithDefaults, convertOptionsWithDefaults)

board :: Ocular (PUI Web)
board = div >>> "style" := "display: flex; flex-wrap: wrap; gap: 16px; align-items: stretch;"

statTile :: { label :: String, unit :: String } -> PUI Web { stat :: String } {}
statTile config =
  tile $ RecordToRecord.do
    labelMedium $ staticText config.label
    ( div >>> "style" := "display: flex; align-items: baseline; gap: 6px;" $ RecordToRecord.do
        displaySmall (text @"stat")
        labelMedium $ staticText config.unit )

gauge :: { label :: String } -> PUI Web { fraction :: Number } {}
gauge config =
  tile $ RecordToRecord.do
    labelMedium $ staticText config.label
    linearProgress @"fraction"
    labelLarge $ text @"percent" # projected percentText

trendChart :: { label :: String } -> PUI Web { trend :: Array Number } {}
trendChart config =
  tile $ RecordToRecord.do
    labelMedium $ staticText config.label
    SVG.svg >>> "viewBox" := "0 0 120 40" >>> "preserveAspectRatio" := "none" >>> "style" := "width: 100%; height: 40px;" $
      ( SVG.path >>> "fill" := "none" >>> "stroke" := "var(--md-sys-color-primary, #6750a4)" >>> "stroke-width" := "2"
          >>> "stroke-linejoin" := "round" >>> "vector-effect" := "non-scaling-stroke"
          >>> attrWith "d" sparkline $ blank )

leaderboard :: { label :: String } -> PUI Web { entries :: Array { name :: String, score :: String } } {}
leaderboard config =
  tile $ RecordToRecord.do
    labelMedium $ staticText config.label
    ( list ( ( listItem $ RecordToRecord.do
        text @"name"
        staticText " — "
        text @"score" ) # foreach @"name" identity ) # muted ) # atField @"entries"

rangePicker :: forall @l provided a ri ro. IsSymbol l => Lacks l () => Cons l (Maybe a) () ri => Cons l a () ro => Eq a => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> Array { value :: a, label :: String } -> PUI Web { | ri } { | ro }
rangePicker provided options =
  ( div >>> "style" := "display: flex; flex-direction: column; gap: 8px;" $ RecordToRecord.do
      labelMedium $ staticText config.label
      segmentedButton @"Picked" options ) # asField @"Picked" @l
  where
  config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided :: { label :: String }

tile :: Ocular (PUI Web)
tile = div >>> "style" := "display: flex; flex-direction: column; gap: 10px; padding: 16px; border: 1px solid var(--md-sys-color-outline-variant, #cac4d0); border-radius: 12px; background: var(--md-sys-color-surface-container-low, #f7f2fa); flex: 1 1 200px; min-width: 200px; box-sizing: border-box;"

percentText :: { fraction :: Number } -> String
percentText { fraction } = show (round (fraction * 100.0)) <> "%"

sparkline :: { trend :: Array Number } -> String
sparkline { trend } = case length trend of
  0 -> "M 0 38 L 120 38"
  1 -> "M 0 38 L 120 38"
  n ->
    let peak = foldl max 1.0 trend
        x i = 120.0 * toNumber i / toNumber (n - 1)
        y v = 38.0 - 36.0 * v / peak
    in joinWith " " (mapWithIndex (\i v -> (if i == 0 then "M " else "L ") <> fmt (x i) <> " " <> fmt (y v)) trend)

fmt :: Number -> String
fmt n = show (toNumber (round (n * 10.0)) / 10.0)
