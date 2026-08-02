module DashboardControlsMDC3
  ( board
  , gauge
  , leaderboard
  , rangePicker
  , statTile
  , trendChart
  ) where

import Prelude (identity, class Eq, const, show, ($), (#), (*), (-), (/), (<>), (==), (>>>))

import Data.Array (foldl, length, mapWithIndex)
import Data.Int (round, toNumber)
import Data.Lens.Extra.Types (Ocular)
import Data.Maybe (Maybe)
import Data.Number (max)
import Data.Profunctor.Row.RecordToRecord (pempty)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.String (joinWith)
import PUI (PUI, constantly, displayed, forField, forValue, foreach, projected)
import PUI.HTML (attrWith, div, staticText, text, (:=))
import PUI.MDC3 (displaySmall, labelLarge, labelMedium, linearProgress, list, listItem, segmentedButton)
import PUI.SVG as SVG
import PUI.Web (Web)

board :: Ocular (PUI Web)
board = div >>> "style" := "display: flex; flex-wrap: wrap; gap: 16px; align-items: stretch;"

statTile :: { label :: String, unit :: String } -> PUI Web { value :: String } {}
statTile config =
  tile $ RecordToRecord.do
    labelMedium $ staticText config.label
    ( div >>> "style" := "display: flex; align-items: baseline; gap: 6px;" $ RecordToRecord.do
        displaySmall text
        labelMedium $ staticText config.unit )

gauge :: { label :: String } -> PUI Web { value :: Number } {}
gauge config =
  tile $ RecordToRecord.do
    labelMedium $ staticText config.label
    linearProgress
    labelLarge $ text # projected percentText

trendChart :: { label :: String } -> PUI Web { value :: Array Number } {}
trendChart config =
  tile $ RecordToRecord.do
    labelMedium $ staticText config.label
    SVG.svg >>> "viewBox" := "0 0 120 40" >>> "preserveAspectRatio" := "none" >>> "style" := "width: 100%; height: 40px;" $
      ( SVG.path >>> "fill" := "none" >>> "stroke" := "var(--md-sys-color-primary, #6750a4)" >>> "stroke-width" := "2"
          >>> "stroke-linejoin" := "round" >>> "vector-effect" := "non-scaling-stroke"
          >>> attrWith "d" sparkline $ pempty # constantly {} )

leaderboard :: { label :: String } -> PUI Web { value :: Array { name :: String, score :: String } } { value :: Array { name :: String, score :: String } }
leaderboard config =
  tile $ RecordToRecord.do
    labelMedium $ staticText config.label
    list ( ( listItem $ RecordToRecord.do
               text # forValue # forField @"name"
               staticText " — "
               text # forValue # forField @"score"
           ) # foreach @"name" identity ) # forField @"value" # displayed

rangePicker :: forall a. Eq a => { label :: String } -> Array { value :: a, label :: String } -> PUI Web { value :: Maybe a } { value :: a }
rangePicker config options =
  div >>> "style" := "display: flex; flex-direction: column; gap: 8px;" $ RecordToRecord.do
    labelMedium $ staticText config.label
    segmentedButton options

tile :: Ocular (PUI Web)
tile = div >>> "style" := "display: flex; flex-direction: column; gap: 10px; padding: 16px; border: 1px solid var(--md-sys-color-outline-variant, #cac4d0); border-radius: 12px; background: var(--md-sys-color-surface-container-low, #f7f2fa); flex: 1 1 200px; min-width: 200px; box-sizing: border-box;"

percentText :: { value :: Number } -> String
percentText { value } = show (round (value * 100.0)) <> "%"

sparkline :: { value :: Array Number } -> String
sparkline { value } = case length value of
  0 -> "M 0 38 L 120 38"
  1 -> "M 0 38 L 120 38"
  n ->
    let peak = foldl max 1.0 value
        x i = 120.0 * toNumber i / toNumber (n - 1)
        y v = 38.0 - 36.0 * v / peak
    in joinWith " " (mapWithIndex (\i v -> (if i == 0 then "M " else "L ") <> fmt (x i) <> " " <> fmt (y v)) value)

fmt :: Number -> String
fmt n = show (toNumber (round (n * 10.0)) / 10.0)
