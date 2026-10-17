module LawBenchFluent (lawBenchFluent) where

import Prelude

import Effect (Effect)
import LawBench (bench, runBench)
import PUI.Web (text, (<+>), choice, staticText)
import PUI.Web.Fluent (acknowledged, confirmed, body, button, dropdown, dropdownOptional, dropdownUnpicked, indeterminateLinearProgress, messageBar, progressBar, radioGroup, radioGroupOptional, radioGroupUnpicked, ratingDisplay, slider, textField, toggleSwitch)

lawBenchFluent :: Effect Unit
lawBenchFluent = do
  body (staticText "Leaf-law bench · Fluent")
  runBench
    [ bench "textField" "×→×" texts (textField @"Name")
    , bench "toggleSwitch" "×→×" flags (toggleSwitch @"On")
    , bench "slider" "×→×" quantities (slider @"Amount")
    , bench "ratingDisplay" "×→×" fractions (ratingDisplay fractionOf)
    , bench "progressBar" "×→×" fractions (progressBar fractionOf)
    , bench "indeterminateLinearProgress" "+→×" runs indeterminateLinearProgress
    , bench "dropdown" "×→×" chosen (dropdown @"Pick" options)
    , bench "dropdownUnpicked" "×→×" picks (dropdownUnpicked @"Pick" @"chosen" options)
    , bench "dropdownOptional" "×→×" picks (dropdownOptional @"Pick" @"chosen" @"unchosen" options)
    , bench "radioGroup" "×→×" chosen (radioGroup @"Pick" options)
    , bench "radioGroupUnpicked" "×→×" picks (radioGroupUnpicked @"Pick" @"chosen" options)
    , bench "radioGroupOptional" "×→×" picks (radioGroupOptional @"Pick" @"chosen" @"unchosen" options)
    , bench "button" "×→+" rows (button @"Go")
    , bench "messageBar" "+→×" events (messageBar @"event" (const {}))
    -- the dialogs last: a modal open traps the page's focus, so nothing is typed after them
    , bench "confirmed" "+→+" decisions (confirmed @"decision" "Confirm" (text nLine))
    , bench "acknowledged" "+→+" decisions (acknowledged @"decision" "Got it" (text nLine))
    ]
  where
  fractionOf { fraction } = fraction
  texts = [ { "Name": "alpha", other: 1 }, { "Name": "beta", other: 2 } ]
  flags = [ { "On": true, other: 1 }, { "On": false, other: 2 } ]
  quantity current = { current, min: 1.0, max: 10.0, step: .discrete 1.0 }
  quantities = [ { "Amount": quantity 3.0, other: 1 }, { "Amount": quantity 7.0, other: 2 } ]
  fractions = [ { fraction: 0.25 }, { fraction: 0.75 } ]
  runs = [ .started {}, .ended {} ]
  options = choice @"one" <+> choice @"two"
  chosen = [ { "Pick": .one {} }, { "Pick": .two {} } ]
  picks = [ { "Pick": .unchosen {} }, { "Pick": .chosen (.one {}) }, { "Pick": .chosen (.two {}) } ] :: Array { "Pick" :: [ chosen :: [ one :: {}, two :: {} ], unchosen :: {} ] }
  rows = [ { n: 1 }, { n: 2 } ]
  decisions = [ .decision { n: 1 }, .decision { n: 2 } ] :: Array [ decision :: { n :: Int } ]
  nLine { n } = "n = " <> show n
  events = [ .event "hello", .event "world" ]
