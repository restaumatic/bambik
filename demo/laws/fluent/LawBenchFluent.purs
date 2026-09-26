module LawBenchFluent (lawBenchFluent) where

import Prelude

import Effect (Effect)
import LawBench (bench, runBench)
import PUI (optional, required)
import PUI.Web (choice, staticText)
import PUI.Web.Fluent (body, button, dropdown, messageBar, progressBar, radioGroup, ratingDisplay, slider, textField, toggleSwitch)

lawBenchFluent :: Effect Unit
lawBenchFluent = do
  body (staticText "Leaf-law bench · Fluent")
  runBench
    [ bench "textField" "×→×" texts (textField @"Name" {})
    , bench "toggleSwitch" "×→×" flags (toggleSwitch @"On" {})
    , bench "slider" "×→×" quantities (slider @"Amount" {})
    , bench "ratingDisplay" "×→×" fractions (ratingDisplay @"Stars" _.fraction)
    , bench "progressBar" "×→×" fractions (progressBar @"Progress" _.fraction)
    , bench "dropdown required" "×→×" chosen (dropdown @"Pick" {} required options)
    , bench "dropdown optional" "×→×" optionals (dropdown @"Pick" {} (optional @"chosen") options)
    , bench "radioGroup required" "×→×" chosen (radioGroup @"Pick" {} required options)
    , bench "radioGroup optional" "×→×" optionals (radioGroup @"Pick" {} (optional @"chosen") options)
    , bench "button" "×→+" rows (button @"Go" {})
    , bench "messageBar" "+→×" events messageBar
    ]
  where
  texts = [ { "Name": "alpha", other: 1 }, { "Name": "beta", other: 2 } ]
  flags = [ { "On": true, other: 1 }, { "On": false, other: 2 } ]
  quantity current = { current, min: 0.0, max: 10.0, step: .discrete 1.0 }
  quantities = [ { "Amount": quantity 3.0, other: 1 }, { "Amount": quantity 7.0, other: 2 } ]
  fractions = [ { fraction: 0.25 }, { fraction: 0.75 } ]
  options = [ choice @"one", choice @"two" ] :: Array { value :: [ one :: {}, two :: {} ], label :: String }
  chosen = [ { "Pick": .one {} }, { "Pick": .two {} } ]
  optionals = [ { "Pick": .unchosen {} }, { "Pick": .chosen (.one {}) }, { "Pick": .chosen (.two {}) } ] :: Array { "Pick" :: [ chosen :: [ one :: {}, two :: {} ], unchosen :: {} ] }
  rows = [ { n: 1 }, { n: 2 } ]
  events = [ .event "hello", .event "world" ]
