module LawBenchBootstrap (lawBenchBootstrap) where

import Prelude

import Effect (Effect)
import LawBench (bench, runBench)
import PUI.Web ((<+>), choice, staticText)
import PUI.Web.Bootstrap (body, button, indeterminateLinearProgress, progress, select, selectOptional, selectUnpicked, sliderLive, textField, toast, toggleSwitch)

lawBenchBootstrap :: Effect Unit
lawBenchBootstrap = do
  body (staticText @"Leaf-law bench · Bootstrap")
  runBench
    [ bench "textField" "×→×" texts (textField @"Name" {})
    , bench "toggleSwitch" "×→×" flags (toggleSwitch @"On" {})
    , bench "sliderLive" "×→×" quantities (sliderLive @"Amount" {})
    , bench "progress" "×→×" fractions (progress @"Progress" _.fraction)
    , bench "indeterminateLinearProgress" "+→×" runs indeterminateLinearProgress
    , bench "select" "×→×" chosen (select @"Pick" {} options)
    , bench "selectUnpicked" "×→×" picks (selectUnpicked @"Pick" @"chosen" {} options)
    , bench "selectOptional" "×→×" picks (selectOptional @"Pick" @"chosen" @"unchosen" {} options)
    , bench "button" "×→+" rows (button @"Go" {})
    , bench "toast" "+→×" events (toast @"event" identity)
    ]
  where
  texts = [ { "Name": "alpha", other: 1 }, { "Name": "beta", other: 2 } ]
  flags = [ { "On": true, other: 1 }, { "On": false, other: 2 } ]
  quantity current = { current, min: 1.0, max: 10.0, step: .discrete 1.0 }
  quantities = [ { "Amount": quantity 3.0, other: 1 }, { "Amount": quantity 7.0, other: 2 } ]
  fractions = [ { fraction: 0.25 }, { fraction: 0.75 } ]
  runs = [ .started {}, .ended {} ]
  options = (choice @"one" <+> choice @"two") :: Array { value :: [ one :: {}, two :: {} ], label :: String }
  chosen = [ { "Pick": .one {} }, { "Pick": .two {} } ]
  picks = [ { "Pick": .unchosen {} }, { "Pick": .chosen (.one {}) }, { "Pick": .chosen (.two {}) } ] :: Array { "Pick" :: [ chosen :: [ one :: {}, two :: {} ], unchosen :: {} ] }
  rows = [ { n: 1 }, { n: 2 } ]
  events = [ .event "hello", .event "world" ]
