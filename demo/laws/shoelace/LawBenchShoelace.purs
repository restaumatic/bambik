module LawBenchShoelace (lawBenchShoelace) where

import Prelude

import Effect (Effect)
import LawBench (bench, runBench)
import PUI.Web (choice, staticText)
import PUI.Web.Shoelace (body, button, progressBar, rating, select, selectOptional, selectUnpicked, sliderLive, textArea, textField, toast, toggleSwitch)

lawBenchShoelace :: Effect Unit
lawBenchShoelace = do
  body (staticText "Leaf-law bench · Shoelace")
  runBench
    [ bench "textField" "×→×" texts (textField @"Name" {})
    , bench "textArea" "×→×" texts (textArea @"Name" { rows: 2 })
    , bench "toggleSwitch" "×→×" flags (toggleSwitch @"On" {})
    , bench "sliderLive" "×→×" quantities (sliderLive @"Amount" {})
    , bench "rating" "×→×" ratings (rating @"Stars" {})
    , bench "progressBar" "×→×" fractions (progressBar @"Progress" _.fraction)
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
  ratings = [ { "Stars": { current: 2.0, max: 5 }, other: 1 }, { "Stars": { current: 4.0, max: 5 }, other: 2 } ]
  fractions = [ { fraction: 0.25 }, { fraction: 0.75 } ]
  options = [ choice @"one", choice @"two" ] :: Array { value :: [ one :: {}, two :: {} ], label :: String }
  chosen = [ { "Pick": .one {} }, { "Pick": .two {} } ]
  picks = [ { "Pick": .unchosen {} }, { "Pick": .chosen (.one {}) }, { "Pick": .chosen (.two {}) } ] :: Array { "Pick" :: [ chosen :: [ one :: {}, two :: {} ], unchosen :: {} ] }
  rows = [ { n: 1 }, { n: 2 } ]
  events = [ .event "hello", .event "world" ]
