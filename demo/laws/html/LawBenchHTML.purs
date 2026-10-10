module LawBenchHTML (lawBenchHTML) where

import Prelude hiding (div)

import Effect (Effect)
import LawBench (bench, runBench)
import PUI.Web ((<+>), choice, clicked, dynamic, each, inCase, inCaseAt, onClickedXY, provided, providedAt, shown, shownEach, shownWhen, shownWhenAt, staticText, text, textAt)
import PUI.Web.HTML (body, button, div, indeterminateLinearProgress, input, output, progress, rangeInput, select, selectOptional, selectUnpicked, textArea)

lawBenchHTML :: Effect Unit
lawBenchHTML = do
  body (staticText "Leaf-law bench · HTML")
  runBench
    [ bench "input" "×→×" texts (input @"Name" "text")
    , bench "textArea" "×→×" texts (textArea @"Name")
    , bench "rangeInput" "×→×" quantities (rangeInput @"Amount")
    , bench "progress" "×→×" fractions (progress fractionOf)
    , bench "indeterminateLinearProgress" "+→×" runs indeterminateLinearProgress
    , bench "select" "×→×" chosen (select @"Pick" options)
    , bench "selectUnpicked" "×→×" picks (selectUnpicked @"Pick" @"chosen" options)
    , bench "selectOptional" "×→×" picks (selectOptional @"Pick" @"chosen" @"unchosen" options)
    , bench "text" "×→×" titled (text titleOf)
    , bench "textAt" "×→×" titled (textAt @"title")
    , bench "dynamic" "×→×" titled (dynamic \r -> staticText r.title)
    , bench "each" "×→×" units (each [ "a", "b" ] staticText)
    , bench "shown" "×→×" titled (shown (text titleOf))
    , bench "shownWhen" "×→×" gated (shownWhen @"on" modeOf (textAt @"n"))
    , bench "shownWhenAt" "×→×" gated (shownWhenAt @"on" @"mode" (textAt @"n"))
    , bench "inCase" "×→×" gated (inCase @"on" modeOf (input @"Name" "text"))
    , bench "inCaseAt" "×→×" gated (inCaseAt @"on" @"mode" (input @"Name" "text"))
    , bench "shownEach" "×→×" lists (shownEach @"id" itemsOf (textAt @"title"))
    , bench "provided" "×→+" modes (provided @"on" modeOf (button @"Go" {}))
    , bench "providedAt" "×→+" modes (providedAt @"on" @"mode" (button @"Go" {}))
    , bench "button" "×→+" rows (button @"Go" {})
    , bench "clicked" "×→+" rows (clicked @"Go" @"n" (div (staticText "Go")))
    , bench "onClickedXY" "×→+" units (onClickedXY @"at" (div (staticText "canvas")))
    , bench "output" "+→×" events (output @"event" identity)
    ]
  where
  texts = [ { "Name": "alpha", other: 1 }, { "Name": "beta", other: 2 } ]
  quantity current = { current, min: 1.0, max: 10.0, step: .discrete 1.0 }
  quantities = [ { "Amount": quantity 3.0, other: 1 }, { "Amount": quantity 7.0, other: 2 } ]
  fractions = [ { fraction: 0.25 }, { fraction: 0.75 } ]
  runs = [ .started {}, .ended {} ]
  options = (choice @"one" <+> choice @"two") :: Array { value :: [ one :: {}, two :: {} ], label :: String }
  chosen = [ { "Pick": .one {} }, { "Pick": .two {} } ]
  picks = [ { "Pick": .unchosen {} }, { "Pick": .chosen (.one {}) }, { "Pick": .chosen (.two {}) } ] :: Array { "Pick" :: [ chosen :: [ one :: {}, two :: {} ], unchosen :: {} ] }
  titled = [ { title: "first", other: 1 }, { title: "second", other: 2 } ]
  units = [ {}, {} ]
  gated = [ { mode: .on { n: "shown" }, "Name": "alpha", other: 1 }, { mode: .off {}, "Name": "beta", other: 2 } ]
  modes = [ { mode: .on { n: "shown" } }, { mode: .off {} } ]
  lists = [ { items: [ { id: 1, title: "first" }, { id: 2, title: "second" } ], other: 1 }, { items: [ { id: 2, title: "second" } ], other: 2 } ]
  rows = [ { n: 1 }, { n: 2 } ]
  events = [ .event "hello", .event "world" ]
  fractionOf { fraction } = fraction

modeOf :: forall r1. { mode :: [ on :: { n :: String }, off :: {} ] | r1 } -> [ on :: { n :: String }, off :: {} ]
modeOf { mode } = mode

itemsOf :: forall r1. { items :: Array { id :: Int, title :: String } | r1 } -> Array { id :: Int, title :: String }
itemsOf { items } = items

titleOf :: forall r1. { title :: String | r1 } -> String
titleOf { title } = title
