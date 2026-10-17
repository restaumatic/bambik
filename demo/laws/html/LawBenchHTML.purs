module LawBenchHTML (lawBenchHTML) where

import Prelude hiding (div)

import Effect (Effect)
import LawBench (bench, runBench)
import PUI.Web ((<+>), choice, clock, colorScheme, dynamic, each, geolocated, inCase, inCaseAt, onClickedXY, onContextMenuXY, onHoveredXY, online, provided, providedAt, staticText, text, textAt)
import PUI.Web.HTML (acknowledged, confirmed, body, button, div, filePicked, inPlaceInput, indeterminateLinearProgress, input, output, progress, rangeInput, select, selectOptional, selectUnpicked, textArea)

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
    , bench "inCase" "×→×" gated (inCase @"on" modeOf (input @"Name" "text"))
    , bench "inCaseAt" "×→×" gated (inCaseAt @"on" @"mode" (input @"Name" "text"))
    , bench "inCase display" "×→×" gated (inCase @"on" modeOf (textAt @"Name"))
    , bench "inCaseAt display" "×→×" gated (inCaseAt @"on" @"mode" (textAt @"Name"))
    , bench "provided" "×→+" modes (provided @"on" modeOf (button @"Go"))
    , bench "providedAt" "×→+" modes (providedAt @"on" @"mode" (button @"Go"))
    , bench "button" "×→+" rows (button @"Go")
    , bench "onClickedXY" "×→+" units (onClickedXY @"at" (div (staticText "canvas")))
    , bench "onHoveredXY" "×→+" units (onHoveredXY @"at" (div (staticText "canvas")))
    , bench "onContextMenuXY" "×→+" units (onContextMenuXY @"at" (div (staticText "canvas")))
    , bench "output" "+→×" events (output @"event" (const {}))
    , bench "geolocated" "×→×" units (geolocated @"position")
    , bench "online" "×→×" units (online @"connection")
    , bench "clock" "×→×" units (clock @"time" { ms: 60000.0 })
    , bench "colorScheme" "×→×" units (colorScheme @"scheme")
    , bench "filePicked" "×→×" units (filePicked @"Photo")
    , bench "inPlaceInput" "×→+" texts (inPlaceInput @"entered" @"Name")
    -- the dialogs last: a modal open traps the page's focus, so nothing is typed after them
    , bench "confirmed" "+→+" decisions (confirmed @"decision" (text nLine))
    , bench "acknowledged" "+→+" decisions (acknowledged @"decision" (text nLine))
    ]
  where
  texts = [ { "Name": "alpha", other: 1 }, { "Name": "beta", other: 2 } ]
  quantity current = { current, min: 1.0, max: 10.0, step: .discrete 1.0 }
  quantities = [ { "Amount": quantity 3.0, other: 1 }, { "Amount": quantity 7.0, other: 2 } ]
  fractions = [ { fraction: 0.25 }, { fraction: 0.75 } ]
  runs = [ .started {}, .ended {} ]
  options = choice @"one" <+> choice @"two"
  chosen = [ { "Pick": .one {} }, { "Pick": .two {} } ]
  picks = [ { "Pick": .unchosen {} }, { "Pick": .chosen (.one {}) }, { "Pick": .chosen (.two {}) } ] :: Array { "Pick" :: [ chosen :: [ one :: {}, two :: {} ], unchosen :: {} ] }
  titled = [ { title: "first", other: 1 }, { title: "second", other: 2 } ]
  units = [ {}, {} ]
  gated = [ { mode: .on { n: "shown" }, "Name": "alpha", other: 1 }, { mode: .off {}, "Name": "beta", other: 2 } ]
  modes = [ { mode: .on { n: "shown" } }, { mode: .off {} } ]
  rows = [ { n: 1 }, { n: 2 } ]
  decisions = [ .decision { n: 1 }, .decision { n: 2 } ] :: Array [ decision :: { n :: Int } ]
  nLine { n } = "n = " <> show n
  events = [ .event "hello", .event "world" ]
  fractionOf { fraction } = fraction

modeOf :: forall r1. { mode :: [ on :: { n :: String }, off :: {} ] | r1 } -> [ on :: { n :: String }, off :: {} ]
modeOf { mode } = mode

titleOf :: forall r1. { title :: String | r1 } -> String
titleOf { title } = title
