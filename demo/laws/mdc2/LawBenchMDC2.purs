module LawBenchMDC2 (lawBenchMDC2) where

import Prelude

import Effect (Effect)
import LawBench (bench, runBench)
import PUI (optional, required)
import PUI.Web (choice, staticText, text)
import PUI.Web.MDC2 (body, button, checkbox, debouncedTextField, fab, filledTextArea, filledTextField, filterChip, group, iconButton, iconToggle, imagePane, indeterminateCircularProgress, indeterminateLinearProgress, linearProgress, listOf, menuItem, outlinedButton, outlinedTextField, radioButton, segmentedButton, select, slider, sliderLive, snackbar, tabBar, textButton, banner, toggleSwitch)

lawBenchMDC2 :: Effect Unit
lawBenchMDC2 = do
  body (staticText "Leaf-law bench · MDC2")
  runBench
    [ bench "filledTextField" "×→×" texts (filledTextField @"Name" {})
    , bench "outlinedTextField" "×→×" texts (outlinedTextField @"Name" {})
    , bench "debouncedTextField" "×→×" texts (debouncedTextField @"Name" { ms: 200.0 })
    , bench "filledTextArea" "×→×" texts (filledTextArea @"Name" { columns: 20, rows: 2 })
    , bench "checkbox" "×→×" ticks (checkbox @"Terms" @"accepted" @"declined" { ticked: {} } (staticText "I accept"))
    , bench "toggleSwitch" "×→×" flags (toggleSwitch @"On" {})
    , bench "filterChip" "×→×" flags (filterChip @"On" {})
    , bench "iconToggle" "×→×" flags (iconToggle @"On" { onIcon: "star", offIcon: "star_border" })
    , bench "slider" "×→×" quantities (slider @"Amount" {})
    , bench "sliderLive" "×→×" quantities (sliderLive @"Amount" {})
    , bench "tabBar" "×→×" tabs (tabBar @"Tab" [ { value: tabA, label: "A" }, { value: tabB, label: "B" } ])
    , bench "linearProgress" "×→×" fractions (linearProgress @"Progress" _.fraction)
    , bench "imagePane" "×→×" images imagePane
    , bench "group" "×→×" grouped (group @"Customer" (filledTextField @"Name" {}))
    , bench "select required" "×→×" chosen (select @"Pick" {} required options)
    , bench "select optional" "×→×" optionals (select @"Pick" {} (optional @"chosen") options)
    , bench "radioButton required" "×→×" chosen (radioButton @"Pick" required options)
    , bench "radioButton optional" "×→×" optionals (radioButton @"Pick" (optional @"chosen") options)
    , bench "segmentedButton required" "×→×" chosen (segmentedButton @"Pick" required options)
    , bench "segmentedButton optional" "×→×" optionals (segmentedButton @"Pick" (optional @"chosen") options)
    , bench "button" "×→+" rows (button @"Go" {})
    , bench "outlinedButton" "×→+" rows (outlinedButton @"Go" {})
    , bench "textButton" "×→+" rows (textButton @"Go" {})
    , bench "fab" "×→+" rows (fab @"Go" { icon: "add" })
    , bench "iconButton" "×→+" rows (iconButton @"Go" { icon: "add" })
    , bench "menuItem" "×→+" rows (menuItem @"Go" {})
    , bench "listOf" "×→+" lists (listOf @"picked" _.id {} _.items (text _.title))
    , bench "snackbar" "+→×" events snackbar
    , bench "banner" "+→×" events banner
    , bench "indeterminateLinearProgress" "+→×" runs (indeterminateLinearProgress @"Loading")
    , bench "indeterminateCircularProgress" "+→×" runs (indeterminateCircularProgress @"Loading")
    ]
  where
  texts = [ { "Name": "alpha", other: 1 }, { "Name": "beta", other: 2 } ]
  ticks = [ { "Terms": .accepted {}, other: 1 }, { "Terms": .declined {}, other: 2 } ]
  flags = [ { "On": true, other: 1 }, { "On": false, other: 2 } ]
  quantity current = { current, min: 0.0, max: 10.0, step: .discrete 1.0 }
  quantities = [ { "Amount": quantity 3.0, other: 1 }, { "Amount": quantity 7.0, other: 2 } ]
  tabA = .a {} :: [ a :: {}, b :: {} ]
  tabB = .b {} :: [ a :: {}, b :: {} ]
  tabs = [ { "Tab": tabA, other: 1 }, { "Tab": tabB, other: 2 } ]
  fractions = [ { fraction: 0.25 }, { fraction: 0.75 } ]
  images = [ { src: "a.png", label: "A" }, { src: "b.png", label: "B" } ]
  grouped = [ { "Customer": { "Name": "alpha" }, other: 1 }, { "Customer": { "Name": "beta" }, other: 2 } ]
  options = [ choice @"one", choice @"two" ] :: Array { value :: [ one :: {}, two :: {} ], label :: String }
  chosen = [ { "Pick": .one {} }, { "Pick": .two {} } ]
  optionals = [ { "Pick": .unchosen {} }, { "Pick": .chosen (.one {}) }, { "Pick": .chosen (.two {}) } ] :: Array { "Pick" :: [ chosen :: [ one :: {}, two :: {} ], unchosen :: {} ] }
  rows = [ { n: 1 }, { n: 2 } ]
  lists = [ { items: [ { id: 1, title: "first" }, { id: 2, title: "second" } ] }, { items: [ { id: 2, title: "second" } ] } ]
  events = [ .event "hello", .event "world" ]
  runs = [ .started {}, .ended {} ]
