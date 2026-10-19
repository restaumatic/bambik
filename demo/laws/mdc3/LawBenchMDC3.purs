module LawBenchMDC3 (lawBenchMDC3) where

import Prelude

import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import PUI (PUI)
import LawBench (bench, runBench)
import PUI.Web (Web, text, (<+>), choice, staticText, textAt)
import PUI.Web.MDC3 (acknowledged, body, confirmed, button, checkbox, debouncedTextField, doubleClickTile, elevatedButton, fab, filledTextArea, filledTextField, filterChip, group, iconButton, iconToggle, imagePane, indeterminateCircularProgress, indeterminateLinearProgress, linearProgress, listOf, listOfAt, menuItem, option, outlinedButton, radio, radioButtonV, radioButtonUnpickedV, radioButtonOptionalV, segment, segmentedButtonV, segmentedButtonUnpickedV, segmentedButtonOptionalV, selectV, selectUnpickedV, selectOptionalV, tab, tabBarV, outlinedTextField, radioButton, radioButtonOptional, radioButtonUnpicked, segmentedButton, segmentedButtonOptional, segmentedButtonUnpicked, select, selectOptional, selectUnpicked, slider, sliderLive, snackbar, tabBar, tabBar', textButton, tile, toggleSwitch, tonalButton)

lawBenchMDC3 :: Effect Unit
lawBenchMDC3 = do
  body (staticText "Leaf-law bench · MDC3")
  runBench
    [ bench "filledTextField" "×→×" texts (filledTextField @"Name")
    , bench "outlinedTextField" "×→×" texts (outlinedTextField @"Name")
    , bench "debouncedTextField" "×→×" texts (debouncedTextField @"Name" { ms: 200.0 })
    , bench "filledTextArea" "×→×" texts (filledTextArea @"Name" { columns: 20, rows: 2 })
    , bench "checkbox" "×→×" ticks (checkbox @"Terms" @"accepted" @"declined")
    , bench "toggleSwitch" "×→×" flags (toggleSwitch @"On")
    , bench "filterChip" "×→×" flags (filterChip @"On")
    , bench "iconToggle" "×→×" flags (iconToggle @"On" { onIcon: "star", offIcon: "star_border" })
    , bench "slider" "×→×" quantities (slider @"Amount")
    , bench "sliderLive" "×→×" quantities (sliderLive @"Amount")
    , bench "tabBar" "×→×" tabs (tabBar @"Tab" (choice @"a" <+> choice @"b"))
    , bench "tabBar'" "×→×" tabs (tabBar' @"Tab" { icons: { a: "star", b: "home" } } (choice @"a" <+> choice @"b"))
    , bench "linearProgress" "×→×" fractions (linearProgress fractionOf)
    , bench "imagePane" "×→×" images (imagePane identity)
    , bench "group" "×→×" grouped (group @"Customer" (filledTextField @"Name"))
    , bench "select" "×→×" chosen (select @"Pick" options)
    , bench "selectUnpicked" "×→×" picks (selectUnpicked @"Pick" @"chosen" options)
    , bench "selectOptional" "×→×" picks (selectOptional @"Pick" @"chosen" @"unchosen" options)
    , bench "radioButton" "×→×" chosen (radioButton @"Pick" options)
    , bench "radioButtonUnpicked" "×→×" picks (radioButtonUnpicked @"Pick" @"chosen" options)
    , bench "radioButtonOptional" "×→×" picks (radioButtonOptional @"Pick" @"chosen" @"unchosen" options)
    , bench "segmentedButton" "×→×" chosen (segmentedButton @"Pick" options)
    , bench "segmentedButtonUnpicked" "×→×" picks (segmentedButtonUnpicked @"Pick" @"chosen" options)
    , bench "segmentedButtonOptional" "×→×" picks (segmentedButtonOptional @"Pick" @"chosen" @"unchosen" options)
    , bench "options/tabBar" "×→×" tabs (tabBarV @"Tab" tabOptions)
    , bench "options/select" "×→×" chosen (selectV @"Pick" selectOptions)
    , bench "options/radioButton" "×→×" chosen (radioButtonV @"Pick" radioOptions)
    , bench "options/selectUnpicked" "×→×" picks (selectUnpickedV @"Pick" @"chosen" selectOptions)
    , bench "options/selectOptional" "×→×" picks (selectOptionalV @"Pick" @"chosen" @"unchosen" selectOptions)
    , bench "options/radioButtonUnpicked" "×→×" picks (radioButtonUnpickedV @"Pick" @"chosen" radioOptions)
    , bench "options/radioButtonOptional" "×→×" picks (radioButtonOptionalV @"Pick" @"chosen" @"unchosen" radioOptions)
    , bench "options/segmentedButton" "×→×" chosen (segmentedButtonV @"Pick" segmentOptions)
    , bench "options/segmentedButtonUnpicked" "×→×" picks (segmentedButtonUnpickedV @"Pick" @"chosen" segmentOptions)
    , bench "options/segmentedButtonOptional" "×→×" picks (segmentedButtonOptionalV @"Pick" @"chosen" @"unchosen" segmentOptions)
    , bench "button" "×→+" rows (button @"Go")
    , bench "outlinedButton" "×→+" rows (outlinedButton @"Go")
    , bench "textButton" "×→+" rows (textButton @"Go")
    , bench "elevatedButton" "×→+" rows (elevatedButton @"Go")
    , bench "tonalButton" "×→+" rows (tonalButton @"Go")
    , bench "fab" "×→+" rows (fab @"Go" "add")
    , bench "iconButton" "×→+" rows (iconButton @"Go" "add")
    , bench "tile" "×→+" rows (tile @"Go" @"n" (staticText "Go"))
    , bench "doubleClickTile" "×→+" rows (doubleClickTile @"Go" @"n" (staticText "Go"))
    , bench "menuItem" "×→+" rows (menuItem @"Go")
    , bench "listOf" "×→+" lists (listOf @"picked" @"id" itemsOf (textAt @"title"))
    , bench "listOfAt" "×→+" lists (listOfAt @"picked" @"id" @"items" (textAt @"title"))
    , bench "snackbar" "+→×" events (snackbar @"event" (const {}))
    , bench "indeterminateLinearProgress" "+→×" runs (indeterminateLinearProgress)
    , bench "indeterminateCircularProgress" "+→×" runs (indeterminateCircularProgress)
    -- the dialogs last: a modal open traps the page's focus, so nothing is typed after them
    , bench "confirmed" "+→+" decisions (confirmed @"decision" (text nLine))
    , bench "acknowledged" "+→+" decisions (acknowledged @"decision" (text nLine))
    ]
  where
  fractionOf { fraction } = fraction
  itemsOf { items } = items
  texts = [ { "Name": "alpha", other: 1 }, { "Name": "beta", other: 2 } ]
  ticks = [ { "Terms": .accepted {}, other: 1 }, { "Terms": .declined {}, other: 2 } ]
  flags = [ { "On": true, other: 1 }, { "On": false, other: 2 } ]
  quantity current = { current, min: 1.0, max: 10.0, step: .discrete 1.0 }
  quantities = [ { "Amount": quantity 3.0, other: 1 }, { "Amount": quantity 7.0, other: 2 } ]
  tabA = .a {} :: [ a :: {}, b :: {} ]
  tabB = .b {} :: [ a :: {}, b :: {} ]
  tabs = [ { "Tab": tabA, other: 1 }, { "Tab": tabB, other: 2 } ]
  fractions = [ { fraction: 0.25 }, { fraction: 0.75 } ]
  images = [ { src: "a.png", alt: "A" }, { src: "b.png", alt: "B" } ]
  grouped = [ { "Customer": { "Name": "alpha" }, other: 1 }, { "Customer": { "Name": "beta" }, other: 2 } ]
  options = choice @"one" <+> choice @"two"
  tabOptions :: PUI Web [ a :: {}, b :: {} ] [ a :: {}, b :: {} ]
  tabOptions = VariantToVariant.do
    tab @"a"
    tab @"b"
  selectOptions :: PUI Web [ one :: {}, two :: {} ] [ one :: {}, two :: {} ]
  selectOptions = VariantToVariant.do
    option @"one"
    option @"two"
  radioOptions :: PUI Web [ one :: {}, two :: {} ] [ one :: {}, two :: {} ]
  radioOptions = VariantToVariant.do
    radio @"one"
    radio @"two"
  segmentOptions :: PUI Web [ one :: {}, two :: {} ] [ one :: {}, two :: {} ]
  segmentOptions = VariantToVariant.do
    segment @"one"
    segment @"two"
  chosen = [ { "Pick": .one {} }, { "Pick": .two {} } ]
  picks = [ { "Pick": .unchosen {} }, { "Pick": .chosen (.one {}) }, { "Pick": .chosen (.two {}) } ] :: Array { "Pick" :: [ chosen :: [ one :: {}, two :: {} ], unchosen :: {} ] }
  rows = [ { n: 1 }, { n: 2 } ]
  decisions = [ .decision { n: 1 }, .decision { n: 2 } ] :: Array [ decision :: { n :: Int } ]
  nLine { n } = "n = " <> show n
  lists = [ { items: [ { id: 1, title: "first" }, { id: 2, title: "second" } ] }, { items: [ { id: 2, title: "second" } ] } ]
  events = [ .event "hello", .event "world" ]
  runs = [ .started {}, .ended {} ]
