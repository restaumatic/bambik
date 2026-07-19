module RestaurantMenu (restaurantMenu) where

import Prelude (($), (>>>), Unit)

import Data.Profunctor.Row.RecordToRecord (pempty)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (with)
import PUI.HTML (a, blockquote, body, circle, cl, each, el, h1, h2, h3, hr, p, span, staticText, svg, ul, (:=))

restaurantMenu :: Effect Unit
restaurantMenu =
  body $ with {} $ el "article" >>> cl "menu" $ RecordToRecord.do
    el "header" >>> cl "menu-header" $ RecordToRecord.do
      svg >>> cl "monogram" >>> "viewBox" := "0 0 100 100" >>> "role" := "img" $ RecordToRecord.do
        circle >>> cl "ring" >>> "cx" := "50" >>> "cy" := "50" >>> "r" := "47" $ pempty
        el "text" >>> cl "initial" >>> "x" := "50" >>> "y" := "52"
          >>> "text-anchor" := "middle" >>> "dominant-baseline" := "central" $ staticText "Y"
      h1 >>> cl "restaurant-name" $ staticText "Osteria Yoneda"
      p >>> cl "tagline" $ staticText "Cucina componibile — a tasting menu, composed"
      hr
    el "div" >>> cl "courses" $ each courses \c ->
      el "section" >>> cl "course" $ RecordToRecord.do
        h2 (staticText c.name)
        ul >>> cl "dishes" $ each c.dishes \d ->
          el "li" >>> cl "dish" $ RecordToRecord.do
            el "div" >>> cl "dish-head" $ RecordToRecord.do
              span >>> cl "dish-name" $ staticText d.name
              span >>> cl "dish-dots" $ pempty
              span >>> cl "dish-price" $ staticText d.price
            p >>> cl "dish-desc" $ staticText d.description
            span >>> cl "tags" $ each d.tags \t ->
              span >>> cl "tag" $ staticText t
    blockquote >>> cl "chef-note" $ RecordToRecord.do
      p (staticText "Every plate is built from a few honest parts that compose into something whole — the same idea that built this page.")
      p >>> cl "attribution" $ staticText "— from the kitchen"
    el "footer" >>> cl "menu-footer" $ RecordToRecord.do
      el "div" >>> cl "info" $ RecordToRecord.do
        h3 (staticText "Hours")
        p (staticText "Tuesday – Sunday · 17:00 – 23:00")
      el "div" >>> cl "info" $ RecordToRecord.do
        h3 (staticText "Find us")
        p (staticText "12 Category Lane · Kraków")
      p >>> cl "colophon" $ RecordToRecord.do
        staticText "A static page composed from HTML oculars with "
        a >>> "href" := "https://github.com/restaumatic/bambik" >>> "target" := "_blank" $ staticText "Bambik"
        staticText " — no Material components, just structure."

type Dish =
  { name :: String
  , price :: String
  , description :: String
  , tags :: Array String
  }

type Course =
  { name :: String
  , dishes :: Array Dish
  }

courses :: Array Course
courses =
  [ { name: "Antipasti"
    , dishes:
        [ { name: "Burrata di Puglia", price: "€14", description: "Heirloom tomato, basil oil, Maldon salt.", tags: [ "vegetarian" ] }
        , { name: "Vitello Tonnato", price: "€16", description: "Rose veal, tuna-caper emulsion, fried capers.", tags: [] }
        , { name: "Carciofi alla Romana", price: "€13", description: "Braised artichoke, mint, lemon.", tags: [ "vegetarian", "gluten-free" ] }
        ]
    }
  , { name: "Primi"
    , dishes:
        [ { name: "Tagliatelle al Tartufo", price: "€24", description: "Fresh egg pasta, black truffle, aged parmigiano.", tags: [ "signature" ] }
        , { name: "Risotto ai Porcini", price: "€21", description: "Carnaroli rice, porcini, grana padano.", tags: [ "vegetarian", "gluten-free" ] }
        , { name: "Gnocchi alla Sorrentina", price: "€18", description: "Potato gnocchi, San Marzano, fior di latte.", tags: [ "vegetarian" ] }
        ]
    }
  , { name: "Dolci"
    , dishes:
        [ { name: "Tiramisù della Casa", price: "€9", description: "Mascarpone, espresso, savoiardi, bitter cocoa.", tags: [ "signature" ] }
        , { name: "Panna Cotta ai Frutti", price: "€8", description: "Vanilla cream, forest berries.", tags: [ "gluten-free" ] }
        , { name: "Affogato al Caffè", price: "€7", description: "Fior di latte gelato drowned in hot espresso.", tags: [ "vegetarian", "gluten-free" ] }
        ]
    }
  ]
