module RestaurantMenuLogic (courseTitle, courses, dishDescription, dishName, dishPrice, tagText) where

import Prelude ((<>))

courses :: Array { name :: String, dishes :: Array { name :: String, price :: String, description :: String, tags :: Array String } }
courses =
  [ { name: "Antipasti"
    , dishes:
        [ { name: "Burrata di Puglia", price: "14", description: "Heirloom tomato, basil oil, Maldon salt.", tags: [ "vegetarian" ] }
        , { name: "Vitello Tonnato", price: "16", description: "Rose veal, tuna-caper emulsion, fried capers.", tags: [] }
        , { name: "Carciofi alla Romana", price: "13", description: "Braised artichoke, mint, lemon.", tags: [ "vegetarian", "gluten-free" ] }
        ]
    }
  , { name: "Primi"
    , dishes:
        [ { name: "Tagliatelle al Tartufo", price: "24", description: "Fresh egg pasta, black truffle, aged parmigiano.", tags: [ "signature" ] }
        , { name: "Risotto ai Porcini", price: "21", description: "Carnaroli rice, porcini, grana padano.", tags: [ "vegetarian", "gluten-free" ] }
        , { name: "Gnocchi alla Sorrentina", price: "18", description: "Potato gnocchi, San Marzano, fior di latte.", tags: [ "vegetarian" ] }
        ]
    }
  , { name: "Dolci"
    , dishes:
        [ { name: "Tiramisù della Casa", price: "9", description: "Mascarpone, espresso, savoiardi, bitter cocoa.", tags: [ "signature" ] }
        , { name: "Panna Cotta ai Frutti", price: "8", description: "Vanilla cream, forest berries.", tags: [ "gluten-free" ] }
        , { name: "Affogato al Caffè", price: "7", description: "Fior di latte gelato drowned in hot espresso.", tags: [ "vegetarian", "gluten-free" ] }
        ]
    }
  ]

courseTitle :: { name :: String, dishes :: Array { name :: String, price :: String, description :: String, tags :: Array String } } -> String
courseTitle { name } = name

dishName :: { name :: String, price :: String, description :: String, tags :: Array String } -> String
dishName { name } = name

dishPrice :: { name :: String, price :: String, description :: String, tags :: Array String } -> String
dishPrice { price } = "\x20ac" <> price

dishDescription :: { name :: String, price :: String, description :: String, tags :: Array String } -> String
dishDescription { description } = description

tagText :: { tag :: String } -> String
tagText { tag } = tag
