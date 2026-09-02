module ShoppingCartLogic (addUnit, cartLines, emptyCart, presentCart, productCatalogue, removeUnit) where

import Prelude ((<>), (*), (+), (-), (/), (<), (==), map, mod, otherwise, show)

import Data.Array (any, foldl, mapMaybe, snoc)
import Data.Maybe (Maybe(..))

emptyCart :: { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int }, totalLine :: String }
emptyCart = presentCart { order: [], totalLine: "" }

presentCart :: { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int }, totalLine :: String } -> { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int }, totalLine :: String }
presentCart cart = cart { totalLine = "Total: $" <> formatMoney (foldl (\sum l -> sum + l.quantity * l.product.unitPrice) 0 cart.order) }

productCatalogue :: {} -> Array { product :: { name :: String, unitPrice :: Int }, catalogueLine :: String }
productCatalogue _ = map catalogued
  [ { name: "Espresso", unitPrice: 350 }
  , { name: "Cappuccino", unitPrice: 450 }
  , { name: "Croissant", unitPrice: 320 }
  , { name: "Bagel", unitPrice: 280 }
  , { name: "Orange Juice", unitPrice: 400 }
  , { name: "Cheesecake", unitPrice: 550 }
  ]

catalogued :: { name :: String, unitPrice :: Int } -> { product :: { name :: String, unitPrice :: Int }, catalogueLine :: String }
catalogued product = { product, catalogueLine: product.name <> " · $" <> formatMoney product.unitPrice }

addUnit :: { name :: String, unitPrice :: Int } -> { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } } -> { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } }
addUnit product { order }
  | any (\l -> l.product.name == product.name) order =
      { order: map (\l -> if l.product.name == product.name then l { quantity = l.quantity + 1 } else l) order }
  | otherwise = { order: snoc order { product, quantity: 1 } }

removeUnit :: String -> { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } } -> { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } }
removeUnit name cart = cart { order = mapMaybe oneFewer cart.order }
  where
  oneFewer l
    | l.product.name == name = if l.quantity == 1 then Nothing else Just l { quantity = l.quantity - 1 }
    | otherwise = Just l

cartLines :: { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } } -> Array { product :: String, quantity :: String, lineTotalLine :: String }
cartLines { order } = map line order
  where
  line { product, quantity } = { product: product.name, quantity: show quantity, lineTotalLine: "$" <> formatMoney (quantity * product.unitPrice) }

formatMoney :: Int -> String
formatMoney cents = show (cents / 100) <> "." <> pad (mod cents 100)
  where
  pad r = if r < 10 then "0" <> show r else show r
