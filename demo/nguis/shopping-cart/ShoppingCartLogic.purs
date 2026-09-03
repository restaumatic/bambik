module ShoppingCartLogic (addUnit, cartLines, catalogueLine, emptyCart, lineTotalLine, productCatalogue, productLine, quantityLine, removeUnit, totalLine) where

import Prelude ((<>), (*), (+), (-), (/), (<), (==), map, mod, otherwise, show)

import Data.Array (any, foldl, mapMaybe, snoc)
import Data.Maybe (Maybe(..))

emptyCart :: { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } }
emptyCart = { order: [] }

totalLine :: { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } } -> String
totalLine { order } = "Total: $" <> formatMoney (foldl (\sum l -> sum + l.quantity * l.product.unitPrice) 0 order)

productCatalogue :: {} -> Array { product :: { name :: String, unitPrice :: Int } }
productCatalogue _ = map (\product -> { product })
  [ { name: "Espresso", unitPrice: 350 }
  , { name: "Cappuccino", unitPrice: 450 }
  , { name: "Croissant", unitPrice: 320 }
  , { name: "Bagel", unitPrice: 280 }
  , { name: "Orange Juice", unitPrice: 400 }
  , { name: "Cheesecake", unitPrice: 550 }
  ]

catalogueLine :: { product :: { name :: String, unitPrice :: Int } } -> String
catalogueLine { product } = product.name <> " · $" <> formatMoney product.unitPrice

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

cartLines :: { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } } -> Array { product :: String, unitPrice :: Int, quantity :: Int }
cartLines { order } = map line order
  where
  line { product, quantity } = { product: product.name, unitPrice: product.unitPrice, quantity }

productLine :: { product :: String, unitPrice :: Int, quantity :: Int } -> String
productLine { product } = product

quantityLine :: { product :: String, unitPrice :: Int, quantity :: Int } -> String
quantityLine { quantity } = show quantity

lineTotalLine :: { product :: String, unitPrice :: Int, quantity :: Int } -> String
lineTotalLine { unitPrice, quantity } = "$" <> formatMoney (quantity * unitPrice)

formatMoney :: Int -> String
formatMoney cents = show (cents / 100) <> "." <> pad (mod cents 100)
  where
  pad r = if r < 10 then "0" <> show r else show r
