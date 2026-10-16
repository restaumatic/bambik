module ShoppingCart (Cart, CartLine, CatalogueEntry, Product, addUnit, cartLines, catalogueLine, emptyCart, lineTotalLine, productCatalogue, quantityLine, removeUnit, totalLine) where

import Prelude ((<>), (*), (+), (-), (/), (<), (==), map, mod, otherwise, show)

import Data.Array (any, foldl, mapMaybe, snoc)
import Data.Maybe (Maybe(..))

type Product = { name :: String, unitPrice :: Int }

type CatalogueEntry = { product :: Product }

type Cart = { order :: Array { product :: Product, quantity :: Int } }

type CartLine = { product :: String, quantity :: Int, unitPrice :: Int }

emptyCart :: Cart
emptyCart = { order: [] }

totalLine :: Cart -> String
totalLine { order } = "Total: $" <> formatMoney (foldl (\sum l -> sum + l.quantity * l.product.unitPrice) 0 order)

productCatalogue :: Array CatalogueEntry
productCatalogue = map (\product -> { product })
  [ { name: "Espresso", unitPrice: 350 }
  , { name: "Cappuccino", unitPrice: 450 }
  , { name: "Croissant", unitPrice: 320 }
  , { name: "Bagel", unitPrice: 280 }
  , { name: "Orange Juice", unitPrice: 400 }
  , { name: "Cheesecake", unitPrice: 550 }
  ]

catalogueLine :: CatalogueEntry -> String
catalogueLine { product } = product.name <> " · $" <> formatMoney product.unitPrice

addUnit :: { event :: Product, model :: Cart } -> Cart
addUnit { event: product, model: cart@{ order } }
  | any (\l -> l.product.name == product.name) order =
    cart { order = map (\l -> if l.product.name == product.name then l { quantity = l.quantity + 1 } else l) order }
  | otherwise = cart { order = snoc order { product: { name: product.name, unitPrice: product.unitPrice }, quantity: 1 } }

removeUnit :: { event :: CartLine, model :: Cart } -> Cart
removeUnit { event: { product: name }, model: cart } = cart { order = mapMaybe oneFewer cart.order }
  where
  oneFewer l
    | l.product.name == name = if l.quantity == 1 then Nothing else Just l { quantity = l.quantity - 1 }
    | otherwise = Just l

cartLines :: Cart -> Array CartLine
cartLines { order } = map line order
  where
  line { product, quantity } = { product: product.name, unitPrice: product.unitPrice, quantity }

quantityLine :: CartLine -> String
quantityLine { quantity } = show quantity

lineTotalLine :: CartLine -> String
lineTotalLine { unitPrice, quantity } = "$" <> formatMoney (quantity * unitPrice)

formatMoney :: Int -> String
formatMoney cents = show (cents / 100) <> "." <> pad (mod cents 100)
  where
  pad r = if r < 10 then "0" <> show r else show r
