module ShoppingCartViewModel (addUnit, cartEmptiedLine, cartLines, catalogueLine, emptyCart, lineTotalLine, productCatalogue, productLine, quantityLine, removeUnit, totalLine, unitAddedLine, unitRemovedLine) where

import Prelude ((<>), (*), (+), (-), (/), (<), (==), map, mod, otherwise, show)

import Data.Array (any, foldl, length, mapMaybe, snoc)
import Data.Maybe (Maybe(..))

emptyCart :: { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } }
emptyCart = { order: [] }

totalLine :: { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } } -> String
totalLine { order } = "Total: $" <> formatMoney (foldl (\sum l -> sum + l.quantity * l.product.unitPrice) 0 order)

productCatalogue :: { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } } -> Array { product :: { name :: String, unitPrice :: Int } }
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

addUnit :: { event :: { name :: String, unitPrice :: Int }, model :: { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } } } -> { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } }
addUnit { event: product, model: cart@{ order } }
  | any (\l -> l.product.name == product.name) order =
    cart { order = map (\l -> if l.product.name == product.name then l { quantity = l.quantity + 1 } else l) order }
  | otherwise = cart { order = snoc order { product: { name: product.name, unitPrice: product.unitPrice }, quantity: 1 } }

removeUnit :: { event :: String, model :: { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } } } -> { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } }
removeUnit { event: name, model: cart } = cart { order = mapMaybe oneFewer cart.order }
  where
  oneFewer l
    | l.product.name == name = if l.quantity == 1 then Nothing else Just l { quantity = l.quantity - 1 }
    | otherwise = Just l

unitAddedLine :: { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } } -> String
unitAddedLine { order } = "Added — " <> cartSummary order

unitRemovedLine :: { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } } -> String
unitRemovedLine { order } = if length order == 0 then "Removed the last item" else "Removed — " <> cartSummary order

cartSummary :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } -> String
cartSummary order = show (foldl (\n l -> n + l.quantity) 0 order) <> " items, $" <> formatMoney (foldl (\sum l -> sum + l.quantity * l.product.unitPrice) 0 order)

cartEmptiedLine :: { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } } -> String
cartEmptiedLine _ = "Emptied the cart"

cartLines :: { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } } -> Array { product :: String, quantity :: Int, unitPrice :: Int }
cartLines { order } = map line order
  where
  line { product, quantity } = { product: product.name, unitPrice: product.unitPrice, quantity }

productLine :: { product :: String, quantity :: Int, unitPrice :: Int } -> String
productLine { product } = product

quantityLine :: { product :: String, quantity :: Int, unitPrice :: Int } -> String
quantityLine { quantity } = show quantity

lineTotalLine :: { product :: String, quantity :: Int, unitPrice :: Int } -> String
lineTotalLine { unitPrice, quantity } = "$" <> formatMoney (quantity * unitPrice)

formatMoney :: Int -> String
formatMoney cents = show (cents / 100) <> "." <> pad (mod cents 100)
  where
  pad r = if r < 10 then "0" <> show r else show r
