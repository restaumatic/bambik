module ShoppingCart (Cart, CartLine, Event, Product, cartLines, lineTotalLine, quantityLine, shelf, shelfLine, stocked, totalLine) where

import Prelude ((#), (&&), (*), (+), (-), (/), (<), (<>), (==), (>), class Applicative, map, mod, negate, otherwise, show)

import Data.Array (any, find, foldl, filter, mapMaybe, snoc)
import Data.Maybe (Maybe(..), maybe)
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import PUI (PUI, fold, looped, with)
import QualifiedDo.Semigroupoid as Semigroupoid

type Product = { name :: String, stock :: Int, unitPrice :: Int }

type Cart = { order :: Array { product :: String, quantity :: Int }, products :: Array Product }

type CartLine = { product :: String, quantity :: Int, unitPrice :: Int }

type Event =
  [ "Empty cart" :: Cart
  , "Unit added" :: { event :: String, model :: Cart }
  , "Unit removed" :: { event :: CartLine, model :: Cart }
  ]

stocked :: forall m. Applicative m => PUI m Cart Event -> PUI m {} Cart
stocked view =
  Semigroupoid.do
    view
    VariantToRecord.do
      fold @"Unit added" addUnit
      fold @"Unit removed" removeUnit
      fold @"Empty cart" returnEverything
  # looped # with morningShelf

morningShelf :: Cart
morningShelf =
  { order: []
  , products:
    [ { name: "Espresso", unitPrice: 350, stock: 12 }
    , { name: "Cappuccino", unitPrice: 450, stock: 12 }
    , { name: "Croissant", unitPrice: 320, stock: 3 }
    , { name: "Bagel", unitPrice: 280, stock: 4 }
    , { name: "Orange Juice", unitPrice: 400, stock: 6 }
    , { name: "Cheesecake", unitPrice: 550, stock: 2 }
    ]
  }

totalLine :: Cart -> String
totalLine cart = "Total: $" <> formatMoney (foldl (\sum l -> sum + l.quantity * l.unitPrice) 0 (cartLines cart))

shelf :: Cart -> Array Product
shelf { products } = filter (\p -> p.stock > 0) products

shelfLine :: Product -> String
shelfLine { name, unitPrice, stock } = name <> " · $" <> formatMoney unitPrice <> " · " <> show stock <> " in stock"

cartLines :: Cart -> Array CartLine
cartLines { order, products } = mapMaybe line order
  where
  line { product, quantity } = map (\p -> { product, quantity, unitPrice: p.unitPrice }) (find (\p -> p.name == product) products)

quantityLine :: CartLine -> String
quantityLine { quantity } = show quantity

lineTotalLine :: CartLine -> String
lineTotalLine { unitPrice, quantity } = "$" <> formatMoney (quantity * unitPrice)

addUnit :: { event :: String, model :: Cart } -> Cart
addUnit { event: name, model: cart@{ order, products } }
  | any (\p -> p.name == name && p.stock > 0) products = cart
    { products = restocked name (-1) products
    , order =
      if any (\l -> l.product == name) order then map (\l -> if l.product == name then l { quantity = l.quantity + 1 } else l) order
      else snoc order { product: name, quantity: 1 }
    }
  | otherwise = cart

removeUnit :: { event :: CartLine, model :: Cart } -> Cart
removeUnit { event: { product: name }, model: cart@{ order, products } } = cart
  { products = restocked name 1 products
  , order = mapMaybe oneFewer order
  }
  where
  oneFewer l
    | l.product == name = if l.quantity == 1 then Nothing else Just l { quantity = l.quantity - 1 }
    | otherwise = Just l

returnEverything :: Cart -> Cart
returnEverything cart@{ order, products } = cart
  { products = map (\p -> p { stock = p.stock + maybe 0 _.quantity (find (\l -> l.product == p.name) order) }) products
  , order = []
  }

restocked :: String -> Int -> Array Product -> Array Product
restocked name units = map \p -> if p.name == name then p { stock = p.stock + units } else p

formatMoney :: Int -> String
formatMoney cents = show (cents / 100) <> "." <> pad (mod cents 100)
  where
  pad r = if r < 10 then "0" <> show r else show r
