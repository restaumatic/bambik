module ShoppingCart (shoppingCart) where

import Prelude ((#), ($), (*), (+), (-), (/), (<), (<<<), (<>), (==), Unit, const, map, mod, otherwise, show)

import Data.Array (any, foldl, mapMaybe, snoc)
import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap, rmap)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (foreach, forField, forValue, mvu, projection, tapped, toCase, updates)
import PUI.HTML (body, clicked, text)
import PUI.MDC (body1, button, card, dataCell, dataRow, dataTable, elevation20, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid

shoppingCart :: Effect Unit
shoppingCart =
  body $
    elevation20 $
      card { caption: "Shopping Cart" } $ ( Semigroupoid.do
          listOf {} (text # projection productOffer) # lcmap productCatalogue # toCase @"productPicked" # updates (match { productPicked: addUnit })
          dataTable { label: "Cart", columns: [ "Product", "Qty", "Total" ] }
            ( ( clicked $ dataRow RecordToRecord.do
                  dataCell text # forValue # forField @"product"
                  dataCell text # forValue # forField @"quantity"
                  dataCell text # forValue # forField @"lineTotal") # foreach @"product") # lcmap cartLines # rmap _.product # toCase @"linePicked" # updates (match { linePicked: removeUnit })
          body1 text # projection grandTotal # tapped
          button { label: "Empty cart" } # updates (match { clicked: const <<< clearCart })
      ) # mvu emptyCart

emptyCart :: { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } }
emptyCart = { order: [] }

productCatalogue :: {} -> Array { name :: String, unitPrice :: Int }
productCatalogue _ =
  [ { name: "Espresso", unitPrice: 350 }
  , { name: "Cappuccino", unitPrice: 450 }
  , { name: "Croissant", unitPrice: 320 }
  , { name: "Bagel", unitPrice: 280 }
  , { name: "Orange Juice", unitPrice: 400 }
  , { name: "Cheesecake", unitPrice: 550 }
  ]

productOffer :: { name :: String, unitPrice :: Int } -> String
productOffer p = p.name <> " · " <> formatMoney p.unitPrice

addUnit :: { name :: String, unitPrice :: Int } -> { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } } -> { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } }
addUnit p cart
  | any (\l -> l.product.name == p.name) cart.order =
      cart { order = map (\l -> if l.product.name == p.name then l { quantity = l.quantity + 1 } else l) cart.order }
  | otherwise = cart { order = snoc cart.order { product: p, quantity: 1 } }

removeUnit :: String -> { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } } -> { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } }
removeUnit name cart = cart { order = mapMaybe oneFewer cart.order }
  where
  oneFewer l
    | l.product.name == name = if l.quantity == 1 then Nothing else Just l { quantity = l.quantity - 1 }
    | otherwise = Just l

clearCart :: { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } } -> { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } }
clearCart _ = emptyCart

cartLines :: { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } } -> Array { product :: String, quantity :: String, lineTotal :: String }
cartLines cart = map line cart.order
  where
  line l = { product: l.product.name, quantity: show l.quantity, lineTotal: formatMoney (l.quantity * l.product.unitPrice) }

grandTotal :: { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } } -> String
grandTotal cart = "Total: " <> formatMoney (foldl (\sum l -> sum + l.quantity * l.product.unitPrice) 0 cart.order)

formatMoney :: Int -> String
formatMoney cents = "$" <> show (cents / 100) <> "." <> pad (mod cents 100)
  where
  pad r = if r < 10 then "0" <> show r else show r
