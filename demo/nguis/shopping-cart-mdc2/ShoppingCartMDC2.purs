module ShoppingCartMDC2 (shoppingCartMDC2) where

import Prelude (identity, (#), ($), (*), (+), (-), (/), (<), (<>), (==), Unit, const, map, mod, otherwise, show)

import Data.Array (any, foldl, mapMaybe, snoc)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (forField, forValue, foreach, mvu, projection, tapped, toCase, updates, with)
import PUI.HTML (body, clicked, staticText, text)
import PUI.MDC2 (body1, button, card, dataCell, dataRow, dataTable, elevation20, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid

shoppingCartMDC2 :: Effect Unit
shoppingCartMDC2 =
  body $
    elevation20 $
      card { caption: "Shopping Cart" } $ ( Semigroupoid.do
          listOf {} productCatalogue ( RecordToRecord.do
              text # forValue # forField @"name"
              staticText " · $"
              text # projection formatMoney # forField @"unitPrice" ) # toCase @"productPicked" identity # updates (match { productPicked: addUnit })
          dataTable { label: "Cart", columns: [ "Product", "Qty", "Total" ] }
            ( ( clicked $ dataRow RecordToRecord.do
                  dataCell text # forValue # forField @"product"
                  dataCell text # forValue # forField @"quantity"
                  dataCell ( RecordToRecord.do
                      staticText "$"
                      text # forValue # forField @"lineTotal" )) # foreach @"product" cartLines) # toCase @"linePicked" _.product # updates (match { linePicked: removeUnit })
          body1 ( RecordToRecord.do
              staticText "Total: $"
              text # projection grandTotalText ) # tapped
          with emptyCart (button { label: "Empty cart" }) # updates (match { clicked: const })
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

addUnit :: { name :: String, unitPrice :: Int } -> { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } } -> { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } }
addUnit p@{ name } cart
  | any (\l -> l.product.name == name) cart.order =
      cart { order = map (\l -> if l.product.name == name then l { quantity = l.quantity + 1 } else l) cart.order }
  | otherwise = cart { order = snoc cart.order { product: p, quantity: 1 } }

removeUnit :: String -> { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } } -> { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } }
removeUnit name cart = cart { order = mapMaybe oneFewer cart.order }
  where
  oneFewer l
    | l.product.name == name = if l.quantity == 1 then Nothing else Just l { quantity = l.quantity - 1 }
    | otherwise = Just l

cartLines :: { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } } -> Array { product :: String, quantity :: String, lineTotal :: String }
cartLines { order } = map line order
  where
  line { product, quantity } = { product: product.name, quantity: show quantity, lineTotal: formatMoney (quantity * product.unitPrice) }

grandTotalText :: { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } } -> String
grandTotalText { order } = formatMoney (foldl (\sum l -> sum + l.quantity * l.product.unitPrice) 0 order)

formatMoney :: Int -> String
formatMoney cents = show (cents / 100) <> "." <> pad (mod cents 100)
  where
  pad r = if r < 10 then "0" <> show r else show r
