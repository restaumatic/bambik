module ShoppingCartMD3 (shoppingCartMD3) where

import Prelude ((#), ($), (*), (+), (-), (/), (<), (<>), (==), Unit, const, map, mod, otherwise, show)

import Data.Array (any, foldl, mapMaybe, snoc)
import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap, rmap)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (foreach, forField, forValue, mvu, projection, tapped, toCase, updates)
import PUI.HTML (body, clicked, staticText, text)
import PUI.MDC3 (bodyLarge, button, card, dataCell, dataRow, dataTable, elevation5, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid

shoppingCartMD3 :: Effect Unit
shoppingCartMD3 =
  body $
    elevation5 $
      card { caption: "Shopping Cart" } $ ( Semigroupoid.do
          listOf {} ( RecordToRecord.do
              text # forValue # forField @"name"
              staticText " · $"
              text # projection formatMoney # forField @"unitPrice" ) # lcmap productCatalogue # toCase @"productPicked" # updates (match { productPicked: addUnit })
          dataTable { label: "Cart", columns: [ "Product", "Qty", "Total" ] }
            ( ( clicked $ dataRow RecordToRecord.do
                  dataCell text # forValue # forField @"product"
                  dataCell text # forValue # forField @"quantity"
                  dataCell ( RecordToRecord.do
                      staticText "$"
                      text # forValue # forField @"lineTotal" )) # foreach @"product") # lcmap cartLines # rmap _.product # toCase @"linePicked" # updates (match { linePicked: removeUnit })
          bodyLarge ( RecordToRecord.do
              staticText "Total: $"
              text # projection grandTotalText ) # tapped
          button { label: "Empty cart" } # lcmap clearCart # updates (match { clicked: const })
      ) # mvu emptyCart

clearCart :: {} -> { order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } }
clearCart {} = emptyCart

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
