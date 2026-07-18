module ShoppingCart (shoppingCart) where

import Prelude ((#), ($), (*), (+), (-), (/), (<), (<>), (==), Unit, map, mod, otherwise, show)

import Data.Array (any, foldl, mapMaybe, snoc)
import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap, rmap)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (forField, forValue, mvu, projection, tapped, updates)
import PUI.HTML (attr, body, clicked, foreach, text)
import PUI.MDC (body1, button, card, dataCell, dataRow, dataTable, elevation20, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid

shoppingCart :: Effect Unit
shoppingCart =
  body $
    elevation20 $
      card { caption: "Shopping Cart" } $ ( Semigroupoid.do
          attr "style" "border: 1px solid #ccc; margin-bottom: 8px;"
            ( listOf {} (text # projection productOffer # forValue)
            ) # lcmap (\(_ :: Cart) -> productCatalogue) # rmap (\p -> .productPicked p :: [ productPicked :: Product ]) # updates (match { productPicked: addUnit })
          dataTable { label: "Cart", columns: [ "Product", "Qty", "Total" ] }
            ( foreach $ clicked $ dataRow RecordToRecord.do
                dataCell (text # forField @"product")
                dataCell (text # forField @"quantity")
                dataCell (text # forField @"lineTotal")
            ) # lcmap cartLines # rmap (\l -> .linePicked l.product :: [ linePicked :: String ]) # updates (match { linePicked: removeUnit })
          body1 (text # projection grandTotal # forValue) # tapped
          button { label: "Empty cart" } # updates (match { clicked: \m _ -> clearCart m })
      ) # mvu emptyCart

type Product = { name :: String, unitPrice :: Int }

type Cart = { order :: Array { product :: Product, quantity :: Int } }

emptyCart :: Cart
emptyCart = { order: [] }

productCatalogue :: Array Product
productCatalogue =
  [ { name: "Espresso", unitPrice: 350 }
  , { name: "Cappuccino", unitPrice: 450 }
  , { name: "Croissant", unitPrice: 320 }
  , { name: "Bagel", unitPrice: 280 }
  , { name: "Orange Juice", unitPrice: 400 }
  , { name: "Cheesecake", unitPrice: 550 }
  ]

productOffer :: Product -> String
productOffer p = p.name <> " · " <> formatMoney p.unitPrice

addUnit :: Product -> Cart -> Cart
addUnit p cart
  | any (\l -> l.product.name == p.name) cart.order =
      cart { order = map (\l -> if l.product.name == p.name then l { quantity = l.quantity + 1 } else l) cart.order }
  | otherwise = cart { order = snoc cart.order { product: p, quantity: 1 } }

removeUnit :: String -> Cart -> Cart
removeUnit name cart = cart { order = mapMaybe oneFewer cart.order }
  where
  oneFewer l
    | l.product.name == name = if l.quantity == 1 then Nothing else Just l { quantity = l.quantity - 1 }
    | otherwise = Just l

clearCart :: Cart -> Cart
clearCart _ = emptyCart

cartLines :: Cart -> Array { product :: String, quantity :: String, lineTotal :: String }
cartLines cart = map line cart.order
  where
  line l = { product: l.product.name, quantity: show l.quantity, lineTotal: formatMoney (l.quantity * l.product.unitPrice) }

grandTotal :: Cart -> String
grandTotal cart = "Total: " <> formatMoney (foldl (\sum l -> sum + l.quantity * l.product.unitPrice) 0 cart.order)

formatMoney :: Int -> String
formatMoney cents = "$" <> show (cents / 100) <> "." <> pad (mod cents 100)
  where
  pad r = if r < 10 then "0" <> show r else show r
