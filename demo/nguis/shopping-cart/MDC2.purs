module ShoppingCart.MDC2 (shoppingCart) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Effect (Effect)
import PUI (gatheredOf, joined)
import PUI.Web (text, textAt)
import PUI.Web.MDC2 (body, body1, button, columnHeader, dataCell, dataRow, dataTable, iconButton, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid
import ShoppingCart (cartLines, lineTotalLine, quantityLine, shelf, shelfLine, stocked, totalLine)

shoppingCart :: Effect Unit
shoppingCart =
  body $ Semigroupoid.do
    body1 $ text totalLine
    RecordToVariant.do
      ( listOf @"Unit added" @"name" shelf $ text shelfLine ) # joined @"Unit added"
      dataTable "Cart"
        ( Semigroupoid.do
          columnHeader "Product"
          columnHeader "Qty"
          columnHeader "Total"
          columnHeader "" )
        ( ( dataRow $ Semigroupoid.do
          dataCell $ textAt @"product"
          dataCell $ text quantityLine
          dataCell $ text lineTotalLine
          dataCell $ iconButton @"Unit removed" "remove" ) # gatheredOf @"product" cartLines ) # joined @"Unit removed"
      button @"Empty cart"
  # stocked
