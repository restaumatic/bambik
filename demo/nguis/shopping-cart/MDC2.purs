module ShoppingCart.MDC2 (shoppingCart) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (fold, foldInto, gatheredOf, looped, stepped, with)
import PUI.Web (text, textAt)
import PUI.Web.MDC2 (body, body1, button, columnHeader, dataCell, dataRow, dataTable, iconButton, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid
import ShoppingCart (addUnit, cartLines, lineTotalLine, morningShelf, quantityLine, removeUnit, returnEverything, shelf, shelfLine, totalLine)

shoppingCart :: Effect Unit
shoppingCart =
  body $ Semigroupoid.do
    body1 $ text totalLine
    ( Semigroupoid.do
      RecordToVariant.do
        listOf @"Unit added" @"name" shelf $ text shelfLine
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
            dataCell $ iconButton @"Unit removed" "remove" ) # gatheredOf @"product" cartLines )
        button @"Empty cart"
      VariantToRecord.do
        foldInto @"Unit added" addUnit
        foldInto @"Unit removed" removeUnit
        fold @"Empty cart" returnEverything ) # stepped
  # looped # with morningShelf
