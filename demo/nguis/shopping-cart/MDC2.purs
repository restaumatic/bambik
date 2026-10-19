module ShoppingCart.MDC2 (shoppingCart) where

import Prelude ((#), ($), Unit, const)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (anyOf, fold, joined, looped, reset, with)
import PUI.Web (text, textAt)
import PUI.Web.MDC2 (body, body1, button, columnHeader, dataCell, dataRow, dataTable, iconButton, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid
import ShoppingCart (addUnit, cartLines, catalogueLine, emptyCart, lineTotalLine, productCatalogue, quantityLine, removeUnit, totalLine)

shoppingCart :: Effect Unit
shoppingCart =
  body $ Semigroupoid.do
    body1 $ text totalLine
    RecordToVariant.do
      ( listOf @"Unit added" @"product" (const productCatalogue) $ text catalogueLine ) # joined @"Unit added"
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
          dataCell $ iconButton @"Unit removed" "remove" ) # anyOf @"product" cartLines ) # joined @"Unit removed"
      button @"Empty cart"
    VariantToRecord.do
      fold @"Unit added" addUnit
      fold @"Unit removed" removeUnit
      reset @"Empty cart" emptyCart
  # looped # with emptyCart
