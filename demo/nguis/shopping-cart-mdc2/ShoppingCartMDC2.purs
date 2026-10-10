module ShoppingCartMDC2 (shoppingCartMDC2) where

import Prelude ((#), ($), Unit, const)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (blankStatus, fold, foreach, joined, looped, with)
import PUI.Web (clicked, shown, text)
import PUI.Web.MDC2 (body, body1, button, columnHeader, dataCell, dataRow, dataTable, listOf, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid
import ShoppingCartViewModel (addUnit, cartEmptiedLine, cartLines, catalogueLine, emptyCart, lineTotalLine, productCatalogue, productLine, quantityLine, removeUnit, totalLine)

shoppingCartMDC2 :: Effect Unit
shoppingCartMDC2 =
  body $ Semigroupoid.do
    body1 (text totalLine) # shown
    RecordToVariant.do
      listOf @"Unit added" @"product" @( product :: { name :: String, unitPrice :: Int } ) {} productCatalogue (text catalogueLine) # joined @"Unit added"
      dataTable "Cart"
        ( RecordToRecord.do
          columnHeader "Product"
          columnHeader "Qty"
          columnHeader "Total" )
        ( ( clicked @"Unit removed" _.product $ dataRow RecordToRecord.do
          dataCell (text productLine)
          dataCell (text quantityLine)
          dataCell (text lineTotalLine) ) # foreach @"product" @( product :: String, unitPrice :: Int, quantity :: Int ) cartLines ) # joined @"Unit removed"
      button @"Empty cart" {}
    VariantToRecord.do
      blankStatus @"Unit added" # fold addUnit
      blankStatus @"Unit removed" # fold removeUnit
      snackbar @"Empty cart" cartEmptiedLine # fold (const emptyCart)
  # looped @( order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } ) # with emptyCart
