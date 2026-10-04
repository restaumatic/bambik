module ShoppingCartMDC2 (shoppingCartMDC2) where

import Prelude ((#), ($), Unit, const)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (fold, foreach, joined, mvu)
import PUI.Web (clicked, shown, text)
import PUI.Web.MDC2 (body, body1, button, columnHeader, dataCell, dataRow, dataTable, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid
import ShoppingCartViewModel (addUnit, cartLines, catalogueLine, emptyCart, lineTotalLine, productCatalogue, productLine, quantityLine, removeUnit, totalLine)

shoppingCartMDC2 :: Effect Unit
shoppingCartMDC2 =
  body $
    ( Semigroupoid.do
      body1 (text totalLine) # shown
      RecordToVariant.do
        listOf @"added" @"product" @( product :: { name :: String, unitPrice :: Int } ) {} productCatalogue (text catalogueLine) # joined @"added"
        dataTable @"Cart"
          ( RecordToRecord.do
            columnHeader @"Product"
            columnHeader @"Qty"
            columnHeader @"Total" )
          ( ( clicked @"removed" _.product $ dataRow RecordToRecord.do
            dataCell (text productLine)
            dataCell (text quantityLine)
            dataCell (text lineTotalLine) ) # foreach @"product" @( product :: String, unitPrice :: Int, quantity :: Int ) cartLines ) # joined @"removed"
        button @"Empty cart" {}
      VariantToRecord.do
        fold @"added" addUnit
        fold @"removed" removeUnit
        fold @"Empty cart" (const emptyCart)
    ) # mvu @( order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } ) emptyCart
