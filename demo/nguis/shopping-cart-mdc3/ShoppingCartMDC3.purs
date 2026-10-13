module ShoppingCartMDC3 (shoppingCartMDC3) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (fold, foreach, joined, looped, prefixed, reset, unshown, with)
import PUI.Web (clicked, text)
import PUI.Web.MDC3 (body, bodyLarge, button, columnHeader, dataCell, dataRow, dataTable, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid
import ShoppingCartViewModel (addUnit, cartLines, catalogueLine, emptyCart, lineTotalLine, productCatalogue, productLine, quantityLine, removeUnit, totalLine)

shoppingCartMDC3 :: Effect Unit
shoppingCartMDC3 =
  body $ Semigroupoid.do
    prefixed (bodyLarge (text totalLine)) $ RecordToVariant.do
      listOf @"Unit added" @"product" @( product :: { name :: String, unitPrice :: Int } ) {} productCatalogue (text catalogueLine) # joined @"Unit added"
      dataTable "Cart"
        ( RecordToRecord.do
          columnHeader "Product"
          columnHeader "Qty"
          columnHeader "Total" )
        ( ( clicked @"Unit removed" @"product" $ dataRow RecordToRecord.do
          dataCell (text productLine)
          dataCell (text quantityLine)
          dataCell (text lineTotalLine) ) # foreach @"product" @( product :: String, unitPrice :: Int, quantity :: Int ) cartLines ) # joined @"Unit removed"
      button @"Empty cart" {}
    VariantToRecord.do
      unshown @"Unit added" # fold addUnit
      unshown @"Unit removed" # fold removeUnit
      unshown @"Empty cart" # reset emptyCart
  # looped @( order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } ) # with emptyCart
