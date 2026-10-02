module ShoppingCartMDC2 (shoppingCartMDC2) where

import Prelude (Unit, const, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (foreach, mvu, state, updated, with)
import PUI.Web (clicked, shown, text)
import PUI.Web.MDC2 (body, body1, button, columnHeader, dataCell, dataRow, dataTable, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid
import ShoppingCartViewModel (addUnit, cartLines, catalogueLine, emptyCart, lineTotalLine, productCatalogue, productLine, quantityLine, removeUnit, totalLine)

shoppingCartMDC2 :: Effect Unit
shoppingCartMDC2 =
  body $
    ( Semigroupoid.do
      state @"order" @(Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int })
      listOf @"added" @"product" @{ name :: String, unitPrice :: Int } {} productCatalogue (text catalogueLine) # updated (match { added: addUnit })
      dataTable @"Cart"
        ( RecordToRecord.do
          columnHeader @"Product"
          columnHeader @"Qty"
          columnHeader @"Total" )
        ( ( Semigroupoid.do
            state @"unitPrice" @Int
            state @"quantity" @Int
            clicked @"removed" _.product $ dataRow RecordToRecord.do
              dataCell (text productLine)
              dataCell (text quantityLine)
              dataCell (text lineTotalLine) ) # foreach @"product" @String cartLines ) # updated (match { removed: removeUnit })
      body1 (text totalLine) # shown
      button @"Empty cart" {} # with emptyCart # updated (match { "Empty cart": const })
    ) # mvu emptyCart
