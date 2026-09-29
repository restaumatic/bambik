module ShoppingCartMDC2 (shoppingCartMDC2) where

import Prelude (Unit, const, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (foreach, mvu, updated, with)
import PUI.Web (clicked, shown, text)
import PUI.Web.MDC2 (body, body1, button, columnHeader, dataCell, dataRow, dataTable, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid
import ShoppingCartLogic (addUnit, cartLines, catalogueLine, emptyCart, lineTotalLine, productCatalogue, productLine, quantityLine, removeUnit, totalLine)

shoppingCartMDC2 :: Effect Unit
shoppingCartMDC2 =
  body $
    ( Semigroupoid.do
      listOf @"added" @"product" {} productCatalogue (text catalogueLine) # updated (match { added: addUnit })
      dataTable @"Cart"
        ( RecordToRecord.do
          columnHeader @"Product"
          columnHeader @"Qty"
          columnHeader @"Total" )
        ( ( clicked @"removed" _.product $ dataRow RecordToRecord.do
          dataCell (text productLine)
          dataCell (text quantityLine)
          dataCell (text lineTotalLine) ) # foreach @"product" cartLines ) # updated (match { removed: removeUnit })
      body1 (text totalLine) # shown
      button @"Empty cart" {} # with emptyCart # updated (match { "Empty cart": const })
    ) # mvu emptyCart
