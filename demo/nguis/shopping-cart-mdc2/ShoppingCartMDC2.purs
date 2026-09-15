module ShoppingCartMDC2 (shoppingCartMDC2) where

import Prelude (Unit, const, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (foreach, mvu, updated, with)
import PUI.Web.HTML (shown, clicked, text)
import PUI.Web.MDC2 (body, body1, button, card, dataCell, dataRow, dataTable, listOf)
import QualifiedDo.Category as Category
import ShoppingCartLogic (addUnit, cartLines, catalogueLine, emptyCart, lineTotalLine, productCatalogue, productLine, quantityLine, removeUnit, totalLine)

shoppingCartMDC2 :: Effect Unit
shoppingCartMDC2 =
  body $
    card $ ( Category.do
      listOf @"productPicked" _.product {} productCatalogue (text catalogueLine) # updated (match { productPicked: addUnit })
      dataTable { label: "Cart", columns: [ "Product", "Qty", "Total" ] }
        ( ( clicked @"linePicked" _.product $ dataRow RecordToRecord.do
          dataCell (text productLine)
          dataCell (text quantityLine)
          dataCell (text lineTotalLine) ) # foreach @"product" cartLines ) # updated (match { linePicked: removeUnit })
      body1 (text totalLine) # shown
      button @"Empty cart" {} # with emptyCart # updated (match { "Empty cart": const })
    ) # mvu emptyCart
