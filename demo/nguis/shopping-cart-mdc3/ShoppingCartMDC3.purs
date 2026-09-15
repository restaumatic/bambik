module ShoppingCartMDC3 (shoppingCartMDC3) where

import Prelude (Unit, const, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (foreach, mvu, updated, with)
import PUI.Web.HTML (shown, clicked, text)
import PUI.Web.MDC3 (body, bodyLarge, button, card, dataCell, dataRow, dataTable, listOf)
import QualifiedDo.Category as Category
import ShoppingCartLogic (addUnit, cartLines, catalogueLine, emptyCart, lineTotalLine, productCatalogue, productLine, quantityLine, removeUnit, totalLine)

shoppingCartMDC3 :: Effect Unit
shoppingCartMDC3 =
  body $
    card $ ( Category.do
      listOf @"productPicked" _.product {} productCatalogue (text catalogueLine) # updated (match { productPicked: addUnit })
      dataTable { label: "Cart", columns: [ "Product", "Qty", "Total" ] }
        ( ( clicked @"linePicked" _.product $ dataRow RecordToRecord.do
          dataCell (text productLine)
          dataCell (text quantityLine)
          dataCell (text lineTotalLine) ) # foreach @"product" cartLines ) # updated (match { linePicked: removeUnit })
      bodyLarge (text totalLine) # shown
      button @"Empty cart" {} # with emptyCart # updated (match { "Empty cart": const })
    ) # mvu emptyCart
