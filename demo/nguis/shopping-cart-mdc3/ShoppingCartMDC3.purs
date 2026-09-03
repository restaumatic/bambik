module ShoppingCartMDC3 (shoppingCartMDC3) where

import Prelude (Unit, const, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (foreach, mvu, toCase, updated, with)
import PUI.Web.HTML (shown, body, clicked, text)
import PUI.Web.MDC3 (bodyLarge, button, card, dataCell, dataRow, dataTable, elevation5, listOf)
import QualifiedDo.Category as Category
import ShoppingCartLogic (addUnit, cartLines, catalogueLine, emptyCart, lineTotalLine, productCatalogue, productLine, quantityLine, removeUnit, totalLine)

shoppingCartMDC3 :: Effect Unit
shoppingCartMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          listOf {} productCatalogue (text catalogueLine) # toCase @"productPicked" _.product # updated (match { productPicked: addUnit })
          dataTable { label: "Cart", columns: [ "Product", "Qty", "Total" ] }
            ( ( clicked $ dataRow RecordToRecord.do
                  dataCell (text productLine)
                  dataCell (text quantityLine)
                  dataCell (text lineTotalLine) ) # foreach @"product" cartLines ) # toCase @"linePicked" _.product # updated (match { linePicked: removeUnit })
          bodyLarge (text totalLine) # shown
          button @"Empty cart" {} # with emptyCart # updated (match { "Empty cart": const })
      ) # mvu emptyCart
