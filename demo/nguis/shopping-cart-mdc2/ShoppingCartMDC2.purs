module ShoppingCartMDC2 (shoppingCartMDC2) where

import Prelude (Unit, const, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (foreach, mvu, toCase, updated, with)
import PUI.Web.HTML (shown, body, clicked, text)
import PUI.Web.MDC2 (body1, button, card, dataCell, dataRow, dataTable, elevation20, listOf)
import QualifiedDo.Category as Category
import ShoppingCartLogic (addUnit, cartLines, catalogueLine, emptyCart, lineTotalLine, productCatalogue, productLine, quantityLine, removeUnit, totalLine)

shoppingCartMDC2 :: Effect Unit
shoppingCartMDC2 =
  body $
    elevation20 $
      card $ ( Category.do
          listOf {} productCatalogue (text catalogueLine) # toCase @"productPicked" _.product # updated (match { productPicked: addUnit })
          dataTable { label: "Cart", columns: [ "Product", "Qty", "Total" ] }
            ( ( clicked $ dataRow RecordToRecord.do
                  dataCell (text productLine)
                  dataCell (text quantityLine)
                  dataCell (text lineTotalLine) ) # foreach @"product" cartLines ) # toCase @"linePicked" _.product # updated (match { linePicked: removeUnit })
          body1 (text totalLine) # shown
          button @"Empty cart" {} # with emptyCart # updated (match { "Empty cart": const })
      ) # mvu emptyCart
