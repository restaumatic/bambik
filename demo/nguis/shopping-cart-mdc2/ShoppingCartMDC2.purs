module ShoppingCartMDC2 (shoppingCartMDC2) where

import Prelude (Unit, const, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (projection, foreach, mvu, projected, toCase, updated, with)
import PUI.Web.HTML (shown, body, clicked, staticText, text)
import PUI.Web.MDC2 (body1, button, card, dataCell, dataRow, dataTable, elevation20, listOf)
import QualifiedDo.Category as Category
import ShoppingCartLogic (addUnit, cartLines, emptyCart, formatMoney, grandTotalText, productCatalogue, removeUnit)

shoppingCartMDC2 :: Effect Unit
shoppingCartMDC2 =
  body $
    elevation20 $
      card $ ( Category.do
          listOf {} productCatalogue ( RecordToRecord.do
              text @"name"
              staticText " · $"
              text @"unitPrice" # projection formatMoney ) # toCase @"productPicked" { product: _ } # updated (match { productPicked: addUnit })
          dataTable { label: "Cart", columns: [ "Product", "Qty", "Total" ] }
            ( ( clicked $ dataRow RecordToRecord.do
                  dataCell (text @"product")
                  dataCell (text @"quantity")
                  dataCell ( RecordToRecord.do
                      staticText "$"
                      text @"lineTotal" )) # foreach @"product" cartLines ) # toCase @"linePicked" _.product # updated (match { linePicked: removeUnit })
          ( body1 $ RecordToRecord.do
              staticText "Total: $"
              text @"grandTotal" # projected grandTotalText ) # shown
          button @"Empty cart" {} # with emptyCart # updated (match { "Empty cart": const })
      ) # mvu emptyCart
