module ShoppingCartMDC3 (shoppingCartMDC3) where

import Prelude (Unit, const, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (foreach, mvu, projection, toCase, updated, with)
import PUI.Web.HTML (shown, body, clicked, staticText, text)
import PUI.Web.MDC3 (bodyLarge, button, card, dataCell, dataRow, dataTable, elevation5, listOf)
import QualifiedDo.Category as Category
import ShoppingCartLogic (addUnit, cartLines, emptyCart, formatMoney, grandTotalText, productCatalogue, removeUnit)

shoppingCartMDC3 :: Effect Unit
shoppingCartMDC3 =
  body $
    elevation5 $
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
          ( bodyLarge $ RecordToRecord.do
              staticText "Total: $"
              text @"order" # projection grandTotalText ) # shown
          button @"Empty cart" {} # with emptyCart # updated (match { "Empty cart": const })
      ) # mvu emptyCart
