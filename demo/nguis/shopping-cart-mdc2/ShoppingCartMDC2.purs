module ShoppingCartMDC2 (shoppingCartMDC2) where

import Prelude ((>>>), identity, (#), ($), Unit, const)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (announce, forField, foreach, informed, mvu, projected, tapped, toCase, updated)
import PUI.Web.HTML (body, clicked, staticText, text)
import PUI.Web.MDC2 (body1, button, card, dataCell, dataRow, dataTable, elevation20, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid
import ShoppingCartLogic (addUnit, cartLines, emptyCart, formatMoney, grandTotalText, productCatalogue, removeUnit)

shoppingCartMDC2 :: Effect Unit
shoppingCartMDC2 =
  body $
    elevation20 $
      card { caption: "Shopping Cart" } $ ( Semigroupoid.do
          listOf {} productCatalogue ( RecordToRecord.do
              text @"name"
              staticText " · $"
              text @"value" # forField @"unitPrice" formatMoney ) # toCase @"productPicked" { product: _ } # updated (match { productPicked: informed addUnit })
          dataTable { label: "Cart", columns: [ "Product", "Qty", "Total" ] }
            ( ( clicked $ dataRow RecordToRecord.do
                  dataCell (text @"product")
                  dataCell (text @"quantity")
                  dataCell ( RecordToRecord.do
                      staticText "$"
                      text @"lineTotal" )) # foreach @"product" cartLines) # toCase @"linePicked" _.product # updated (match { linePicked: removeUnit })
          body1 ( RecordToRecord.do
              staticText "Total: $"
              text @"value" # projected @"value" grandTotalText ) # tapped
          announce emptyCart >>> button { label: "Empty cart" } # updated (match { clicked: const })
      ) # mvu emptyCart
