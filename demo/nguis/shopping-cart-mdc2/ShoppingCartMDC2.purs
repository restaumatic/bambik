module ShoppingCartMDC2 (shoppingCartMDC2) where

import Prelude (identity, (#), ($), Unit, const)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (forField, foreach, informed, mvu, projected, tapped, toCase, updated, with)
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
              text # forField @"value" @"name" identity
              staticText " · $"
              text # forField @"value" @"unitPrice" formatMoney ) # toCase @"productPicked" { product: _ } # updated (match { productPicked: informed addUnit })
          dataTable { label: "Cart", columns: [ "Product", "Qty", "Total" ] }
            ( ( clicked $ dataRow RecordToRecord.do
                  dataCell text # forField @"value" @"product" identity
                  dataCell text # forField @"value" @"quantity" identity
                  dataCell ( RecordToRecord.do
                      staticText "$"
                      text # forField @"value" @"lineTotal" identity )) # foreach @"product" cartLines) # toCase @"linePicked" _.product # updated (match { linePicked: removeUnit })
          body1 ( RecordToRecord.do
              staticText "Total: $"
              text # projected @"value" grandTotalText ) # tapped
          button { label: "Empty cart" } # with emptyCart # updated (match { clicked: const })
      ) # mvu emptyCart
