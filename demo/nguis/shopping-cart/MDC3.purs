module ShoppingCart.MDC3 (shoppingCart) where

import Prelude ((#), ($), Unit, const)

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (anyOf, fold, joined, looped, reset, with)
import PUI.Web (clicked, text, textAt)
import PUI.Web.MDC3 (body, bodyLarge, button, columnHeader, dataCell, dataRow, dataTable, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid
import ShoppingCart (addUnit, cartLines, catalogueLine, emptyCart, lineTotalLine, productCatalogue, quantityLine, removeUnit, totalLine)

shoppingCart :: Effect Unit
shoppingCart =
  body $ Semigroupoid.do
    bodyLarge $ text totalLine
    RecordToVariant.do
      ( listOf @"Unit added" @"product" @( product :: { name :: String, unitPrice :: Int } ) (const productCatalogue) $ text catalogueLine ) # joined
      dataTable "Cart"
        ( Semigroupoid.do
          columnHeader "Product"
          columnHeader "Qty"
          columnHeader "Total" )
        ( ( clicked @"Unit removed" @"product" $ dataRow Semigroupoid.do
          dataCell $ textAt @"product"
          dataCell $ text quantityLine
          dataCell $ text lineTotalLine ) # anyOf @"product" @( product :: String, unitPrice :: Int, quantity :: Int ) cartLines ) # joined
      button @"Empty cart"
    VariantToRecord.do
      fold @"Unit added" addUnit
      fold @"Unit removed" removeUnit
      reset @"Empty cart" emptyCart
  # looped @( order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } ) # with emptyCart
