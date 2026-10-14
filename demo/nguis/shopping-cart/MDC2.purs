module ShoppingCart.MDC2 (shoppingCart) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (anyOf, fold, joined, looped, reset, with)
import PUI.Web (clicked, text)
import PUI.Web.MDC2 (body, body1, button, columnHeader, dataCell, dataRow, dataTable, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid
import ShoppingCart (addUnit, cartLines, catalogueLine, emptyCart, lineTotalLine, productCatalogue, productLine, quantityLine, removeUnit, totalLine)

shoppingCart :: Effect Unit
shoppingCart =
  body $ Semigroupoid.do
    body1 $ text totalLine
    RecordToVariant.do
      ( listOf @"Unit added" @"product" @( product :: { name :: String, unitPrice :: Int } ) productCatalogue $ text catalogueLine ) # joined
      dataTable "Cart"
        ( Semigroupoid.do
          columnHeader "Product"
          columnHeader "Qty"
          columnHeader "Total" )
        ( ( clicked @"Unit removed" @"product" $ dataRow Semigroupoid.do
          dataCell $ text productLine
          dataCell $ text quantityLine
          dataCell $ text lineTotalLine ) # anyOf @"product" @( product :: String, unitPrice :: Int, quantity :: Int ) cartLines ) # joined
      button @"Empty cart"
    VariantToRecord.do
      fold @"Unit added" addUnit
      fold @"Unit removed" removeUnit
      reset @"Empty cart" emptyCart
  # looped @( order :: Array { product :: { name :: String, unitPrice :: Int }, quantity :: Int } ) # with emptyCart
