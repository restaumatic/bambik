module Main (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap)
import Data.Profunctor.EditPropP (property)
import Data.Profunctor.IntroPropP (newProperty)
import Data.Profunctor.OutputP (constant, function, output')
import Data.Profunctor.Zero (pzero)
import Effect (Effect)
import MDC as MDC
import QualifiedDo.Semigroupoid as Semigroupoid
import Web (body, text)

main :: Effect Unit
main = body @(Record ()) $ Semigroupoid.do
  newProperty @"day" $ lcmap (const "1") $ MDC.filledTextField { floatingLabel: "Text" }
  newProperty @"product" $ lcmap (const { name: "Coke", remarks: "cold" }) $ MDC.card $ Semigroupoid.do
    property @"name" $ MDC.filledTextField { floatingLabel: "Name" }
    property @"remarks" $ MDC.filledTextField { floatingLabel: "Remarks" }
  newProperty @"quantity" $ lcmap (const "1") $ MDC.filledTextField { floatingLabel: "Quantity" }
  newProperty @"price" $ lcmap (const "10") $ MDC.filledTextField { floatingLabel: "Price" }
  -- newProperty @"fulfilment" $ lcmap (const (inj (Proxy @"takeaway") {code: "XYZ"})) Semigroupoid.do
  --   variant' @"takeaway" (\_ -> {code: "a code"}) $ Semigroupoid.do
  --     property @"code" $ MDC.filledTextField { floatingLabel: "Code" }
  --   variant' @"delivery" (\_ -> {address: "an address"}) $ Semigroupoid.do
  --     property @"address" $ MDC.filledTextField { floatingLabel: "Address" }
  --   variant' @"dinein" (\_ -> {table: "a table"}) $ Semigroupoid.do
  --     property @"table" $ MDC.filledTextField { floatingLabel: "Table" }
  constant "Product name: " $ text
  property @"product" $ output' @"name" $ text
  constant ", Product remarks: " $ text
  property @"product" $ output' @"remarks" $ text
  constant ", Quantity: " $ text
  output' @"quantity" $ text
  constant ", Price: " $ text
  output' @"price" $ text
  constant ", function: " $ text
  function (\_ -> "2") $ text
  MDC.containedButton { icon: Nothing, label: Just "Enter" }
  constant "Thank you for your order!" $ text
  pzero

-- variant notation? | takeaway: { code: "317" } |
