module Main (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap, rmap)
import Data.Profunctor.EditPropP (edit)
import Data.Profunctor.IntroPropP (input)
import Data.Profunctor.ReadP (constant, output)
import Data.Profunctor.Zero (pzero)
import Effect (Effect)
import MDC as MDC
import Prim.Row (class Lacks)
import QualifiedDo.Semigroupoid as Semigroupoid
import UI (UI)
import Web (Web, body, text)

main :: Effect Unit
main = body @(Record ()) $ lcmap (\_ -> { foo: "foo" }) $ Semigroupoid.do
  edit @"foo" $ MDC.filledTextField { floatingLabel: "Foo" }
  output @"foo" $ text
  input @"day" $ lcmap (const "1") $ MDC.filledTextField { floatingLabel: "Text" }
  input @"product" $ MDC.card $ Semigroupoid.do
    input @"name" $ lcmap (const "") $ MDC.filledTextField { floatingLabel: "Name" }
    input @"remarks" $ lcmap (const "") $ MDC.filledTextField { floatingLabel: "Remarks" }
  -- input @"product" $ lcmap (const { name: "Coke", remarks: "cold" }) $ MDC.card $ Semigroupoid.do
  --   edit @"name" $ MDC.filledTextField { floatingLabel: "Name" }
  --   edit @"remarks" $ MDC.filledTextField { floatingLabel: "Remarks" }
  input @"quantity" $ lcmap (const "1") $ MDC.filledTextField { floatingLabel: "Quantity" }
  input @"price" $ lcmap (const "10") $ MDC.filledTextField { floatingLabel: "Price" }
  -- input @"fulfilment" $ lcmap (const (inj (Proxy @"takeaway") {code: "XYZ"})) Semigroupoid.do
  --   variant' @"takeaway" (\_ -> {code: "a code"}) $ Semigroupoid.do
  --     edit @"code" $ MDC.filledTextField { floatingLabel: "Code" }
  --   variant' @"delivery" (\_ -> {address: "an address"}) $ Semigroupoid.do
  --     edit @"address" $ MDC.filledTextField { floatingLabel: "Address" }
  --   variant' @"dinein" (\_ -> {table: "a table"}) $ Semigroupoid.do
  --     edit @"table" $ MDC.filledTextField { floatingLabel: "Table" }
  -- constant "Product name: " $ text
  -- edit @"product" $ output @"name" $ text -- smell, it's not edit
  -- constant ", Product remarks: " $ text
  -- edit @"product" $ output @"remarks" $ text -- smell, it's not edit

  output @"product" $ Semigroupoid.do
    constant "Product name: " $ text
    output @"name" $ text
    constant ", Product remarks: " $ text
    output @"remarks" $ text
    -- output @"aaa" $ text

  -- constant ", Quantity: " $ text
  -- output @"quantity" $ text
  -- output @"price" $ text
  -- constant ", function: " $ text
  -- function (\_ -> "2") $ text
  -- MDC.containedButton { icon: Nothing, label: Just "Enter" }
  -- constant "Thank you for your order!" $ text
  pzero

-- variant notation? | takeaway: { code: "317" } |
