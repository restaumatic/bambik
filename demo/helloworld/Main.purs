module Main (main) where

import Prelude

import Data.Lens.Extra.Commons (property)
import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap)
import Data.Profunctor.StrongLike (newProperty)
import Data.Profunctor.Sum as Sum
import Data.Profunctor.Zero (pzero)
import Effect (Effect)
import MDC as MDC
import QualifiedDo.Semigroup as Semigroup
import QualifiedDo.Semigroupoid as Semigroupoid
import UI (constant, debounced)
import Web (body, label, staticText, text)

main :: Effect Unit
main = body @(Record ()) $ Semigroupoid.do
  newProperty @"productName" $ lcmap (const "") $ MDC.filledTextField { floatingLabel: "Product name" }
  newProperty @"quantity" $ lcmap (const "") $ MDC.filledTextField { floatingLabel: "Quantity" }
  debounced $ Sum.do
    constant "Hello, " text
    property @"productName" $ text
    constant " " $ text
    property @"quantity" $ text
    constant "!" $ text
    MDC.containedButton { icon: Nothing, label: Just "Enter" }
  pzero
