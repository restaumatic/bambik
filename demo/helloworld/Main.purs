module Main (main) where

import Prelude

import Data.Lens.Extra.Commons (field)
import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap)
import Data.Profunctor.StrongLike (field) as StrongLike
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
  StrongLike.field @"firstName" $ lcmap (const "") $ MDC.filledTextField { floatingLabel: "First name" }
  StrongLike.field @"lastName" $ lcmap (const "") $ MDC.filledTextField { floatingLabel: "Last name" }
  debounced $ Sum.do
    constant "Hello, " text
    field @"firstName" $ text
    constant " " $ text
    field @"lastName" $ text
    constant "!" $ text
    MDC.containedButton { icon: Nothing, label: Just "Enter" }
  pzero
