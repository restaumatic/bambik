module Main (main) where

import Prelude

import Data.Lens.Extra.Commons (field)
import Data.Profunctor (lcmap)
import Data.Profunctor.StrongLike (field) as StrongLike
import Data.Profunctor.Zero (pzero)
import Effect (Effect)
import MDC as MDC
import QualifiedDo.Semigroup as Semigroup
import QualifiedDo.Semigroupoid as Semigroupoid
import Web (body, staticText, text)

main :: Effect Unit
main = body @(Record ()) $ Semigroupoid.do
  -- staticText "Hello, World!"
  StrongLike.field @"firstName" $ lcmap (const "") $ MDC.filledTextField { floatingLabel: "First name" }
  StrongLike.field @"lastName" $ lcmap (const "") $ MDC.filledTextField { floatingLabel: "Last name" }
  field @"firstName" $ text
  field @"lastName" $ text
  pzero
