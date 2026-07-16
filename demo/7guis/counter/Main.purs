module Main (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToRecord (completed)
import Effect (Effect)
import MDC as MDC
import QualifiedDo.Semigroupoid as Semigroupoid
import UI (looped, updates)
import Web (bodyWith, text)

main :: Effect Unit
main = bodyWith { count: 0 } $ MDC.elevation20 $ MDC.card { caption: Just "Counter" } $ looped Semigroupoid.do
  completed $ MDC.headline4 $ lcmap countCaption text
  updates increment $ MDC.button @"count" { label: Just "Count", icon: Nothing }

increment :: forall click. click -> { count :: Int } -> { count :: Int }
increment _ r = { count: r.count + 1 }

countCaption :: { count :: Int } -> String
countCaption r = show r.count
