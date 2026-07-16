module Main (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap)
import Effect (Effect)
import PUI (looped, updates, with)
import PUI.Data.Profunctor.Row.RecordToRecord (completed)
import PUI.MDC as MDC
import PUI.Web (body, text)
import QualifiedDo.Semigroupoid as Semigroupoid

main :: Effect Unit
main = body $ MDC.elevation20 $ MDC.card { caption: Just "Counter" } $ looped $ with { count: 0 } Semigroupoid.do
  completed $ MDC.headline4 $ lcmap countCaption text
  updates increment $ MDC.button @"count" { label: Just "Count", icon: Nothing }

increment :: forall click. click -> { count :: Int } -> { count :: Int }
increment _ r = { count: r.count + 1 }

countCaption :: { count :: Int } -> String
countCaption r = show r.count
