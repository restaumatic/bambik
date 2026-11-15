module Counter (Count, countLine, counted) where

import Prelude ((#), (+), class Applicative, show)

import PUI (PUI, fold, looped, with)
import QualifiedDo.Semigroupoid as Semigroupoid

type Count = { count :: Int }

counted :: forall m. Applicative m => PUI m Count [ "Count" :: Count ] -> PUI m {} Count
counted view =
  Semigroupoid.do
    view
    fold @"Count" increment
  # looped # with freshCount

freshCount :: Count
freshCount = { count: 0 }

countLine :: Count -> String
countLine { count } = show count

increment :: Count -> Count
increment m = m { count = m.count + 1 }
