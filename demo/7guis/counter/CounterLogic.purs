module CounterLogic (countLine, freshCount, increment) where

import Prelude ((+), show)

freshCount :: { count :: Int }
freshCount = { count: 0 }

countLine :: forall r1. { count :: Int | r1 } -> String
countLine { count } = show count

increment :: forall r1. { count :: Int | r1 } -> { count :: Int | r1 }
increment m = m { count = m.count + 1 }
