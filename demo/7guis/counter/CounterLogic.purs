module CounterLogic (countLine, freshCount, increment) where

import Prelude ((+), show)

freshCount :: { count :: Int }
freshCount = { count: 0 }

countLine :: { count :: Int } -> String
countLine { count } = show count

increment :: { count :: Int } -> { count :: Int }
increment { count } = { count: count + 1 }
