module CounterViewModel (countedLine, countLine, freshCount, increment) where

import Prelude ((+), (<>), show)

freshCount :: { count :: Int }
freshCount = { count: 0 }

countLine :: { count :: Int } -> String
countLine { count } = show count

increment :: { count :: Int } -> { count :: Int }
increment m = m { count = m.count + 1 }

countedLine :: { count :: Int } -> String
countedLine { count } = "Counted to " <> show count
