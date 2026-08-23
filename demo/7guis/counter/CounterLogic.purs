module CounterLogic (countText, freshCount, increment) where

import Prelude ((+), show)

freshCount :: { count :: Int }
freshCount = { count: 0 }

increment :: { count :: Int } -> { count :: Int }
increment { count } = { count: count + 1 }

countText :: { count :: Int } -> String
countText { count } = show count
