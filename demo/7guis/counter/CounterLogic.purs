module CounterLogic (freshCount, increment) where

import Prelude ((+))

freshCount :: { count :: Int }
freshCount = { count: 0 }

increment :: { count :: Int } -> { count :: Int }
increment { count } = { count: count + 1 }
