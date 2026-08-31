module CounterLogic (freshCount, increment, presentCounter) where

import Prelude ((+), show)

freshCount :: { count :: Int, countText :: String }
freshCount = presentCounter { count: 0, countText: "" }

presentCounter :: { count :: Int, countText :: String } -> { count :: Int, countText :: String }
presentCounter r = r { countText = show r.count }

increment :: { count :: Int } -> { count :: Int }
increment { count } = { count: count + 1 }
