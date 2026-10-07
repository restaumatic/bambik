module CounterViewModel (countedLine, countLine, freshCount, increment) where

import Prelude ((+), (<>), show)

freshCount :: { counted :: Int }
freshCount = { counted: 0 }

countLine :: { counted :: Int } -> String
countLine { counted } = show counted

increment :: { counted :: Int } -> { counted :: Int }
increment m = m { counted = m.counted + 1 }

countedLine :: { counted :: Int } -> String
countedLine { counted } = "Counted to " <> show counted
