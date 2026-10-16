module Counter (Count, countLine, freshCount, increment) where

import Prelude ((+), show)

type Count = { counted :: Int }

freshCount :: Count
freshCount = { counted: 0 }

countLine :: Count -> String
countLine { counted } = show counted

increment :: Count -> Count
increment m = m { counted = m.counted + 1 }
