module OrderDashboardMDC3 (orderDashboardMDC3) where

import Prelude (Unit, compare, max, min, mod, negate, show, ($), (#), (&&), (*), (+), (-), (/), (<), (<$>), (>), (>=))

import Data.Array (filter, index, length, mapMaybe, range, snoc, sortBy, take)
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Map (fromFoldableWith, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Number.Format (fixed, toStringWith)
import Data.Tuple (Tuple(..))
import Data.Variant (match)
import DashboardControlsMDC3 (board, gauge, leaderboard, rangePicker, statTile, trendChart)
import Effect (Effect)
import PUI (asField, completed, displayed, every, mvu, projected, required)
import PUI.Web.HTML (body)
import PUI.Web.MDC3 (elevation5, topAppBar)
import QualifiedDo.Semigroupoid as Semigroupoid

orderDashboardMDC3 :: Effect Unit
orderDashboardMDC3 =
  body $
    elevation5 $
      topAppBar { title: "Order Dashboard" } $ ( Semigroupoid.do
          every tickPeriod ordersArrive
          rangePicker { label: "Showing" }
            [ { value: .lastMinute {}, label: "Last minute" }
            , { value: .lastQuarter {}, label: "Last 15 min" }
            , { value: .sinceOpen {}, label: "Since open" }
            ] # required # asField @"window" # completed
          board $ Semigroupoid.do
            statTile { label: "Orders", unit: "placed" } # projected ordersCount # displayed
            statTile { label: "Revenue", unit: "EUR" } # projected revenue # displayed
            gauge { label: "Kitchen load" } # projected kitchenLoad # displayed
            trendChart { label: "Order flow" } # projected orderFlow # displayed
            leaderboard { label: "Top dishes" } # projected topDishes # displayed
      ) # mvu openingDay

openingDay :: { tick :: Int, orders :: Array { id :: Int, dish :: String, total :: Number, at :: Int }, window :: [ lastMinute :: {}, lastQuarter :: {}, sinceOpen :: {} ] }
openingDay = { tick: 0, orders: mapMaybe arrival (range openingTick 0), window: .lastQuarter {} }

ordersArrive :: { tick :: Int, orders :: Array { id :: Int, dish :: String, total :: Number, at :: Int } } -> Maybe { tick :: Int, orders :: Array { id :: Int, dish :: String, total :: Number, at :: Int } }
ordersArrive { tick, orders } = Just
  { tick: tick + 1
  , orders: case arrival (tick + 1) of
      Just order -> snoc orders order
      Nothing -> orders
  }

arrival :: Int -> Maybe { id :: Int, dish :: String, total :: Number, at :: Int }
arrival t = case pseudo t `mod` 3 of
  0 -> case index menu (pseudo (t + 17) `mod` length menu) of
    Just dish -> Just { id: t, dish: dish.name, total: dish.price * toNumber (1 + pseudo (t + 5) `mod` 3), at: t }
    Nothing -> Nothing
  _ -> Nothing

pseudo :: Int -> Int
pseudo n = (n * 733 + 379) `mod` 997

menu :: Array { name :: String, price :: Number }
menu =
  [ { name: "Margherita", price: 9.5 }
  , { name: "Pad Thai", price: 11.0 }
  , { name: "Ramen", price: 12.5 }
  , { name: "Burrito", price: 8.5 }
  , { name: "Poke Bowl", price: 12.0 }
  , { name: "Carbonara", price: 10.5 }
  , { name: "Falafel Wrap", price: 7.5 }
  ]

tickPeriod :: { ms :: Number }
tickPeriod = { ms: 1000.0 }

openingTick :: Int
openingTick = -900

windowStart :: [ lastMinute :: {}, lastQuarter :: {}, sinceOpen :: {} ] -> Int -> Int
windowStart window tick = match { lastMinute: \_ -> tick - 60, lastQuarter: \_ -> tick - 900, sinceOpen: \_ -> openingTick } window

inWindow :: { orders :: Array { id :: Int, dish :: String, total :: Number, at :: Int }, window :: [ lastMinute :: {}, lastQuarter :: {}, sinceOpen :: {} ], tick :: Int } -> Array { id :: Int, dish :: String, total :: Number, at :: Int }
inWindow { orders, window, tick } = filter (\o -> o.at >= windowStart window tick) orders

ordersCount :: { orders :: Array { id :: Int, dish :: String, total :: Number, at :: Int }, window :: [ lastMinute :: {}, lastQuarter :: {}, sinceOpen :: {} ], tick :: Int } -> String
ordersCount m = show (length (inWindow m))

revenue :: { orders :: Array { id :: Int, dish :: String, total :: Number, at :: Int }, window :: [ lastMinute :: {}, lastQuarter :: {}, sinceOpen :: {} ], tick :: Int } -> String
revenue m = toStringWith (fixed 2) (sum (_.total <$> inWindow m))

kitchenLoad :: { orders :: Array { id :: Int, dish :: String, total :: Number, at :: Int }, tick :: Int } -> Number
kitchenLoad { orders, tick } = min 1.0 (toNumber (length (filter (\o -> o.at > tick - prepTime) orders)) / kitchenCapacity)

orderFlow :: { orders :: Array { id :: Int, dish :: String, total :: Number, at :: Int }, window :: [ lastMinute :: {}, lastQuarter :: {}, sinceOpen :: {} ], tick :: Int } -> Array Number
orderFlow m@{ window, tick } =
  let start = windowStart window tick
      width = max 1 ((tick - start) / trendBuckets)
      recent = inWindow m
      bucket i = toNumber (length (filter (\o -> o.at >= start + i * width && o.at < start + (i + 1) * width) recent))
  in bucket <$> range 0 (trendBuckets - 1)

topDishes :: { orders :: Array { id :: Int, dish :: String, total :: Number, at :: Int }, window :: [ lastMinute :: {}, lastQuarter :: {}, sinceOpen :: {} ], tick :: Int } -> Array { name :: String, score :: String }
topDishes m = take 5 ((\(Tuple name count) -> { name, score: show (count :: Int) }) <$> sortBy (\(Tuple _ a) (Tuple _ b) -> compare b a) (toUnfoldable (fromFoldableWith (+) ((\o -> Tuple o.dish 1) <$> inWindow m))))

prepTime :: Int
prepTime = 45

kitchenCapacity :: Number
kitchenCapacity = 24.0

trendBuckets :: Int
trendBuckets = 12
