module OrderDashboard (Day, Order, Window, kitchenLoad, openingDay, orderFlow, ordersArrive, ordersCount, revenue, tickPeriod, topDishes) where

import Prelude (compare, min, mod, negate, show, (*), (+), (-), (/), (<$>), (==), (>), (>=))

import Data.Array (filter, index, length, mapMaybe, range, snoc, sortBy, take)
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Map (fromFoldableWith, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Number.Format (fixed, toStringWith)
import Data.Tuple (Tuple(..))
import Data.Variant (match)

type Day = { "Showing" :: Window, orders :: Array Order, tick :: Int }

type Order = { at :: Int, dish :: String, total :: Number }

type Window = [ "Last 15 min" :: {}, "Last minute" :: {}, "Since open" :: {} ]

openingDay :: Day
openingDay = { tick: 0, orders: mapMaybe arrival (range openingTick 0), "Showing": ."Last 15 min" {} }

tickPeriod :: { ms :: Number }
tickPeriod = { ms: 1000.0 }

ordersArrive :: Day -> Day
ordersArrive day@{ tick, orders } = day
  { tick = tick + 1
  , orders = case arrival (tick + 1) of
    Just order -> snoc orders order
    Nothing -> orders
  }

arrival :: Int -> Maybe Order
arrival t = case pseudo t `mod` 3 of
  0 -> case index menu (pseudo (t + 17) `mod` length menu) of
    Just dish -> Just { dish: dish.name, total: dish.price * toNumber (1 + pseudo (t + 5) `mod` 3), at: t }
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

openingTick :: Int
openingTick = -900

windowStart :: Window -> Int -> Int
windowStart window tick = match
  { "Last minute": \_ -> tick - 59
  , "Last 15 min": \_ -> tick - 899
  , "Since open": \_ -> openingTick
  } window

inWindow :: Day -> Array Order
inWindow { orders, "Showing": window, tick } = filter (\o -> o.at >= windowStart window tick) orders

ordersCount :: Day -> String
ordersCount m = show (length (inWindow m))

revenue :: Day -> String
revenue m = toStringWith (fixed 2) (sum (_.total <$> inWindow m))

kitchenLoad :: Day -> Number
kitchenLoad { orders, tick } = min 1.0 (toNumber (length (filter (\o -> o.at > tick - prepTime) orders)) / kitchenCapacity)

orderFlow :: Day -> Array Number
orderFlow m@{ "Showing": window, tick } =
  let start = windowStart window tick
      bucketOf o = (o.at - start) * trendBuckets / (tick - start + 1)
      bucket i = toNumber (length (filter (\o -> bucketOf o == i) (inWindow m)))
  in bucket <$> range 0 (trendBuckets - 1)

topDishes :: Day -> Array { name :: String, score :: String }
topDishes m = take 5 (entry <$> sortBy byCountDescending counts)
  where
  counts = toUnfoldable (fromFoldableWith (+) ((\o -> Tuple o.dish 1) <$> inWindow m))
  byCountDescending (Tuple _ a) (Tuple _ b) = compare b a
  entry (Tuple name count) = { name, score: show (count :: Int) }

prepTime :: Int
prepTime = 45

kitchenCapacity :: Number
kitchenCapacity = 24.0

trendBuckets :: Int
trendBuckets = 12
