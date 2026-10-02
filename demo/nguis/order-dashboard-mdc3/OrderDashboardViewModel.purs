module OrderDashboardViewModel (kitchenLoad, openingDay, orderFlow, ordersArrive, ordersCount, revenue, tickPeriod, topDishes) where

import Prelude (compare, max, min, mod, negate, show, ($), (&&), (*), (+), (-), (/), (<), (<$>), (>), (>=))

import Data.Array (filter, index, length, mapMaybe, range, snoc, sortBy, take)
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Map (fromFoldableWith, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Number.Format (fixed, toStringWith)
import Data.Tuple (Tuple(..))
import Data.Variant (match)

openingDay :: { tick :: Int, orders :: Array { id :: Int, dish :: String, total :: Number, at :: Int }, "Showing" :: [ "Last minute" :: {}, "Last 15 min" :: {}, "Since open" :: {} ] }
openingDay = { tick: 0, orders: mapMaybe arrival (range openingTick 0), "Showing": ."Last 15 min" {} }

tickPeriod :: { ms :: Number }
tickPeriod = { ms: 1000.0 }

ordersArrive :: forall r1. { tick :: Int, orders :: Array { id :: Int, dish :: String, total :: Number, at :: Int } | r1 } -> Maybe { tick :: Int, orders :: Array { id :: Int, dish :: String, total :: Number, at :: Int } | r1 }
ordersArrive day@{ tick, orders } = Just $ day
  { tick = tick + 1
  , orders = case arrival (tick + 1) of
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

openingTick :: Int
openingTick = -900

windowStart :: [ "Last minute" :: {}, "Last 15 min" :: {}, "Since open" :: {} ] -> Int -> Int
windowStart window tick = match { "Last minute": \_ -> tick - 60, "Last 15 min": \_ -> tick - 900, "Since open": \_ -> openingTick } window

inWindow :: forall r1. { orders :: Array { id :: Int, dish :: String, total :: Number, at :: Int }, "Showing" :: [ "Last minute" :: {}, "Last 15 min" :: {}, "Since open" :: {} ], tick :: Int | r1 } -> Array { id :: Int, dish :: String, total :: Number, at :: Int }
inWindow { orders, "Showing": window, tick } = filter (\o -> o.at >= windowStart window tick) orders

ordersCount :: forall r1. { orders :: Array { id :: Int, dish :: String, total :: Number, at :: Int }, "Showing" :: [ "Last minute" :: {}, "Last 15 min" :: {}, "Since open" :: {} ], tick :: Int | r1 } -> String
ordersCount m = show (length (inWindow m))

revenue :: forall r1. { orders :: Array { id :: Int, dish :: String, total :: Number, at :: Int }, "Showing" :: [ "Last minute" :: {}, "Last 15 min" :: {}, "Since open" :: {} ], tick :: Int | r1 } -> String
revenue m = toStringWith (fixed 2) (sum (_.total <$> inWindow m))

kitchenLoad :: forall r1. { orders :: Array { id :: Int, dish :: String, total :: Number, at :: Int }, tick :: Int | r1 } -> Number
kitchenLoad { orders, tick } = min 1.0 (toNumber (length (filter (\o -> o.at > tick - prepTime) orders)) / kitchenCapacity)

orderFlow :: forall r1. { orders :: Array { id :: Int, dish :: String, total :: Number, at :: Int }, "Showing" :: [ "Last minute" :: {}, "Last 15 min" :: {}, "Since open" :: {} ], tick :: Int | r1 } -> Array Number
orderFlow m@{ "Showing": window, tick } =
  let start = windowStart window tick
      width = max 1 ((tick - start) / trendBuckets)
      recent = inWindow m
      bucket i = toNumber (length (filter (\o -> o.at >= start + i * width && o.at < start + (i + 1) * width) recent))
  in bucket <$> range 0 (trendBuckets - 1)

topDishes :: forall r1. { orders :: Array { id :: Int, dish :: String, total :: Number, at :: Int }, "Showing" :: [ "Last minute" :: {}, "Last 15 min" :: {}, "Since open" :: {} ], tick :: Int | r1 } -> Array { name :: String, score :: String }
topDishes m = take 5 ((\(Tuple name count) -> { name, score: show (count :: Int) }) <$> sortBy (\(Tuple _ a) (Tuple _ b) -> compare b a) (toUnfoldable (fromFoldableWith (+) ((\o -> Tuple o.dish 1) <$> inWindow m))))

prepTime :: Int
prepTime = 45

kitchenCapacity :: Number
kitchenCapacity = 24.0

trendBuckets :: Int
trendBuckets = 12
