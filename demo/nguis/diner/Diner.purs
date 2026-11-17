module Diner
  ( Bill
  , Diner
  , Dish
  , DishSales
  , Line
  , Order
  , Portions
  , Stage
  , Station
  , Table
  , TableLine
  , Time
  , addUnit
  , billNumberLine
  , billPlacedLine
  , billTableLine
  , billTotalLine
  , bills
  , busyTables
  , billsState
  , cooking
  , dinnerService
  , dishSales
  , isPlated
  , isSoldOut
  , noneLeft
  , itemLine
  , kitchenState
  , lineTotalLine
  , markReady
  , menuCard
  , offerFace
  , offerLine
  , orderDish
  , orderItemsLine
  , orderNumberLine
  , orderTableLine
  , paidOrders
  , paidState
  , paidTotalLine
  , placeOrder
  , placedLine
  , portionAdded
  , portionRemoved
  , plateLine
  , quantityLine
  , salesAmountLine
  , salesQuantityLine
  , soldQuantityLine
  , removalLine
  , removeDish
  , returnUnit
  , sendToKitchen
  , settle
  , tableLines
  , tableState
  , takingsLine
  , ticketLine
  , ticketState
  ) where

import Prelude (bind, pure, (<$>), (&&), (||), (*), (+), (-), (/), (/=), (<), (<>), (==), (>), map, max, mod, show)

import Data.Array (all, any, concatMap, filter, find, foldl, mapMaybe, null, reverse, snoc, sortWith)
import Data.DateTime.Instant (unInstant)
import Data.Int (floor, round)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String (joinWith)
import Data.Variant (match)
import Data.Variant.Case (caseText)
import Data.Time.Duration (Milliseconds(..), Minutes(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Now (getTimezoneOffset, now)

type Diner =
  { "Stanowisko" :: Station
  , "Stolik" :: Table
  , dishes :: Array Dish
  , orders :: Array Order
  }

type Station = [ "Magazyn" :: {}, "Sala" :: {}, "Kuchnia" :: {}, "Wydawka" :: {}, "Raport" :: {}, "Statystyki" :: {} ]

type Table = [ "1" :: {}, "2" :: {}, "3" :: {}, "4" :: {}, "5" :: {}, "6" :: {} ]

type Dish = { name :: String, "Porcje" :: Portions, price :: Int }

type Portions = [ count :: Int, invalid :: String ]

type Order =
  { lines :: Array Line
  , number :: Int
  , stage :: Stage
  , table :: Table
  }

type Stage = [ cooking :: { placed :: Time }, paid :: { placed :: Time, total :: Int }, ready :: { placed :: Time }, taking :: {} ]

type Time = { hours :: Int, minutes :: Int }

type Line = { dish :: String, quantity :: Int, state :: [ ordered :: {}, plated :: {} ] }

type TableLine = { dish :: String, left :: Int, price :: Int, quantity :: Int }

type DishSales = { amount :: Int, dish :: String, quantity :: Int }

type Bill = { number :: Int, placed :: Time, table :: Table, total :: Int }

dinnerService :: Diner
dinnerService =
  { "Stanowisko": ."Sala" {}
  , "Stolik": ."1" {}
  , dishes:
    [ { name: "Carbonara", price: 1050, "Porcje": .count 6 }
    , { name: "Margherita", price: 950, "Porcje": .count 8 }
    , { name: "Risotto", price: 1200, "Porcje": .count 3 }
    , { name: "Tiramisu", price: 650, "Porcje": .count 2 }
    ]
  , orders:
    [ { number: 1
      , table: ."2" {}
      , stage: .paid { placed: { hours: 8, minutes: 5 }, total: 2750 }
      , lines:
        [ { dish: "Carbonara", quantity: 2, state: .plated {} }
        , { dish: "Tiramisu", quantity: 1, state: .plated {} }
        ]
      }
    , { number: 2
      , table: ."5" {}
      , stage: .paid { placed: { hours: 8, minutes: 20 }, total: 2150 }
      , lines:
        [ { dish: "Risotto", quantity: 1, state: .plated {} }
        , { dish: "Margherita", quantity: 1, state: .plated {} }
        ]
      }
    , { number: 3
      , table: ."1" {}
      , stage: .paid { placed: { hours: 8, minutes: 41 }, total: 2850 }
      , lines:
        [ { dish: "Margherita", quantity: 3, state: .plated {} } ]
      }
    , { number: 4
      , table: ."3" {}
      , stage: .cooking { placed: { hours: 8, minutes: 58 } }
      , lines:
        [ { dish: "Margherita", quantity: 2, state: .plated {} }
        , { dish: "Tiramisu", quantity: 1, state: .ordered {} }
        ]
      }
    , { number: 5
      , table: ."4" {}
      , stage: .ready { placed: { hours: 9, minutes: 4 } }
      , lines:
        [ { dish: "Carbonara", quantity: 1, state: .plated {} }
        , { dish: "Risotto", quantity: 1, state: .plated {} }
        ]
      }
    ]
  }

menuCard :: Diner -> Array Dish
menuCard { dishes } = dishes

offerLine :: Dish -> String
offerLine dish@{ price } = money price <> " · " <> availability
  where
  availability = if portionsOf dish > 0 then "dostępne: " <> show (portionsOf dish) else "niedostępne"

isSoldOut :: Dish -> Boolean
isSoldOut dish = portionsOf dish == 0

noneLeft :: TableLine -> Boolean
noneLeft { left } = left == 0

offerFace :: Dish -> String
offerFace dish = if portionsOf dish > 0 then "" else "opacity: 0.38;"

tableLines :: Diner -> Array TableLine
tableLines diner = maybe [] (\order -> mapMaybe priced order.lines) (takenAt diner)
  where
  priced { dish, quantity } = map (\d -> { dish, quantity, price: d.price, left: portionsOf d }) (find (\d -> d.name == dish) diner.dishes)

quantityLine :: TableLine -> String
quantityLine { quantity } = show quantity

lineTotalLine :: TableLine -> String
lineTotalLine { price, quantity } = money (price * quantity)

busyTables :: Diner -> Array Table
busyTables { orders } = map _.table (filter (\o -> phaseOf o == .cooking {} || phaseOf o == .ready {}) orders)

tableState :: Diner -> [ idle :: {}, taking :: {} ]
tableState diner = if null (tableLines diner) then .idle {} else .taking {}

cooking :: Diner -> Array Order
cooking { orders } = sortWith placedMinutes (filter (\o -> phaseOf o == .cooking {}) orders)

kitchenState :: Diner -> [ busy :: {}, cleared :: {} ]
kitchenState diner = if null (cooking diner) then .cleared {} else .busy {}

ticketLine :: Order -> String
ticketLine order@{ number, table } = "#" <> show number <> " · Stolik " <> caseText table <> " · " <> placedLine order

itemLine :: Line -> String
itemLine { dish, quantity } = show quantity <> " × " <> dish

isPlated :: Line -> Boolean
isPlated { state } = state == .plated {}

ticketState :: Order -> [ plated :: {}, plating :: {} ]
ticketState { lines } = if all isPlated lines then .plated {} else .plating {}

bills :: Diner -> Array Bill
bills { dishes, orders } = mapMaybe bill orders
  where
  bill { number, table, lines, stage } = match
    { ready: \{ placed } -> Just { number, table, placed, total: foldl (\sum l -> sum + l.quantity * priceOf l.dish) 0 lines }
    , taking: \_ -> Nothing
    , cooking: \_ -> Nothing
    , paid: \_ -> Nothing
    } stage
  priceOf name = maybe 0 _.price (find (\d -> d.name == name) dishes)

billNumberLine :: Bill -> String
billNumberLine { number } = show number

billTableLine :: Bill -> String
billTableLine { table } = caseText table

billPlacedLine :: Bill -> String
billPlacedLine { placed } = clockText placed

billTotalLine :: Bill -> String
billTotalLine { total } = money total

billsState :: Diner -> [ pending :: {}, settled :: {} ]
billsState diner = if null (bills diner) then .settled {} else .pending {}

paidOrders :: Diner -> Array Order
paidOrders { orders } = reverse (filter (\o -> isJust (paidTotal o)) orders)

paidState :: Diner -> [ none :: {}, some :: {} ]
paidState diner = if null (paidOrders diner) then .none {} else .some {}

orderNumberLine :: Order -> String
orderNumberLine { number } = show number

orderTableLine :: Order -> String
orderTableLine { table } = caseText table

orderItemsLine :: Order -> String
orderItemsLine { lines } = joinWith ", " (map itemLine lines)

placedLine :: Order -> String
placedLine { stage } = match
  { taking: \_ -> ""
  , cooking: \{ placed } -> clockText placed
  , ready: \{ placed } -> clockText placed
  , paid: \{ placed } -> clockText placed
  } stage

paidTotalLine :: Order -> String
paidTotalLine order = money (fromMaybe 0 (paidTotal order))

dishSales :: Diner -> Array DishSales
dishSales diner = sortWith _.dish (filter (\s -> s.quantity > 0) (map sales diner.dishes))
  where
  sold = concatMap _.lines (paidOrders diner)
  sales { name, price } =
    let quantity = foldl (+) 0 (map _.quantity (filter (\l -> l.dish == name) sold))
    in { dish: name, quantity, amount: quantity * price }

salesQuantityLine :: DishSales -> String
salesQuantityLine { quantity } = show quantity

salesAmountLine :: DishSales -> String
salesAmountLine { amount } = money amount

soldQuantityLine :: Diner -> String
soldQuantityLine diner = show (foldl (+) 0 (map _.quantity (dishSales diner)))

takingsLine :: Diner -> String
takingsLine diner = money (foldl (+) 0 (mapMaybe paidTotal (paidOrders diner)))

paidTotal :: Order -> Maybe Int
paidTotal { stage } = match
  { paid: \{ total } -> Just total
  , taking: \_ -> Nothing
  , cooking: \_ -> Nothing
  , ready: \_ -> Nothing
  } stage

orderDish :: { event :: Dish, model :: Diner } -> Diner
orderDish { event: { name }, model } = withOneMore name model

withOneMore :: String -> Diner -> Diner
withOneMore name diner =
  if any (\d -> d.name == name && portionsOf d > 0) diner.dishes then
    diner
      { dishes = map (\d -> if d.name == name then withPortions (_ - 1) d else d) diner.dishes
      , orders = case takenAt diner of
        Just order -> map (\o -> if o.number == order.number then o { lines = oneMore o.lines } else o) diner.orders
        Nothing -> snoc diner.orders
          { number: foldl max 0 (map _.number diner.orders) + 1
          , table: diner."Stolik"
          , stage: .taking {}
          , lines: oneMore []
          }
      }
  else diner
  where
  oneMore lines =
    if any (\l -> l.dish == name) lines then map (\l -> if l.dish == name then l { quantity = l.quantity + 1 } else l) lines
    else snoc lines { dish: name, quantity: 1, state: .ordered {} }

returnUnit :: { event :: TableLine, model :: Diner } -> Diner
returnUnit { event: { dish: name }, model: diner } = case takenAt diner of
  Just order -> diner
    { dishes = map (\d -> if d.name == name then withPortions (_ + 1) d else d) diner.dishes
    , orders = mapMaybe (\o -> if o.number == order.number then fewer o else Just o) diner.orders
    }
  Nothing -> diner
  where
  fewer o = case mapMaybe oneFewer o.lines of
    [] -> Nothing
    lines -> Just o { lines = lines }
  oneFewer l
    | l.dish /= name = Just l
    | l.quantity < 2 = Nothing
    | true = Just l { quantity = l.quantity - 1 }

addUnit :: { event :: TableLine, model :: Diner } -> Diner
addUnit { event: { dish }, model } = withOneMore dish model

removalLine :: TableLine -> String
removalLine { dish, quantity } = "Usunąć " <> show quantity <> " × " <> dish <> " z zamówienia?"

removeDish :: { event :: TableLine, model :: Diner } -> Diner
removeDish { event: { dish: name }, model: diner } = case takenAt diner of
  Just order -> diner
    { dishes = map (\d -> if d.name == name then withPortions (_ + ordered order) d else d) diner.dishes
    , orders = mapMaybe (\o -> if o.number == order.number then without o else Just o) diner.orders
    }
  Nothing -> diner
  where
  ordered order = maybe 0 _.quantity (find (\l -> l.dish == name) order.lines)
  without o = case filter (\l -> l.dish /= name) o.lines of
    [] -> Nothing
    lines -> Just o { lines = lines }

portionAdded :: Dish -> Dish
portionAdded = withPortions (_ + 1)

portionRemoved :: Dish -> Dish
portionRemoved = withPortions (_ - 1)

placeOrder :: Diner -> Aff [ "Zamówienie złożone" :: Time ]
placeOrder _ = liftEffect do
  Milliseconds sinceEpoch <- unInstant <$> now
  Minutes offset <- getTimezoneOffset
  let local = mod (floor (sinceEpoch / 60000.0) - round offset) 1440
  pure (."Zamówienie złożone" { hours: local / 60, minutes: mod local 60 })

sendToKitchen :: { event :: Time, model :: Diner } -> Diner
sendToKitchen { event: placed, model: diner } = case takenAt diner of
  Just order -> staged order.number (.cooking { placed }) diner
  Nothing -> diner

plateLine :: { event :: { event :: String, model :: Order }, model :: Diner } -> Diner
plateLine { event: { event: dish, model: { number } }, model: diner } = diner
  { orders = map (\o -> if o.number == number then o { lines = map plated o.lines } else o) diner.orders }
  where
  plated l = if l.dish == dish then l { state = .plated {} } else l

markReady :: { event :: Order, model :: Diner } -> Diner
markReady { event: { number, stage }, model: diner } = match
  { cooking: \{ placed } -> staged number (.ready { placed }) diner
  , taking: \_ -> diner
  , ready: \_ -> diner
  , paid: \_ -> diner
  } stage

settle :: { event :: Bill, model :: Diner } -> Diner
settle { event: { number, placed, total }, model: diner } = staged number (.paid { placed, total }) diner

takenAt :: Diner -> Maybe Order
takenAt { "Stolik": table, orders } = find (\o -> o.table == table && phaseOf o == .taking {}) orders

placedMinutes :: Order -> Int
placedMinutes { stage } = match
  { taking: \_ -> 0
  , cooking: \{ placed } -> minutesOf placed
  , ready: \{ placed } -> minutesOf placed
  , paid: \{ placed } -> minutesOf placed
  } stage
  where
  minutesOf { hours, minutes } = hours * 60 + minutes

phaseOf :: Order -> [ cooking :: {}, paid :: {}, ready :: {}, taking :: {} ]
phaseOf { stage } = match
  { taking: \_ -> .taking {}
  , cooking: \_ -> .cooking {}
  , ready: \_ -> .ready {}
  , paid: \_ -> .paid {}
  } stage

portionsOf :: Dish -> Int
portionsOf dish = match { count: max 0, invalid: \_ -> 0 } dish."Porcje"

withPortions :: (Int -> Int) -> Dish -> Dish
withPortions f dish = dish { "Porcje" = .count (max 0 (f (portionsOf dish))) }

staged :: Int -> Stage -> Diner -> Diner
staged number stage diner = diner { orders = map (\o -> if o.number == number then o { stage = stage } else o) diner.orders }

clockText :: Time -> String
clockText { hours, minutes } = show hours <> ":" <> pad minutes

money :: Int -> String
money cents = show (cents / 100) <> "," <> pad (mod cents 100) <> "\x00a0zł"

pad :: Int -> String
pad r = if r < 10 then "0" <> show r else show r
