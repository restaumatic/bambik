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
  , billPlacedLine
  , bills
  , billsState
  , billTotalLine
  , busyTables
  , cooking
  , dinnerService
  , dishSales
  , isLineSoldOut
  , isPlated
  , isSinglePortion
  , isSoldOut
  , isZeroed
  , itemLine
  , kitchenState
  , lineTotalLine
  , markReady
  , numberLine
  , offerLine
  , orderDish
  , orderItemsLine
  , paidOrders
  , paidState
  , paidTotalLine
  , placedLine
  , placeOrder
  , plateDish
  , portionAdded
  , portionRemoved
  , portionsZeroed
  , quantityLine
  , removalLine
  , removeDish
  , returnUnit
  , salesAmountLine
  , sendToKitchen
  , settle
  , soldAmountLine
  , soldQuantityLine
  , tableAvailability
  , tableLine
  , tableLines
  , tableState
  , takingsLine
  , ticketLine
  , ticketState
  , zeroingLine
  ) where

import Prelude (bind, const, negate, not, pure, (&&), (*), (+), (-), (/), (/=), (<), (<$>), (<>), (==), (>), (||), map, max, mod, show)

import Data.Array (all, any, concatMap, elem, filter, find, foldl, mapMaybe, null, snoc, sortWith)
import Data.DateTime.Instant (unInstant)
import Data.Int (floor, round)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String (joinWith)
import Data.Time.Duration (Milliseconds(..), Minutes(..))
import Data.Variant (default, match, on)
import Data.Variant.Case (caseText)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Now (getTimezoneOffset, now)
import Type.Proxy (Proxy(..))

type Diner =
  { "Stanowisko" :: Station
  , "Stolik" :: Table
  , dishes :: Array Dish
  , orders :: Array Order
  }

type Station =
  [ "Magazyn" :: {}
  , "Sala" :: {}
  , "Kuchnia" :: {}
  , "Wydawka" :: {}
  , "Raport" :: {}
  , "Statystyki" :: {}
  ]

type Table = [ "1" :: {}, "2" :: {}, "3" :: {}, "4" :: {}, "5" :: {}, "6" :: {} ]

type Dish = { name :: String, "Porcje" :: Portions, price :: Int }

type Portions = [ count :: Int, invalid :: String ]

type Order =
  { lines :: Array Line
  , number :: Int
  , stage :: Stage
  , table :: Table
  }

type Stage =
  [ cooking :: { placed :: Time }
  , paid :: { placed :: Time, total :: Int }
  , ready :: { placed :: Time }
  , taking :: {}
  ]

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

-- Shared copy

numberLine :: forall r. { number :: Int | r } -> String
numberLine { number } = show number

tableLine :: forall r. { table :: Table | r } -> String
tableLine { table } = caseText table

quantityLine :: forall r. { quantity :: Int | r } -> String
quantityLine { quantity } = show quantity

-- Magazyn

portionAdded :: Dish -> Dish
portionAdded = withPortions (_ + 1)

portionRemoved :: Dish -> Dish
portionRemoved = withPortions (_ - 1)

isZeroed :: Dish -> Boolean
isZeroed dish = dish."Porcje" == .count 0

portionsZeroed :: Dish -> Dish
portionsZeroed dish = dish { "Porcje" = .count 0 }

zeroingLine :: Dish -> String
zeroingLine { name } = "Wyzerować porcje dania " <> name <> "?"

-- Sala

offerLine :: Dish -> String
offerLine dish@{ price } = money price <> " · " <> availability
  where
  availability = if portionsOf dish > 0 then "dostępne: " <> show (portionsOf dish) else "niedostępne"

isSoldOut :: Dish -> Boolean
isSoldOut dish = portionsOf dish == 0

busyTables :: Diner -> Array Table
busyTables { orders } = map _.table (filter (\o -> isCooking o || isReady o) orders)

tableAvailability :: Diner -> [ busy :: {}, free :: {} ]
tableAvailability diner = if isBusy diner then .busy {} else .free {}

tableState :: Diner -> [ busy :: {}, idle :: {}, taking :: {} ]
tableState diner
  | isBusy diner = .busy {}
  | null (tableLines diner) = .idle {}
  | true = .taking {}

tableLines :: Diner -> Array TableLine
tableLines diner = maybe [] (\order -> mapMaybe priced order.lines) (takenAt diner)
  where
  priced { dish, quantity } = map (\d -> { dish, quantity, price: d.price, left: portionsOf d }) (find (\d -> d.name == dish) diner.dishes)

lineTotalLine :: TableLine -> String
lineTotalLine { price, quantity } = money (price * quantity)

isSinglePortion :: TableLine -> Boolean
isSinglePortion { quantity } = quantity == 1

isLineSoldOut :: TableLine -> Boolean
isLineSoldOut { left } = left == 0

removalLine :: TableLine -> String
removalLine { dish, quantity } = "Usunąć " <> show quantity <> " × " <> dish <> " z zamówienia?"

orderDish :: { event :: Dish, model :: Diner } -> Diner
orderDish { event: { name }, model } = withOneMore name model

addUnit :: { event :: TableLine, model :: Diner } -> Diner
addUnit { event: { dish }, model } = withOneMore dish model

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

placeOrder :: Diner -> Aff [ "Zamówienie złożone" :: { number :: Int, placed :: Time } ]
placeOrder diner = liftEffect do
  Milliseconds sinceEpoch <- unInstant <$> now
  Minutes offset <- getTimezoneOffset
  let local = mod (floor (sinceEpoch / 60000.0) - round offset) 1440
  pure (."Zamówienie złożone" { number: maybe 0 _.number (takenAt diner), placed: { hours: local / 60, minutes: mod local 60 } })

sendToKitchen :: { event :: { number :: Int, placed :: Time }, model :: Diner } -> Diner
sendToKitchen { event: { number, placed }, model: diner } = case find (\o -> o.number == number && isTaking o) diner.orders of
  Just order -> diner { orders = snoc (filter (\o -> o.number /= number) diner.orders) order { stage = .cooking { placed } } }
  Nothing -> diner

-- Kuchnia

cooking :: Diner -> Array Order
cooking { orders } = filter isCooking orders

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

plateDish :: { event :: { event :: String, model :: Order }, model :: Diner } -> Diner
plateDish { event: { event: dish, model: { number } }, model: diner } = diner
  { orders = map (\o -> if o.number == number then o { lines = map plated o.lines } else o) diner.orders }
  where
  plated l = if l.dish == dish then l { state = .plated {} } else l

markReady :: { event :: Order, model :: Diner } -> Diner
markReady { event: { number }, model } = advanced number ready model
  where
  ready = on (Proxy @"cooking") (\{ placed } -> Just (.ready { placed })) (default Nothing)

-- Wydawka

bills :: Diner -> Array Bill
bills { dishes, orders } = mapMaybe bill orders
  where
  bill { number, table, lines, stage } = on (Proxy @"ready")
    (\{ placed } -> Just { number, table, placed, total: foldl (\sum l -> sum + l.quantity * priceOf l.dish) 0 lines })
    (default Nothing)
    stage
  priceOf name = maybe 0 _.price (find (\d -> d.name == name) dishes)

billsState :: Diner -> [ pending :: {}, settled :: {} ]
billsState diner = if null (bills diner) then .settled {} else .pending {}

billPlacedLine :: Bill -> String
billPlacedLine { placed } = clockText placed

billTotalLine :: Bill -> String
billTotalLine { total } = money total

settle :: { event :: Bill, model :: Diner } -> Diner
settle { event: { number, total }, model } = advanced number paid model
  where
  paid = on (Proxy @"ready") (\{ placed } -> Just (.paid { placed, total })) (default Nothing)

-- Raport

paidOrders :: Diner -> Array Order
paidOrders { orders } = sortWith (\o -> negate o.number) (filter (\o -> isJust (paidTotal o)) orders)

paidState :: Diner -> [ none :: {}, some :: {} ]
paidState diner = if null (paidOrders diner) then .none {} else .some {}

placedLine :: Order -> String
placedLine { stage } = match
  { taking: \_ -> ""
  , cooking: \{ placed } -> clockText placed
  , ready: \{ placed } -> clockText placed
  , paid: \{ placed } -> clockText placed
  } stage

orderItemsLine :: Order -> String
orderItemsLine { lines } = joinWith ", " (map itemLine lines)

paidTotalLine :: Order -> String
paidTotalLine order = money (fromMaybe 0 (paidTotal order))

takingsLine :: Diner -> String
takingsLine diner = money (foldl (+) 0 (mapMaybe paidTotal (paidOrders diner)))

-- Statystyki

dishSales :: Diner -> Array DishSales
dishSales diner = sortWith _.dish (filter (\s -> s.quantity > 0) (map sales diner.dishes))
  where
  sold = concatMap _.lines (paidOrders diner)
  sales { name, price } =
    let quantity = foldl (+) 0 (map _.quantity (filter (\l -> l.dish == name) sold))
    in { dish: name, quantity, amount: quantity * price }

salesAmountLine :: DishSales -> String
salesAmountLine { amount } = money amount

soldAmountLine :: Diner -> String
soldAmountLine diner = money (foldl (+) 0 (map _.amount (dishSales diner)))

soldQuantityLine :: Diner -> String
soldQuantityLine diner = show (foldl (+) 0 (map _.quantity (dishSales diner)))

-- Private

withOneMore :: String -> Diner -> Diner
withOneMore name diner =
  if not (isBusy diner) && any (\d -> d.name == name && portionsOf d > 0) diner.dishes then
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

isBusy :: Diner -> Boolean
isBusy diner = elem diner."Stolik" (busyTables diner)

takenAt :: Diner -> Maybe Order
takenAt { "Stolik": table, orders } = find (\o -> o.table == table && isTaking o) orders

isTaking :: Order -> Boolean
isTaking { stage } = on (Proxy @"taking") (const true) (default false) stage

isCooking :: Order -> Boolean
isCooking { stage } = on (Proxy @"cooking") (const true) (default false) stage

isReady :: Order -> Boolean
isReady { stage } = on (Proxy @"ready") (const true) (default false) stage

paidTotal :: Order -> Maybe Int
paidTotal { stage } = on (Proxy @"paid") (\{ total } -> Just total) (default Nothing) stage

-- an advance of the order's stage as it stands in the model, if `step` allows one
advanced :: Int -> (Stage -> Maybe Stage) -> Diner -> Diner
advanced number step diner = diner { orders = map advance diner.orders }
  where
  advance o = if o.number == number then maybe o (\stage -> o { stage = stage }) (step o.stage) else o

-- unreadable or negative stock is left as it is: what is really there is unknown
portionsOf :: Dish -> Int
portionsOf dish = match { count: max 0, invalid: \_ -> 0 } dish."Porcje"

withPortions :: (Int -> Int) -> Dish -> Dish
withPortions f dish = match
  { count: \n -> if n < 0 then dish else dish { "Porcje" = .count (max 0 (f n)) }
  , invalid: \_ -> dish
  } dish."Porcje"

clockText :: Time -> String
clockText { hours, minutes } = show hours <> ":" <> pad minutes

money :: Int -> String
money cents = show (cents / 100) <> "," <> pad (mod cents 100) <> "\x00a0zł"

pad :: Int -> String
pad r = if r < 10 then "0" <> show r else show r
