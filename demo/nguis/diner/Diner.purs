module Diner
  ( Bill
  , Diner
  , Dish
  , Line
  , Order
  , Portions
  , Stage
  , Station
  , Table
  , TableLine
  , billLine
  , bills
  , billsState
  , cooking
  , dinnerService
  , isPlated
  , itemLine
  , kitchenState
  , lineTotalLine
  , markReady
  , menuCard
  , offerLine
  , orderDish
  , orderItemsLine
  , orderNumberLine
  , orderTableLine
  , paidOrders
  , paidState
  , paidTotalLine
  , portionAdded
  , portionRemoved
  , plateLine
  , quantityLine
  , returnUnit
  , sendToKitchen
  , settle
  , tableLine
  , tableLines
  , tableState
  , takingsLine
  , ticketLine
  , ticketState
  ) where

import Prelude ((&&), (*), (+), (-), (/), (/=), (<), (<>), (==), (>), map, max, mod, show)

import Data.Int (floor, toNumber)
import Data.Array (all, any, filter, find, foldl, length, mapMaybe, null, snoc)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String (joinWith)
import Data.Variant (match)
import Data.Variant.Case (caseText)

type Diner =
  { "Stanowisko" :: Station
  , "Stolik" :: Table
  , dishes :: Array Dish
  , orders :: Array Order
  }

type Station = [ "Magazyn" :: {}, "Sala" :: {}, "Kuchnia" :: {}, "Wydawka" :: {}, "Raport" :: {} ]

type Table =
  [ "Stolik 1" :: {}, "Stolik 2" :: {}, "Stolik 3" :: {}, "Stolik 4" :: {}, "Stolik 5" :: {}, "Stolik 6" :: {} ]

type Dish = { name :: String, "Porcje" :: Portions, price :: Int }

type Portions = [ count :: Number, invalid :: String ]

type Order =
  { lines :: Array Line
  , number :: Int
  , stage :: Stage
  , table :: Table
  }

type Stage = [ cooking :: {}, paid :: { total :: Int }, ready :: {}, taking :: {} ]

type Line = { dish :: String, quantity :: Int, state :: [ ordered :: {}, plated :: {} ] }

type TableLine = { dish :: String, price :: Int, quantity :: Int }

type Bill = { number :: Int, table :: Table, total :: Int }

dinnerService :: Diner
dinnerService =
  { "Stanowisko": ."Sala" {}
  , "Stolik": ."Stolik 1" {}
  , dishes:
    [ { name: "Carbonara", price: 1050, "Porcje": .count 6.0 }
    , { name: "Margherita", price: 950, "Porcje": .count 8.0 }
    , { name: "Risotto", price: 1200, "Porcje": .count 3.0 }
    , { name: "Tiramisu", price: 650, "Porcje": .count 2.0 }
    ]
  , orders:
    [ { number: 1
      , table: ."Stolik 3" {}
      , stage: .cooking {}
      , lines:
        [ { dish: "Margherita", quantity: 2, state: .plated {} }
        , { dish: "Tiramisu", quantity: 1, state: .ordered {} }
        ]
      }
    ]
  }

menuCard :: Diner -> Array Dish
menuCard { dishes } = filter (\d -> portionsOf d > 0) dishes

offerLine :: Dish -> String
offerLine dish@{ name, price } = name <> " · " <> money price <> " · dostępne: " <> show (portionsOf dish)

tableLines :: Diner -> Array TableLine
tableLines diner = maybe [] (\order -> mapMaybe priced order.lines) (takenAt diner)
  where
  priced { dish, quantity } = map (\d -> { dish, quantity, price: d.price }) (find (\d -> d.name == dish) diner.dishes)

quantityLine :: TableLine -> String
quantityLine { quantity } = show quantity

lineTotalLine :: TableLine -> String
lineTotalLine { price, quantity } = money (price * quantity)

tableLine :: Diner -> String
tableLine diner = caseText diner."Stolik" <> " · " <> case tableLines diner of
  [] -> "nic jeszcze nie zamówiono"
  lines -> money (foldl (\sum l -> sum + l.price * l.quantity) 0 lines)

tableState :: Diner -> [ idle :: {}, taking :: {} ]
tableState diner = if null (tableLines diner) then .idle {} else .taking {}

cooking :: Diner -> Array Order
cooking { orders } = filter (\o -> o.stage == .cooking {}) orders

kitchenState :: Diner -> [ busy :: {}, cleared :: {} ]
kitchenState diner = if null (cooking diner) then .cleared {} else .busy {}

ticketLine :: Order -> String
ticketLine { number, table } = "#" <> show number <> " · " <> caseText table

itemLine :: Line -> String
itemLine { dish, quantity } = show quantity <> " × " <> dish

isPlated :: Line -> Boolean
isPlated { state } = state == .plated {}

ticketState :: Order -> [ plated :: {}, plating :: {} ]
ticketState { lines } = if all isPlated lines then .plated {} else .plating {}

bills :: Diner -> Array Bill
bills { dishes, orders } = map bill (filter (\o -> o.stage == .ready {}) orders)
  where
  bill { number, table, lines } = { number, table, total: foldl (\sum l -> sum + l.quantity * priceOf l.dish) 0 lines }
  priceOf name = maybe 0 _.price (find (\d -> d.name == name) dishes)

billLine :: Bill -> String
billLine { number, table, total } = "#" <> show number <> " · " <> caseText table <> " · " <> money total

billsState :: Diner -> [ pending :: {}, settled :: {} ]
billsState diner = if null (bills diner) then .settled {} else .pending {}

paidOrders :: Diner -> Array Order
paidOrders { orders } = filter (\o -> isJust (paidTotal o)) orders

paidState :: Diner -> [ none :: {}, some :: {} ]
paidState diner = if null (paidOrders diner) then .none {} else .some {}

orderNumberLine :: Order -> String
orderNumberLine { number } = "#" <> show number

orderTableLine :: Order -> String
orderTableLine { table } = caseText table

orderItemsLine :: Order -> String
orderItemsLine { lines } = joinWith ", " (map itemLine lines)

paidTotalLine :: Order -> String
paidTotalLine order = money (fromMaybe 0 (paidTotal order))

takingsLine :: Diner -> String
takingsLine { orders } = case mapMaybe paidTotal orders of
  [] -> "Nic jeszcze nie zapłacono"
  totals -> "Zapłacone zamówienia: " <> show (length totals) <> " · " <> money (foldl (+) 0 totals)

paidTotal :: Order -> Maybe Int
paidTotal { stage } = match
  { paid: \{ total } -> Just total
  , taking: \_ -> Nothing
  , cooking: \_ -> Nothing
  , ready: \_ -> Nothing
  } stage

orderDish :: { event :: String, model :: Diner } -> Diner
orderDish { event: name, model: diner } =
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

portionAdded :: Dish -> Dish
portionAdded = withPortions (_ + 1)

portionRemoved :: Dish -> Dish
portionRemoved = withPortions (_ - 1)

sendToKitchen :: Diner -> Diner
sendToKitchen diner = case takenAt diner of
  Just order -> staged order.number (.cooking {}) diner
  Nothing -> diner

plateLine :: { event :: { event :: String, model :: Order }, model :: Diner } -> Diner
plateLine { event: { event: dish, model: { number } }, model: diner } = diner
  { orders = map (\o -> if o.number == number then o { lines = map plated o.lines } else o) diner.orders }
  where
  plated l = if l.dish == dish then l { state = .plated {} } else l

markReady :: { event :: Order, model :: Diner } -> Diner
markReady { event: { number }, model: diner } = staged number (.ready {}) diner

settle :: { event :: Bill, model :: Diner } -> Diner
settle { event: { number, total }, model: diner } = staged number (.paid { total }) diner

takenAt :: Diner -> Maybe Order
takenAt { "Stolik": table, orders } = find (\o -> o.table == table && o.stage == .taking {}) orders

portionsOf :: Dish -> Int
portionsOf dish = match { count: \n -> max 0 (floor n), invalid: \_ -> 0 } dish."Porcje"

withPortions :: (Int -> Int) -> Dish -> Dish
withPortions f dish = dish { "Porcje" = .count (toNumber (max 0 (f (portionsOf dish)))) }

staged :: Int -> Stage -> Diner -> Diner
staged number stage diner = diner { orders = map (\o -> if o.number == number then o { stage = stage } else o) diner.orders }

money :: Int -> String
money cents = show (cents / 100) <> "," <> pad (mod cents 100) <> " zł"
  where
  pad r = if r < 10 then "0" <> show r else show r
