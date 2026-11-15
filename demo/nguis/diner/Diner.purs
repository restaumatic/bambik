module Diner
  ( Bill
  , Diner
  , Dish
  , Event
  , Line
  , Order
  , Stage
  , Table
  , TableLine
  , billLine
  , bills
  , cooking
  , isPlated
  , itemLine
  , lineTotalLine
  , menuCard
  , offerLine
  , opened
  , quantityLine
  , tableLine
  , tableLines
  , tableState
  , takingsLine
  , ticketLine
  , ticketState
  ) where

import Prelude ((#), (&&), (*), (+), (-), (/), (/=), (<), (<>), (==), (>), class Applicative, map, max, mod, show)

import Data.Array (all, any, filter, find, foldl, length, mapMaybe, null, snoc)
import Data.Maybe (Maybe(..), maybe)
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Variant (match)
import Data.Variant.Case (caseText)
import PUI (PUI, capture, fold, looped, with)
import QualifiedDo.Semigroupoid as Semigroupoid

type Diner =
  { "Table" :: Table
  , dishes :: Array Dish
  , orders :: Array Order
  }

type Table = [ "Table 1" :: {}, "Table 2" :: {}, "Table 3" :: {} ]

type Dish = { name :: String, portions :: Int, price :: Int }

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

type Event =
  [ "Dish ordered" :: { event :: String, model :: Diner }
  , "Line plated" :: { event :: { event :: String, model :: Order }, model :: Diner }
  , "Order ready" :: { event :: Order, model :: Diner }
  , "Paid" :: { event :: Bill, model :: Diner }
  , "Send to kitchen" :: Diner
  , "Table chosen" :: Diner
  , "Unit returned" :: { event :: TableLine, model :: Diner }
  ]

opened :: forall m. Applicative m => PUI m Diner Event -> PUI m {} Diner
opened view =
  Semigroupoid.do
    view
    VariantToRecord.do
      capture @"Table chosen"
      fold @"Dish ordered" orderDish
      fold @"Unit returned" returnUnit
      fold @"Send to kitchen" sendToKitchen
      fold @"Line plated" plateLine
      fold @"Order ready" markReady
      fold @"Paid" settle
  # looped # with dinnerService

dinnerService :: Diner
dinnerService =
  { "Table": ."Table 1" {}
  , dishes:
    [ { name: "Carbonara", price: 1050, portions: 6 }
    , { name: "Margherita", price: 950, portions: 8 }
    , { name: "Risotto", price: 1200, portions: 3 }
    , { name: "Tiramisu", price: 650, portions: 2 }
    ]
  , orders:
    [ { number: 1
      , table: ."Table 3" {}
      , stage: .cooking {}
      , lines:
        [ { dish: "Margherita", quantity: 2, state: .plated {} }
        , { dish: "Tiramisu", quantity: 1, state: .ordered {} }
        ]
      }
    ]
  }

menuCard :: Diner -> Array Dish
menuCard { dishes } = filter (\d -> d.portions > 0) dishes

offerLine :: Dish -> String
offerLine { name, price, portions } = name <> " · " <> money price <> " · " <> show portions <> " left"

tableLines :: Diner -> Array TableLine
tableLines diner = maybe [] (\order -> mapMaybe priced order.lines) (takenAt diner)
  where
  priced { dish, quantity } = map (\d -> { dish, quantity, price: d.price }) (find (\d -> d.name == dish) diner.dishes)

quantityLine :: TableLine -> String
quantityLine { quantity } = show quantity

lineTotalLine :: TableLine -> String
lineTotalLine { price, quantity } = money (price * quantity)

tableLine :: Diner -> String
tableLine diner = caseText diner."Table" <> " · " <> case tableLines diner of
  [] -> "nothing ordered yet"
  lines -> money (foldl (\sum l -> sum + l.price * l.quantity) 0 lines)

tableState :: Diner -> [ idle :: {}, taking :: {} ]
tableState diner = if null (tableLines diner) then .idle {} else .taking {}

cooking :: Diner -> Array Order
cooking { orders } = filter (\o -> o.stage == .cooking {}) orders

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

takingsLine :: Diner -> String
takingsLine { orders } = case mapMaybe paidTotal orders of
  [] -> "Nothing paid yet"
  [ total ] -> "1 order paid · " <> money total
  totals -> show (length totals) <> " orders paid · " <> money (foldl (+) 0 totals)
  where
  paidTotal { stage } = match
    { paid: \{ total } -> Just total
    , taking: \_ -> Nothing
    , cooking: \_ -> Nothing
    , ready: \_ -> Nothing
    } stage

orderDish :: { event :: String, model :: Diner } -> Diner
orderDish { event: name, model: diner } =
  if any (\d -> d.name == name && d.portions > 0) diner.dishes then
    diner
      { dishes = map (\d -> if d.name == name then d { portions = d.portions - 1 } else d) diner.dishes
      , orders = case takenAt diner of
        Just order -> map (\o -> if o.number == order.number then o { lines = oneMore o.lines } else o) diner.orders
        Nothing -> snoc diner.orders
          { number: foldl max 0 (map _.number diner.orders) + 1
          , table: diner."Table"
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
    { dishes = map (\d -> if d.name == name then d { portions = d.portions + 1 } else d) diner.dishes
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
takenAt { "Table": table, orders } = find (\o -> o.table == table && o.stage == .taking {}) orders

staged :: Int -> Stage -> Diner -> Diner
staged number stage diner = diner { orders = map (\o -> if o.number == number then o { stage = stage } else o) diner.orders }

money :: Int -> String
money cents = "€" <> show (cents / 100) <> "." <> pad (mod cents 100)
  where
  pad r = if r < 10 then "0" <> show r else show r
