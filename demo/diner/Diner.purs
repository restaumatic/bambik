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

-- | The restaurant during a service: the station being worked at, the table
-- | being served, the dishes on offer with their stock, and every order of
-- | the day.
type Diner =
  { "Stanowisko" :: Station
  , "Stolik" :: Table
  , dishes :: Array Dish
  , orders :: Array Order
  }

-- | Where in the restaurant one works: the stockroom, the hall, the kitchen,
-- | the pass, or the day's books (takings, then sales per dish).
type Station =
  [ "Magazyn" :: {}
  , "Sala" :: {}
  , "Kuchnia" :: {}
  , "Wydawka" :: {}
  , "Raport" :: {}
  , "Statystyki" :: {}
  ]

-- | The hall's tables.
type Table = [ "1" :: {}, "2" :: {}, "3" :: {}, "4" :: {}, "5" :: {}, "6" :: {} ]

-- | A dish on the menu: its price in grosze and the portions left in stock.
type Dish = { name :: String, "Porcje" :: Portions, price :: Int }

-- | Stock as counted, or as written when it cannot be read as a whole number.
type Portions = [ count :: Int, invalid :: String ]

-- | A table's order: numbered as orders are opened, its lines, and how far
-- | along it is.
type Order =
  { lines :: Array Line
  , number :: Int
  , stage :: Stage
  , table :: Table
  }

-- | A dish ordered in some quantity, and whether the kitchen has plated it.
type Line = { dish :: String, quantity :: Int, state :: [ ordered :: {}, plated :: {} ] }

-- | An order's life: taken at the table, cooking from the time it was placed,
-- | ready at the pass until it is paid, then paid for its total.
type Stage =
  [ cooking :: { placed :: Time }
  , paid :: { placed :: Time, total :: Int }
  , ready :: { placed :: Time }
  , taking :: {}
  ]

-- | A local wall-clock time.
type Time = { hours :: Int, minutes :: Int }

-- | A line of the order being taken, with its dish's price and the portions
-- | still in stock.
type TableLine = { dish :: String, left :: Int, price :: Int, quantity :: Int }

-- | What a ready order owes.
type Bill = { number :: Int, placed :: Time, table :: Table, total :: Int }

-- | What the paid orders sold of one dish, and for how much.
type DishSales = { amount :: Int, dish :: String, quantity :: Int }

-- | The restaurant mid-service: three orders paid, one cooking, one waiting
-- | at the pass, and table 1 free to order.
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

-- | No portion left to order; stock that cannot be read counts as none.
isSoldOut :: Dish -> Boolean
isSoldOut dish = portionsOf dish == 0

-- | Stock counted at exactly zero.
isZeroed :: Dish -> Boolean
isZeroed dish = dish."Porcje" == .count 0

-- | A dish's price and whether it can be ordered.
offerLine :: Dish -> String
offerLine dish@{ price } = money price <> " · " <> availability
  where
  availability = if portionsOf dish > 0 then "dostępne: " <> show (portionsOf dish) else "niedostępne"

-- | The question asked before a dish's stock is zeroed.
zeroingLine :: Dish -> String
zeroingLine { name } = "Wyzerować porcje dania " <> name <> "?"

-- | One portion more in stock.
portionAdded :: Dish -> Dish
portionAdded = withPortions (_ + 1)

-- | One portion fewer in stock, never below zero.
portionRemoved :: Dish -> Dish
portionRemoved = withPortions (_ - 1)

-- | Stock set to zero, whatever it read before.
portionsZeroed :: Dish -> Dish
portionsZeroed dish = dish { "Porcje" = .count 0 }

-- | Tables whose order is cooking or waiting to be paid: they take no new
-- | order until it is paid.
busyTables :: Diner -> Array Table
busyTables { orders } = map _.table (filter (\o -> isCooking o || isReady o) orders)

-- | Whether the table being served can take an order.
tableAvailability :: Diner -> [ busy :: {}, free :: {} ]
tableAvailability diner = if isBusy diner then .busy {} else .free {}

-- | The table being served: busy with an order in progress, idle with nothing
-- | ordered yet, or taking an order.
tableState :: Diner -> [ busy :: {}, idle :: {}, taking :: {} ]
tableState diner
  | isBusy diner = .busy {}
  | null (tableLines diner) = .idle {}
  | true = .taking {}

-- | The order being taken at the table being served, priced.
tableLines :: Diner -> Array TableLine
tableLines diner = maybe [] (\order -> mapMaybe priced order.lines) (takenAt diner)
  where
  priced { dish, quantity } = map (\d -> { dish, quantity, price: d.price, left: portionsOf d }) (find (\d -> d.name == dish) diner.dishes)

-- | A line of one portion: one fewer would remove it.
isSinglePortion :: TableLine -> Boolean
isSinglePortion { quantity } = quantity == 1

-- | No portion left in stock to add to the line.
isLineSoldOut :: TableLine -> Boolean
isLineSoldOut { left } = left == 0

-- | A line's amount: price times quantity.
lineTotalLine :: TableLine -> String
lineTotalLine { price, quantity } = money (price * quantity)

-- | The question asked before a line is removed.
removalLine :: TableLine -> String
removalLine { dish, quantity } = "Usunąć " <> show quantity <> " × " <> dish <> " z zamówienia?"

-- | One portion of a dish onto the table's order, taken from stock.
orderDish :: { event :: Dish, model :: Diner } -> Diner
orderDish { event: { name }, model } = withOneMore name model

-- | One more portion of a line's dish, taken from stock.
addUnit :: { event :: TableLine, model :: Diner } -> Diner
addUnit { event: { dish }, model } = withOneMore dish model

-- | One portion fewer, back in stock; the last portion of a line removes the
-- | line, and the last line the order.
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

-- | A line off the order, all its portions back in stock; the last line
-- | removes the order.
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

-- | Places the order being taken, at the local time it is placed.
placeOrder :: Diner -> Aff [ "Zamówienie złożone" :: { number :: Int, placed :: Time } ]
placeOrder diner = liftEffect do
  Milliseconds sinceEpoch <- unInstant <$> now
  Minutes offset <- getTimezoneOffset
  let local = mod (floor (sinceEpoch / 60000.0) - round offset) 1440
  pure (."Zamówienie złożone" { number: maybe 0 _.number (takenAt diner), placed: { hours: local / 60, minutes: mod local 60 } })

-- | A placed order goes to the kitchen, to the end of its queue, so the
-- | kitchen cooks orders in the order they were sent.
sendToKitchen :: { event :: { number :: Int, placed :: Time }, model :: Diner } -> Diner
sendToKitchen { event: { number, placed }, model: diner } = case find (\o -> o.number == number && isTaking o) diner.orders of
  Just order -> diner { orders = snoc (filter (\o -> o.number /= number) diner.orders) order { stage = .cooking { placed } } }
  Nothing -> diner

-- | The kitchen's queue: cooking orders, oldest sent first.
cooking :: Diner -> Array Order
cooking { orders } = filter isCooking orders

-- | Whether the kitchen has anything to cook.
kitchenState :: Diner -> [ busy :: {}, cleared :: {} ]
kitchenState diner = if null (cooking diner) then .cleared {} else .busy {}

-- | An order is plated once every line is.
ticketState :: Order -> [ plated :: {}, plating :: {} ]
ticketState { lines } = if all isPlated lines then .plated {} else .plating {}

-- | A line the kitchen has plated.
isPlated :: Line -> Boolean
isPlated { state } = state == .plated {}

-- | An order's number, table and the time it was placed.
ticketLine :: Order -> String
ticketLine order@{ number, table } = "#" <> show number <> " · Stolik " <> caseText table <> " · " <> placedLine order

-- | A line as quantity × dish.
itemLine :: Line -> String
itemLine { dish, quantity } = show quantity <> " × " <> dish

-- | A dish of an order plated.
plateDish :: { event :: { event :: String, model :: Order }, model :: Diner } -> Diner
plateDish { event: { event: dish, model: { number } }, model: diner } = diner
  { orders = map (\o -> if o.number == number then o { lines = map plated o.lines } else o) diner.orders }
  where
  plated l = if l.dish == dish then l { state = .plated {} } else l

-- | A cooking order goes to the pass; an order at any other stage stays as
-- | it is.
markReady :: { event :: Order, model :: Diner } -> Diner
markReady { event: { number }, model } = advanced number ready model
  where
  ready = on (Proxy @"cooking") (\{ placed } -> Just (.ready { placed })) (default Nothing)

-- | What each order at the pass owes, at the dishes' prices.
bills :: Diner -> Array Bill
bills { dishes, orders } = mapMaybe bill orders
  where
  bill { number, table, lines, stage } = on (Proxy @"ready")
    (\{ placed } -> Just { number, table, placed, total: foldl (\sum l -> sum + l.quantity * priceOf l.dish) 0 lines })
    (default Nothing)
    stage
  priceOf name = maybe 0 _.price (find (\d -> d.name == name) dishes)

-- | Whether any order at the pass is waiting to be paid.
billsState :: Diner -> [ pending :: {}, settled :: {} ]
billsState diner = if null (bills diner) then .settled {} else .pending {}

-- | The time a billed order was placed.
billPlacedLine :: Bill -> String
billPlacedLine { placed } = clockText placed

-- | A bill's total.
billTotalLine :: Bill -> String
billTotalLine { total } = money total

-- | An order at the pass paid for its bill's total; an order at any other
-- | stage stays as it is.
settle :: { event :: Bill, model :: Diner } -> Diner
settle { event: { number, total }, model } = advanced number paid model
  where
  paid = on (Proxy @"ready") (\{ placed } -> Just (.paid { placed, total })) (default Nothing)

-- | The paid orders, newest first.
paidOrders :: Diner -> Array Order
paidOrders { orders } = sortWith (\o -> negate o.number) (filter (\o -> isJust (paidTotal o)) orders)

-- | Whether anything has been sold yet.
paidState :: Diner -> [ none :: {}, some :: {} ]
paidState diner = if null (paidOrders diner) then .none {} else .some {}

-- | The time an order was placed; none while it is still being taken.
placedLine :: Order -> String
placedLine { stage } = match
  { taking: \_ -> ""
  , cooking: \{ placed } -> clockText placed
  , ready: \{ placed } -> clockText placed
  , paid: \{ placed } -> clockText placed
  } stage

-- | An order's lines, in the order they were taken.
orderItemsLine :: Order -> String
orderItemsLine { lines } = joinWith ", " (map itemLine lines)

-- | What a paid order was paid.
paidTotalLine :: Order -> String
paidTotalLine order = money (fromMaybe 0 (paidTotal order))

-- | The day's takings: every paid order's total.
takingsLine :: Diner -> String
takingsLine diner = money (foldl (+) 0 (mapMaybe paidTotal (paidOrders diner)))

-- | Sales per dish over the paid orders, at the dishes' prices, by name; a
-- | dish nobody bought is left out.
dishSales :: Diner -> Array DishSales
dishSales diner = sortWith _.dish (filter (\s -> s.quantity > 0) (map sales diner.dishes))
  where
  sold = concatMap _.lines (paidOrders diner)
  sales { name, price } =
    let quantity = foldl (+) 0 (map _.quantity (filter (\l -> l.dish == name) sold))
    in { dish: name, quantity, amount: quantity * price }

-- | What one dish sold for.
salesAmountLine :: DishSales -> String
salesAmountLine { amount } = money amount

-- | What every dish sold for together.
soldAmountLine :: Diner -> String
soldAmountLine diner = money (foldl (+) 0 (map _.amount (dishSales diner)))

-- | How many portions were sold.
soldQuantityLine :: Diner -> String
soldQuantityLine diner = show (foldl (+) 0 (map _.quantity (dishSales diner)))

-- | An order's number.
numberLine :: forall r. { number :: Int | r } -> String
numberLine { number } = show number

-- | A table's number.
tableLine :: forall r. { table :: Table | r } -> String
tableLine { table } = caseText table

-- | A number of portions.
quantityLine :: forall r. { quantity :: Int | r } -> String
quantityLine { quantity } = show quantity

-- | Portions to be had: stock that cannot be read, or is negative, counts
-- | as none.
portionsOf :: Dish -> Int
portionsOf dish = match { count: max 0, invalid: \_ -> 0 } dish."Porcje"

-- | Stock changed by `f`, never below zero; stock that cannot be read, or is
-- | negative, is left as it is, since what is really there is unknown.
withPortions :: (Int -> Int) -> Dish -> Dish
withPortions f dish = match
  { count: \n -> if n < 0 then dish else dish { "Porcje" = .count (max 0 (f n)) }
  , invalid: \_ -> dish
  } dish."Porcje"

-- | Whether the table being served has an order in progress.
isBusy :: Diner -> Boolean
isBusy diner = elem diner."Stolik" (busyTables diner)

-- | The order being taken at the table being served, if one is open.
takenAt :: Diner -> Maybe Order
takenAt { "Stolik": table, orders } = find (\o -> o.table == table && isTaking o) orders

-- | One portion of the named dish onto the table's order, taken from stock,
-- | opening a new order if none is being taken; a busy table or a dish out of
-- | stock changes nothing.
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

-- | An order still being taken.
isTaking :: Order -> Boolean
isTaking { stage } = on (Proxy @"taking") (const true) (default false) stage

-- | An order in the kitchen.
isCooking :: Order -> Boolean
isCooking { stage } = on (Proxy @"cooking") (const true) (default false) stage

-- | An order at the pass, waiting to be paid.
isReady :: Order -> Boolean
isReady { stage } = on (Proxy @"ready") (const true) (default false) stage

-- | The numbered order moved on by `step`, from the stage it has in the
-- | model; a stage `step` does not allow stays as it is.
advanced :: Int -> (Stage -> Maybe Stage) -> Diner -> Diner
advanced number step diner = diner { orders = map advance diner.orders }
  where
  advance o = if o.number == number then maybe o (\stage -> o { stage = stage }) (step o.stage) else o

-- | What an order was paid, once it is paid.
paidTotal :: Order -> Maybe Int
paidTotal { stage } = on (Proxy @"paid") (\{ total } -> Just total) (default Nothing) stage

-- | A time as hours:minutes.
clockText :: Time -> String
clockText { hours, minutes } = show hours <> ":" <> pad minutes

-- | An amount in grosze as złoty, `12,50 zł`.
money :: Int -> String
money cents = show (cents / 100) <> "," <> pad (mod cents 100) <> "\x00a0zł"

-- | Two digits.
pad :: Int -> String
pad r = if r < 10 then "0" <> show r else show r
