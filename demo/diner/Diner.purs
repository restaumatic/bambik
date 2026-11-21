module Diner
  ( Bill
  , Course
  , Diner
  , Dish
  , DishSales
  , Group
  , Line
  , Offer
  , Order
  , Portions
  , Price
  , Receipt
  , Seating
  , Stage
  , Standing
  , Station
  , TableLine
  , Ticket
  , Time
  , Turned
  , addedLine
  , addUnit
  , billLine
  , billsPage
  , billsState
  , billTotalLine
  , browseState
  , clockPeriod
  , isFirstPage
  , isLastPage
  , isAdded
  , isLineSoldOut
  , isOffMenu
  , isPlated
  , isSinglePortion
  , isSoldOut
  , isZeroed
  , itemLine
  , kitchenState
  , lineTotalLine
  , markReady
  , menuPage
  , nextPage
  , numberLine
  , offerLine
  , offerState
  , openService
  , orderDish
  , orderItemsLine
  , orderTotalLine
  , paceFace
  , pageLine
  , paidPage
  , paidState
  , paidTotalLine
  , paymentLine
  , placeOrder
  , plateDish
  , portionAdded
  , portionRemoved
  , portionsZeroed
  , previousPage
  , quantityLine
  , readyLine
  , receiptLine
  , removalLine
  , removeDish
  , returnUnit
  , salesAmountLine
  , salesPage
  , searchState
  , seatings
  , secondPassed
  , sendToKitchen
  , servedLine
  , serveTable
  , settle
  , soldAmountLine
  , soldQuantityLine
  , standings
  , stockPage
  , swatchFace
  , tableAvailability
  , tableFace
  , tableLines
  , tableName
  , tableState
  , takingsLine
  , ticketLine
  , ticketsPage
  , ticketState
  , waitLine
  , zeroingLine
  ) where

import Prelude (Ordering, bind, clamp, compare, const, map, max, min, mod, not, pure, show, top, (&&), (*), (+), (-), (/), (/=), (<), (<$>), (<*>), (<=), (<>), (==), (>), (>=), (||))

import Data.Array (all, any, filter, find, foldl, length, mapMaybe, null, range, slice, snoc, sortBy, sortWith)
import Data.Foldable (sum)
import Data.Map (Map)
import Data.Int (fromString)
import Data.Map as Map
import Data.Set as Set
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String (Pattern(..), Replacement(..), contains, joinWith, replaceAll, split, toLower, trim)
import Data.String as Data.String
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))
import Data.Variant (default, match, on)
import Data.Variant.Case (caseText)
import Diner.Service as Service
import Effect.Aff (Aff)
import Type.Proxy (Proxy(..))

-- | The restaurant during a service: the station being worked at, the
-- | group of dishes and the dish being looked for in the stockroom and, each
-- | on their own, on the menu and in the statistics, the table being served, the rows to a page and
-- | the pages turned to, and the day itself — the dishes on offer with their
-- | stock, the hall's tables, every order, and the time it is.
type Diner =
  { "Grupa magazynu" :: Group
  , "Grupa menu" :: Group
  , "Grupa statystyk" :: Group
  , "Stanowisko" :: Station
  , "Szukaj w magazynie" :: String
  , "Szukaj w menu" :: String
  , "Szukaj w statystykach" :: String
  , dishes :: Array Dish
  , now :: Time
  , orders :: Array Order
  , perPage :: { list :: Int, tiles :: Int }
  , table :: Int
  , tables :: Int
  , turned :: Array Turned
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

-- | The dishes browsed while nothing is searched for: every course, or one
-- | of them.
type Group =
  [ "Wszystkie" :: {}
  , "Przystawki" :: {}
  , "Zupy" :: {}
  , "Makarony" :: {}
  , "Dania główne" :: {}
  , "Desery" :: {}
  ]

-- | A page turned to at a station: it is the one on screen there for as
-- | long as the group and the search it was turned at stand; a station
-- | shows its first page otherwise.
type Turned = { group :: Group, page :: Int, query :: String, station :: Station }

-- | A dish on the menu: its price, the course it belongs to and the
-- | portions left in stock.
type Dish = Service.Dish

-- | A price in złoty as written on the menu, a comma before the grosze.
type Price = Service.Price

-- | The courses of the menu, in the order they are served.
type Course = Service.Course

-- | Stock as counted, or as written when it cannot be read as a whole number.
type Portions = Service.Portions

-- | A table's order: numbered as orders are opened, its lines, and how far
-- | along it is.
type Order = Service.Order

-- | A dish ordered in some quantity, at the price in grosze it had when it
-- | was put on the order, and whether the kitchen has plated it.
type Line = Service.Line

-- | An order's life: taken at the table, cooking from the time it was placed,
-- | ready at the pass until it is paid, then paid for its total.
type Stage = Service.Stage

-- | A moment of local time, in seconds.
type Time = Service.Time

-- | A table of the hall: what it is up to, and whether it is the one being
-- | served.
type Seating =
  { number :: Int
  , served :: [ elsewhere :: {}, here :: {} ]
  , state :: Standing
  }

-- | What a table is up to: free, or as far as its open order has come.
type Standing =
  [ "wolny" :: {}
  , "zamawia" :: {}
  , "w kuchni" :: {}
  , "do zapłaty" :: {}
  ]

-- | A dish as the menu offers it to the table being served: the dish, and
-- | the portions of it already on the table's order.
type Offer = { added :: Int, "Cena" :: Price, group :: Course, name :: String, "Porcje" :: Portions }

-- | A line of the order being taken, at the price it was put on the order
-- | at, with the portions still in stock.
type TableLine = { dish :: String, left :: Int, price :: Int, quantity :: Int }

-- | An order in the kitchen, and the minutes it has waited there.
type Ticket = { lines :: Array Line, number :: Int, placed :: Time, table :: Int, waited :: Int }

-- | What a ready order owes, and the minutes since it was placed.
type Bill = { number :: Int, placed :: Time, table :: Int, total :: Int, waited :: Int }

-- | A paid order, and whether it was placed today.
type Receipt =
  { day :: [ today :: {}, yesterday :: {} ]
  , lines :: Array Line
  , number :: Int
  , placed :: Time
  , table :: Int
  , total :: Int
  }

-- | What the paid orders sold of one dish, and for how much.
type DishSales = { amount :: Int, dish :: String, quantity :: Int }

-- | The service opened on the day as the back office has it: the first free
-- | table being served, every dish looked through, the first page
-- | everywhere.
openService :: {} -> Aff [ "Serwis otwarty" :: Diner ]
openService _ = do
  day <- Service.today
  pure
    ( ."Serwis otwarty"
        { "Grupa magazynu": ."Wszystkie" {}
        , "Grupa menu": ."Wszystkie" {}
        , "Grupa statystyk": ."Wszystkie" {}
        , "Stanowisko": ."Sala" {}
        , "Szukaj w magazynie": ""
        , "Szukaj w menu": ""
        , "Szukaj w statystykach": ""
        , dishes: day.dishes
        , now: day.now
        , orders: day.orders
        , perPage: { list: 10, tiles: 24 }
        , table: fromMaybe 1 (find (\table -> not (any (\o -> o.table == table && not (isPaid o)) day.orders)) (range 1 day.tables))
        , tables: day.tables
        , turned: []
        }
    )

-- | How often the clock is read.
clockPeriod :: { ms :: Number }
clockPeriod = { ms: 1000.0 }

-- | The time one clock period later.
secondPassed :: Diner -> Diner
secondPassed diner = diner { now = diner.now + 1 }

-- | Where the rows on screen stand among all of them, `21–30 z 312`.
pageLine :: Diner -> String
pageLine diner =
  if count == 0 then "0 z 0"
  else show (first + 1) <> "–" <> show (min count (first + rowsPerPage diner)) <> " z " <> show count
  where
  count = listed diner
  first = pageOf count diner * rowsPerPage diner

-- | Nothing before the rows on screen.
isFirstPage :: Diner -> Boolean
isFirstPage diner = pageOf (listed diner) diner == 0

-- | Nothing after the rows on screen.
isLastPage :: Diner -> Boolean
isLastPage diner = pageOf count diner == lastPage count diner
  where
  count = listed diner

-- | The page before the one on screen.
previousPage :: Diner -> Diner
previousPage diner = turnedTo (pageOf (listed diner) diner - 1) diner

-- | The page after the one on screen.
nextPage :: Diner -> Diner
nextPage diner = turnedTo (pageOf (listed diner) diner + 1) diner

-- | How dishes are being found: browsed by group, or searched for by name
-- | across every group.
browseState :: Diner -> [ browsing :: {}, searching :: {} ]
browseState diner = if wanted diner == "" then .browsing {} else .searching {}

-- | The group browsed and the name searched for at the station being worked
-- | at: the stockroom's, the hall's, the statistics', and none at the
-- | stations that list no dishes.
finding :: Diner -> { group :: Group, query :: String }
finding diner = match
  { "Magazyn": \_ -> { group: diner."Grupa magazynu", query: diner."Szukaj w magazynie" }
  , "Sala": \_ -> { group: diner."Grupa menu", query: diner."Szukaj w menu" }
  , "Kuchnia": \_ -> nothing
  , "Wydawka": \_ -> nothing
  , "Raport": \_ -> nothing
  , "Statystyki": \_ -> { group: diner."Grupa statystyk", query: diner."Szukaj w statystykach" }
  } diner."Stanowisko"
  where
  nothing = { group: ."Wszystkie" {}, query: "" }

-- | Whether the station being worked at has any dish to list.
searchState :: Diner -> [ found :: {}, none :: {} ]
searchState diner = if listed diner == 0 then .none {} else .found {}

-- | The dishes on the stockroom's page, by name.
stockPage :: Diner -> Array Dish
stockPage diner = paged diner (sortWith _.name (sought diner))

-- | No portion left to order; stock that cannot be read counts as none.
isSoldOut :: Dish -> Boolean
isSoldOut dish = portionsOf dish == 0

-- | Stock counted at exactly zero.
isZeroed :: Dish -> Boolean
isZeroed dish = dish."Porcje" == .count 0

-- | Nothing of the dish can be ordered: none in stock, or no price that
-- | can be read.
isOffMenu :: Offer -> Boolean
isOffMenu { "Cena": written, "Porcje": portions } = not (isOrderable { "Cena": written, "Porcje": portions })

-- | A dish's price and the portions of it left to order.
offerLine :: Offer -> String
offerLine { "Cena": written, "Porcje": portions } = maybe "bez ceny" money (priceRead written) <> " · " <> availability
  where
  availability =
    if isOrderable { "Cena": written, "Porcje": portions } then "dostępne: " <> show (portionsOf { "Porcje": portions })
    else "niedostępne"

-- | Whether any of the dish is on the table's order yet.
offerState :: Offer -> [ added :: {}, untouched :: {} ]
offerState offer = if isAdded offer then .added {} else .untouched {}

-- | Some of the dish is on the table's order.
isAdded :: Offer -> Boolean
isAdded { added } = added > 0

-- | The portions of a dish on the table's order, as a multiple beside its
-- | name.
addedLine :: Offer -> String
addedLine { added } = "×" <> show added

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

-- | Every table of the hall, in order: free, or as far as its open order
-- | has come.
seatings :: Diner -> Array Seating
seatings diner = map seating (range 1 diner.tables)
  where
  open = Map.fromFoldable (mapMaybe (\o -> map (Tuple o.table) (openState o)) diner.orders)
  seating number =
    { number
    , served: if number == diner.table then .here {} else .elsewhere {}
    , state: fromMaybe (."wolny" {}) (Map.lookup number open)
    }
  openState { stage } = match
    { taking: \_ -> Just (."zamawia" {})
    , cooking: \_ -> Just (."w kuchni" {})
    , ready: \_ -> Just (."do zapłaty" {})
    , paid: \_ -> Nothing
    } stage

-- | Everything a table can be up to, each under its name.
standings :: Array { name :: String, state :: Standing }
standings = map (\state -> { name: caseText state, state }) [ ."wolny" {}, ."zamawia" {}, ."w kuchni" {}, ."do zapłaty" {} ]

-- | A table's number.
numberLine :: Seating -> String
numberLine { number } = show number

-- | A table by its name and what it is up to.
tableName :: Seating -> String
tableName { number, state } = "Stolik " <> show number <> " · " <> caseText state

-- | A table's colour, by what it is up to, framed when it is the one being
-- | served.
tableFace :: Seating -> String
tableFace { served, state } =
  "display: flex; flex-direction: column; align-items: center; justify-content: center; height: 44px; border-radius: 8px; "
    <> frame
    <> colourOf state
  where
  frame = match
    { here: \_ -> "outline: 3px solid var(--md-sys-color-primary, #6750a4); outline-offset: -3px; font-weight: 700; "
    , elsewhere: \_ -> ""
    } served

-- | The colour of a thing a table can be up to, as a patch beside its name.
swatchFace :: { name :: String, state :: Standing } -> String
swatchFace { state } =
  "display: inline-flex; align-items: center; justify-content: center; width: 24px; height: 24px; border-radius: 6px; "
    <> colourOf state

-- | Grey while free, blue while taking an order, orange while the kitchen
-- | cooks, green once the order waits to be paid.
colourOf :: Standing -> String
colourOf = match
  { "wolny": \_ -> "background: #e7e0ec;"
  , "zamawia": \_ -> "background: #cfe4ff;"
  , "w kuchni": \_ -> "background: #ffdcc2;"
  , "do zapłaty": \_ -> "background: #cdeccd;"
  }

-- | The picked table is the one being served.
serveTable :: { event :: Int, model :: Diner } -> Diner
serveTable { event: table, model } = model { table = table }

-- | The table being served and what it is up to: free, taking an order, or
-- | which order it waits for, where that order is and since when.
servedLine :: Diner -> String
servedLine diner = "Stolik " <> show diner.table <> " · " <> maybe "wolny" standing (find (\o -> o.table == diner.table && not (isPaid o)) diner.orders)
  where
  standing { number, stage } = match
    { taking: \_ -> "zamawia"
    , cooking: \{ placed } -> placedLine number "w kuchni" placed
    , ready: \{ placed } -> placedLine number "do zapłaty" placed
    , paid: \_ -> "wolny"
    } stage
  placedLine number place placed =
    "zamówienie #" <> show number <> " " <> place <> ", złożone " <> show (minutesSince placed diner) <> " min temu"

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
  priced { dish, price, quantity } = map (\d -> { dish, quantity, price, left: if isOrderable d then portionsOf d else 0 }) (find (\d -> d.name == dish) diner.dishes)

-- | A line of one portion: one fewer would remove it.
isSinglePortion :: TableLine -> Boolean
isSinglePortion { quantity } = quantity == 1

-- | No portion left in stock to add to the line.
isLineSoldOut :: TableLine -> Boolean
isLineSoldOut { left } = left == 0

-- | A line's amount: the price it was ordered at times its quantity.
lineTotalLine :: TableLine -> String
lineTotalLine { price, quantity } = money (price * quantity)

-- | What the order being taken comes to, at the prices its lines were
-- | ordered at.
orderTotalLine :: Diner -> String
orderTotalLine diner = money (sum (map (\l -> l.price * l.quantity) (tableLines diner)))

-- | The question asked before a line is removed.
removalLine :: TableLine -> String
removalLine { dish, quantity } = "Usunąć " <> show quantity <> " × " <> dish <> " z zamówienia?"

-- | The dishes on the menu's page, by name, each with the portions of it on
-- | the order being taken.
menuPage :: Diner -> Array Offer
menuPage diner = map offer (paged diner (sortWith _.name (sought diner)))
  where
  taken = Map.fromFoldable (map (\l -> Tuple l.dish l.quantity) (maybe [] _.lines (takenAt diner)))
  offer { "Cena": written, group, name, "Porcje": portions } =
    { added: fromMaybe 0 (Map.lookup name taken), "Cena": written, group, name, "Porcje": portions }

-- | One portion of the named dish onto the table's order, taken from stock.
orderDish :: { event :: String, model :: Diner } -> Diner
orderDish { event: name, model } = withOneMore name model

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

-- | Places the order being taken with the kitchen.
placeOrder :: Diner -> Aff [ "Zamówienie złożone" :: { number :: Int } ]
placeOrder diner = do
  number <- Service.taken (maybe 0 _.number (takenAt diner))
  pure (."Zamówienie złożone" { number })

-- | A placed order goes to the kitchen, placed at the time it lands there.
sendToKitchen :: { event :: { number :: Int }, model :: Diner } -> Diner
sendToKitchen { event: { number }, model } = advanced number cooked model
  where
  cooked = on (Proxy @"taking") (\_ -> Just (.cooking { placed: model.now })) (default Nothing)

-- | Whether the kitchen has anything to cook.
kitchenState :: Diner -> [ busy :: {}, cleared :: {} ]
kitchenState diner = if any isCooking diner.orders then .busy {} else .cleared {}

-- | The tickets on the kitchen's page, oldest sent first.
ticketsPage :: Diner -> Array Ticket
ticketsPage diner = paged diner (cooking diner)

-- | An order is plated once every line is.
ticketState :: Ticket -> [ plated :: {}, plating :: {} ]
ticketState { lines } = if all isPlated lines then .plated {} else .plating {}

-- | A line the kitchen has plated.
isPlated :: Line -> Boolean
isPlated { state } = state == .plated {}

-- | An order's number, table and the time it was placed.
ticketLine :: Ticket -> String
ticketLine { number, placed, table } = "#" <> show number <> " · Stolik " <> show table <> " · " <> clockText placed

-- | The minutes an order has waited.
waitLine :: forall r. { waited :: Int | r } -> String
waitLine { waited } = show waited <> " min"

-- | How a wait reads: plain while the order is fresh, marked once it is
-- | late, alarming once it is overdue.
paceFace :: forall r. { waited :: Int | r } -> String
paceFace { waited }
  | waited < lateAfter = ""
  | waited < overdueAfter = "color: var(--md-sys-color-tertiary, #7d5260); font-weight: 700;"
  | true = "color: var(--md-sys-color-error, #b3261e); font-weight: 700;"

-- | A line as quantity × dish.
itemLine :: Line -> String
itemLine { dish, quantity } = show quantity <> " × " <> dish

-- | A dish of an order plated.
plateDish :: { event :: { event :: String, model :: Ticket }, model :: Diner } -> Diner
plateDish { event: { event: dish, model: { number } }, model: diner } = diner
  { orders = map (\o -> if o.number == number then o { lines = map plated o.lines } else o) diner.orders }
  where
  plated l = if l.dish == dish then l { state = .plated {} } else l

-- | The question asked before an order is passed on as ready.
readyLine :: Ticket -> String
readyLine { number, table } = "Przekazać zamówienie #" <> show number <> " dla stolika " <> show table <> " na wydawkę?"

-- | A cooking order goes to the pass; an order at any other stage stays as
-- | it is.
markReady :: { event :: Ticket, model :: Diner } -> Diner
markReady { event: { number }, model } = advanced number ready model
  where
  ready = on (Proxy @"cooking") (\{ placed } -> Just (.ready { placed })) (default Nothing)

-- | Whether any order at the pass is waiting to be paid.
billsState :: Diner -> [ pending :: {}, settled :: {} ]
billsState diner = if any isReady diner.orders then .pending {} else .settled {}

-- | The bills on the pass's page, the longest waiting first: what each
-- | order there owes, at the prices its dishes were ordered at.
billsPage :: Diner -> Array Bill
billsPage diner = mapMaybe bill (paged diner (sortBy oldestFirst (filter isReady diner.orders)))
  where
  bill { number, table, lines, stage } = on (Proxy @"ready")
    ( \{ placed } -> Just
        { number
        , table
        , placed
        , total: sum (map (\l -> l.quantity * l.price) lines)
        , waited: minutesSince placed diner
        }
    )
    (default Nothing)
    stage

-- | A billed order's number and table.
billLine :: Bill -> String
billLine { number, table } = "#" <> show number <> " · stolik " <> show table

-- | A bill's total.
billTotalLine :: Bill -> String
billTotalLine { total } = money total

-- | The question asked before a bill is taken as paid.
paymentLine :: Bill -> String
paymentLine { number, table, total } =
  "Przyjąć " <> money total <> " za zamówienie #" <> show number <> " ze stolika " <> show table <> "?"

-- | An order at the pass paid for its bill's total; an order at any other
-- | stage stays as it is.
settle :: { event :: Bill, model :: Diner } -> Diner
settle { event: { number, total }, model } = advanced number paid model
  where
  paid = on (Proxy @"ready") (\{ placed } -> Just (.paid { placed, total })) (default Nothing)

-- | Whether anything has been sold yet.
paidState :: Diner -> [ none :: {}, some :: {} ]
paidState diner = if null (receipts diner) then .none {} else .some {}

-- | The receipts on the report's page, the latest placed first.
paidPage :: Diner -> Array Receipt
paidPage diner = paged diner (sortBy (\a b -> compare b.placed a.placed <> compare b.number a.number) (receipts diner))

-- | A paid order's number, table and the time it was placed.
receiptLine :: Receipt -> String
receiptLine { day, number, placed, table } = "#" <> show number <> " · stolik " <> show table <> " · " <> match
  { today: \_ -> clockText placed
  , yesterday: \_ -> "wczoraj " <> clockText placed
  } day

-- | An order's lines, in the order they were taken.
orderItemsLine :: Receipt -> String
orderItemsLine { lines } = joinWith ", " (map itemLine lines)

-- | What a paid order was paid.
paidTotalLine :: Receipt -> String
paidTotalLine { total } = money total

-- | The day's takings: every paid order's total.
takingsLine :: Diner -> String
takingsLine diner = money (sum (map _.total (receipts diner)))

-- | The dishes on the statistics' page, the best selling first.
salesPage :: Diner -> Array DishSales
salesPage diner = paged diner (soughtSales diner)

-- | What one dish sold for.
salesAmountLine :: DishSales -> String
salesAmountLine { amount } = money amount

-- | What the dishes looked for sold for together.
soldAmountLine :: Diner -> String
soldAmountLine diner = money (sum (map _.amount (soughtSales diner)))

-- | How many portions of the dishes looked for were sold.
soldQuantityLine :: Diner -> String
soldQuantityLine diner = show (sum (map _.quantity (soughtSales diner)))

-- | A number of portions.
quantityLine :: forall r. { quantity :: Int | r } -> String
quantityLine { quantity } = show quantity

-- | Minutes of waiting after which an order is late, and after which it is
-- | overdue.
lateAfter :: Int
lateAfter = 15

overdueAfter :: Int
overdueAfter = 30

-- | The dishes looked for: while a name is searched for, the dishes of every
-- | group whose name contains it, whatever the letter case; otherwise the
-- | dishes of the group being browsed.
sought :: Diner -> Array Dish
sought diner@{ dishes } =
  if wanted diner == "" then filter grouped dishes
  else filter (\d -> contains (Pattern (wanted diner)) (toLower d.name)) dishes
  where
  { group } = finding diner
  grouped d = group == ."Wszystkie" {} || caseText group == caseText d.group

-- | The name searched for, as it is compared: trimmed, in lower case.
wanted :: Diner -> String
wanted diner = toLower (trim (finding diner).query)

-- | How many rows the station being worked at lists.
listed :: Diner -> Int
listed diner = match
  { "Magazyn": \_ -> length (sought diner)
  , "Sala": \_ -> length (sought diner)
  , "Kuchnia": \_ -> length (cooking diner)
  , "Wydawka": \_ -> length (filter isReady diner.orders)
  , "Raport": \_ -> length (receipts diner)
  , "Statystyki": \_ -> length (soughtSales diner)
  } diner."Stanowisko"

-- | Rows to a page at the station being worked at, at least one: the hall's
-- | menu is tiles, every other station a list.
rowsPerPage :: Diner -> Int
rowsPerPage diner = max 1 (if diner."Stanowisko" == ."Sala" {} then diner.perPage.tiles else diner.perPage.list)

-- | The last page of `count` rows, counting from zero.
lastPage :: Int -> Diner -> Int
lastPage count diner = max 0 ((count - 1) / rowsPerPage diner)

-- | The page on screen of `count` rows, counting from zero: the one turned
-- | to at this station while the group and the search were what they are
-- | now, never beyond the last, and the first when none was.
pageOf :: Int -> Diner -> Int
pageOf count diner = maybe 0 (\{ page } -> clamp 0 (lastPage count diner) page)
  (find (\t -> t.station == diner."Stanowisko" && t.query == query && t.group == group) diner.turned)
  where
  { group, query } = finding diner

-- | The rows of the page on screen.
paged :: forall a. Diner -> Array a -> Array a
paged diner rows = slice first (first + rowsPerPage diner) rows
  where
  first = pageOf (length rows) diner * rowsPerPage diner

-- | A page turned to at the station being worked at, under the group and
-- | the search as they stand, in place of the one turned to there before.
turnedTo :: Int -> Diner -> Diner
turnedTo page diner = diner
  { turned = snoc (filter (\t -> t.station /= diner."Stanowisko") diner.turned)
      { group: (finding diner).group, page: max 0 page, query: (finding diner).query, station: diner."Stanowisko" }
  }

-- | The kitchen's queue: cooking orders, the longest waiting first, with the
-- | minutes each has waited.
cooking :: Diner -> Array Ticket
cooking diner = mapMaybe ticket (sortBy oldestFirst (filter isCooking diner.orders))
  where
  ticket { lines, number, stage, table } = on (Proxy @"cooking")
    (\{ placed } -> Just { lines, number, placed, table, waited: minutesSince placed diner })
    (default Nothing)
    stage

-- | The paid orders, as placed.
receipts :: Diner -> Array Receipt
receipts diner = mapMaybe receipt diner.orders
  where
  receipt { lines, number, stage, table } = on (Proxy @"paid")
    ( \{ placed, total } -> Just
        { day: if placed / 86400 < diner.now / 86400 then .yesterday {} else .today {}
        , lines
        , number
        , placed
        , table
        , total
        }
    )
    (default Nothing)
    stage

-- | The sales of the dishes looked for, the best selling first.
soughtSales :: Diner -> Array DishSales
soughtSales diner = filter (\{ dish } -> Set.member dish names) (dishSales diner)
  where
  names = Set.fromFoldable (map _.name (sought diner))

-- | Sales per dish over the paid orders, at the prices the dishes were
-- | ordered at: the dish that sold for the most first, dishes level on
-- | that by name; a dish nobody bought is left out.
dishSales :: Diner -> Array DishSales
dishSales diner = sortBy (\a b -> compare b.amount a.amount <> compare a.dish b.dish) (map sales (Map.toUnfoldable sold))
  where
  sold :: Map String { amount :: Int, quantity :: Int }
  sold = Map.fromFoldableWith (\a b -> { amount: a.amount + b.amount, quantity: a.quantity + b.quantity }) do
    { lines } <- receipts diner
    { dish, price, quantity } <- lines
    pure (Tuple dish { amount: price * quantity, quantity })
  sales (Tuple dish { amount, quantity }) = { amount, dish, quantity }

-- | Orders by the time they were placed, then by number; an order not yet
-- | placed after every placed one.
oldestFirst :: Order -> Order -> Ordering
oldestFirst a b = compare (placedAt a) (placedAt b) <> compare a.number b.number
  where
  placedAt { stage } = match
    { taking: \_ -> top
    , cooking: \{ placed } -> placed
    , ready: \{ placed } -> placed
    , paid: \{ placed } -> placed
    } stage

-- | A price as written, in grosze: whole złoty, then at most a comma and one
-- | or two digits of grosze, spaces between the thousands allowed; anything
-- | else cannot be read.
priceRead :: Price -> Maybe Int
priceRead written = case split (Pattern ",") (replaceAll (Pattern " ") (Replacement "") (replaceAll (Pattern "\x00a0") (Replacement "") (trim written))) of
  [ whole ] -> map (_ * 100) (digits 7 whole)
  [ whole, grosze ] -> case Data.String.length grosze of
    1 -> (\z g -> z * 100 + g * 10) <$> digits 7 whole <*> digits 1 grosze
    2 -> (\z g -> z * 100 + g) <$> digits 7 whole <*> digits 2 grosze
    _ -> Nothing
  _ -> Nothing
  where
  digits most text =
    if text /= "" && Data.String.length text <= most && all (\c -> c >= '0' && c <= '9') (toCharArray text) then fromString text
    else Nothing

-- | A dish that can be ordered: some of it is in stock and its price can be
-- | read.
isOrderable :: forall r. { "Cena" :: Price, "Porcje" :: Portions | r } -> Boolean
isOrderable dish = portionsOf dish > 0 && isJust (priceRead dish."Cena")

-- | Whole minutes from a moment until now.
minutesSince :: Time -> Diner -> Int
minutesSince time { now } = max 0 ((now - time) / 60)

-- | Portions to be had: stock that cannot be read, or is negative, counts
-- | as none.
portionsOf :: forall r. { "Porcje" :: Portions | r } -> Int
portionsOf dish = match { count: max 0, invalid: \_ -> 0 } dish."Porcje"

-- | Stock changed by `f`, never below zero; stock that cannot be read, or is
-- | negative, is left as it is, since what is really there is unknown.
withPortions :: (Int -> Int) -> Dish -> Dish
withPortions f dish = match
  { count: \n -> if n < 0 then dish else dish { "Porcje" = .count (max 0 (f n)) }
  , invalid: \_ -> dish
  } dish."Porcje"

-- | Whether the table being served has an order in the kitchen or at the
-- | pass: it takes no new order until that one is paid.
isBusy :: Diner -> Boolean
isBusy { orders, table } = any (\o -> o.table == table && (isCooking o || isReady o)) orders

-- | The order being taken at the table being served, if one is open.
takenAt :: Diner -> Maybe Order
takenAt { orders, table } = find (\o -> o.table == table && isTaking o) orders

-- | One portion of the named dish onto the table's order, taken from stock,
-- | opening a new order if none is being taken; a dish new to the order goes
-- | on it at the price it has now, a dish already on it keeps the price it
-- | was put on at. A busy table or a dish that cannot be ordered changes
-- | nothing.
withOneMore :: String -> Diner -> Diner
withOneMore name diner = case find (\d -> d.name == name && isOrderable d) diner.dishes of
  Just dish | not (isBusy diner) -> diner
    { dishes = map (\d -> if d.name == name then withPortions (_ - 1) d else d) diner.dishes
    , orders = case takenAt diner of
      Just order -> map (\o -> if o.number == order.number then o { lines = oneMore dish o.lines } else o) diner.orders
      Nothing -> snoc diner.orders
        { number: foldl max 0 (map _.number diner.orders) + 1
        , table: diner.table
        , stage: .taking {}
        , lines: oneMore dish []
        }
    }
  _ -> diner
  where
  oneMore dish lines =
    if any (\l -> l.dish == name) lines then map (\l -> if l.dish == name then l { quantity = l.quantity + 1 } else l) lines
    else snoc lines { dish: name, price: fromMaybe 0 (priceRead dish."Cena"), quantity: 1, state: .ordered {} }

-- | An order still being taken.
isTaking :: Order -> Boolean
isTaking { stage } = on (Proxy @"taking") (const true) (default false) stage

-- | An order in the kitchen.
isCooking :: Order -> Boolean
isCooking { stage } = on (Proxy @"cooking") (const true) (default false) stage

-- | An order at the pass, waiting to be paid.
isReady :: Order -> Boolean
isReady { stage } = on (Proxy @"ready") (const true) (default false) stage

-- | An order paid for.
isPaid :: Order -> Boolean
isPaid { stage } = on (Proxy @"paid") (const true) (default false) stage

-- | The numbered order moved on by `step`, from the stage it has in the
-- | model; a stage `step` does not allow stays as it is.
advanced :: Int -> (Stage -> Maybe Stage) -> Diner -> Diner
advanced number step diner = diner { orders = map advance diner.orders }
  where
  advance o = if o.number == number then maybe o (\stage -> o { stage = stage }) (step o.stage) else o

-- | A time as hours:minutes of its day.
clockText :: Time -> String
clockText time = show (mod (time / 3600) 24) <> ":" <> pad (mod (time / 60) 60)

-- | An amount in grosze as złoty, the thousands set apart: `18 311,50 zł`.
money :: Int -> String
money cents = thousands (cents / 100) <> "," <> pad (mod cents 100) <> "\x00a0zł"
  where
  thousands n = if n < 1000 then show n else thousands (n / 1000) <> "\x00a0" <> pad (mod n 1000 / 10) <> show (mod n 10)

-- | Two digits.
pad :: Int -> String
pad r = if r < 10 then "0" <> show r else show r
