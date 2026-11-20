-- | The stand-in server: the restaurant's day as its back office keeps it,
-- | and the requests the application makes of it. Nothing leaves the browser:
-- | a day is generated from its load, as of the moment it is asked for, and
-- | every answer comes after a simulated latency.
module Diner.Service
  ( Course
  , Day
  , Dish
  , Line
  , Load
  , Order
  , Portions
  , Price
  , Stage
  , Time
  , dayAt
  , standardLoad
  , taken
  , today
  ) where

import Prelude (Unit, bind, discard, map, max, mod, pure, show, (*), (+), (-), (/), (<), (<$>), (<=), (<>), (==), (>=))

import Data.Array (index, length, mapMaybe, nubByEq, range, sortWith)
import Data.DateTime.Instant (unInstant)
import Data.Foldable (sum)
import Data.Int (floor, round, toNumber)
import Data.Int.Bits (shl, xor, zshr)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..), Minutes(..))
import Effect (Effect)
import Effect.Aff (Aff, delay)
import Effect.Class (liftEffect)
import Effect.Now (getTimezoneOffset, now)
import Effect.Random (randomInt)

-- | A restaurant's day: the menu with its stock, the hall's tables, numbered
-- | from 1, every order since the day began, and the time it is.
type Day = { dishes :: Array Dish, now :: Time, orders :: Array Order, tables :: Int }

-- | A dish on the menu: its price, the course it belongs to and the
-- | portions left in stock.
type Dish = { "Cena" :: Price, group :: Course, name :: String, "Porcje" :: Portions }

-- | A price in złoty as written on the menu, a comma before the grosze.
type Price = String

-- | The courses of the menu, in the order they are served.
type Course =
  [ "Przystawki" :: {}
  , "Zupy" :: {}
  , "Makarony" :: {}
  , "Dania główne" :: {}
  , "Desery" :: {}
  ]

-- | Stock as counted, or as written when it cannot be read as a whole number.
type Portions = [ count :: Int, invalid :: String ]

-- | A table's order: numbered as orders are opened, its lines, and how far
-- | along it is.
type Order = { lines :: Array Line, number :: Int, stage :: Stage, table :: Int }

-- | A dish ordered in some quantity, at the price in grosze it had when it
-- | was put on the order, and whether the kitchen has plated it.
type Line = { dish :: String, price :: Int, quantity :: Int, state :: [ ordered :: {}, plated :: {} ] }

-- | An order's life: taken at the table, cooking from the time it was placed,
-- | ready at the pass until it is paid, then paid for its total.
type Stage =
  [ cooking :: { placed :: Time }
  , paid :: { placed :: Time, total :: Int }
  , ready :: { placed :: Time }
  , taking :: {}
  ]

-- | A moment of local time, in seconds.
type Time = Int

-- | How busy a generated day is: the size of the menu and of the hall, and
-- | the orders at each stage; `seed` picks one such day among many.
type Load =
  { cooking :: Int
  , dishes :: Int
  , paid :: Int
  , ready :: Int
  , seed :: Int
  , tables :: Int
  , taking :: Int
  }

-- | A busy day in a large restaurant: a long menu, a full hall, a couple of
-- | dozen orders already paid.
standardLoad :: Load
standardLoad = { cooking: 20, dishes: 304, paid: 219, ready: 10, seed: 7, tables: 42, taking: 3 }

-- | The day as of now, at the standard load.
today :: Aff Day
today = do
  answering { least: 700, most: 1300 }
  dayAt standardLoad <$> liftEffect localNow

-- | A placed order taken by the kitchen: its number comes back once it has.
taken :: Int -> Aff Int
taken number = do
  answering { least: 150, most: 450 }
  pure number

-- | A day of the given load as of the given time: the paid orders placed
-- | over the 24 hours before it, the ones at the pass and in the kitchen
-- | within the last hour, each open order at a table of its own, whichever
-- | its number. The same load and time give the same day.
dayAt :: Load -> Time -> Day
dayAt load time = { dishes, now: time, orders, tables: load.tables }
  where
  -- the menu with every price in grosze, as the orders' lines keep them
  menu = map (\i -> { cents: 650 + 50 * drawn 1 i 40, course: (dishAt i).course, i, name: (dishAt i).name }) (range 0 (load.dishes - 1))
  dishes = map dish menu
  dish { cents, course, i, name } =
    { name
    , group: course
    , "Cena": show (cents / 100) <> "," <> (if mod cents 100 < 10 then "0" else "") <> show (mod cents 100)
    , "Porcje": .count (if drawn 2 i 9 == 0 then 0 else 2 + drawn 3 i 18)
    }
  orders = mapMaybe order (range 1 (load.paid + pipeline + load.taking))
  pipeline = load.ready + load.cooking
  -- numbered in the order they were opened: the paid ones, then the open
  -- ones, each at a table of its own drawn by lot, as many as the hall seats
  order number
    | number <= load.paid = Just
        { number
        , table: 1 + drawn 4 number load.tables
        , lines: linesOf number \_ -> .plated {}
        , stage: .paid { placed: paidAt number, total: sum (map (\p -> p.quantity * p.dish.cents) (picks number)) }
        }
    | number <= load.paid + load.ready = seated (number - load.paid) \table ->
        { number, table, lines: linesOf number \_ -> .plated {}, stage: .ready { placed: placedAt (number - load.paid) } }
    | number <= load.paid + pipeline = seated (number - load.paid) \table ->
        { number
        , table
        , lines: linesOf number \slot -> if drawn 10 (number * 8 + slot) 3 == 0 then .ordered {} else .plated {}
        , stage: .cooking { placed: placedAt (number - load.paid) }
        }
    | true = seated (number - load.paid) \table ->
        { number, table, lines: linesOf number \_ -> .ordered {}, stage: .taking {} }
  -- the `seat`-th open order at the `seat`-th table of the hall as shuffled
  seated seat opened = map opened (index shuffled (seat - 1))
  shuffled = map (\{ table } -> table) (sortWith (\{ lot } -> lot) (map (\table -> { table, lot: drawn 11 table 1000003 }) (range 1 load.tables)))
  -- the paid orders placed evenly over the day before the last hour
  paidAt number = time - 86400 + (number * 82800) / (load.paid + 1) + drawn 5 number 300
  -- the open orders placed over the last 50 minutes, oldest first
  placedAt seat = time - 3000 + ((seat - 1) * 2940) / max 1 pipeline + drawn 6 seat 60
  linesOf number plating = map (\p -> { dish: p.dish.name, price: p.dish.cents, quantity: p.quantity, state: plating p.slot }) (picks number)
  -- one to four dishes, no dish twice
  picks number = nubByEq (\a b -> a.dish.name == b.dish.name) (mapMaybe (pick number) (range 1 (1 + drawn 7 number 4)))
  pick number slot = map (\d -> { dish: d, quantity: 1 + drawn 9 (number * 8 + slot) 3, slot })
    (index menu (drawn 8 (number * 8 + slot) (length menu)))
  drawn stream i below = if below < 1 then 0 else mod (scrambled (load.seed * 7919 + stream * 104729 + i)) below

-- | The `i`-th dish of a menu: thirty dishes, each in ten variations, and
-- | numbered editions of them beyond that.
dishAt :: Int -> { course :: Course, name :: String }
dishAt i = { course: base.course, name: base.name <> variation <> edition }
  where
  base = fromMaybe { course: ."Dania główne" {}, name: "" } (index bases (mod i (length bases)))
  variation = fromMaybe "" (index variations (mod (i / length bases) (length variations)))
  edition = if i >= length bases * length variations then " " <> show (1 + i / (length bases * length variations)) else ""

bases :: Array { course :: Course, name :: String }
bases =
  [ pasta "Carbonara", main "Margherita", main "Risotto", dessert "Tiramisu", pasta "Lasagne", pasta "Gnocchi"
  , pasta "Ravioli", starter "Bruschetta", soup "Minestrone", starter "Caprese", starter "Focaccia", main "Calzone"
  , pasta "Tagliatelle", pasta "Penne", pasta "Pappardelle", main "Ossobuco", main "Saltimbocca", dessert "Panna cotta"
  , dessert "Cannoli", dessert "Affogato", starter "Arancini", main "Polenta", starter "Carpaccio"
  , starter "Vitello tonnato", main "Parmigiana", soup "Ribollita", pasta "Tortellini", pasta "Cacio e pepe"
  , starter "Panzanella", dessert "Zabaglione"
  ]
  where
  starter name = { course: ."Przystawki" {}, name }
  soup name = { course: ."Zupy" {}, name }
  pasta name = { course: ."Makarony" {}, name }
  main name = { course: ."Dania główne" {}, name }
  dessert name = { course: ."Desery" {}, name }

variations :: Array String
variations =
  [ "", " z truflami", " z grzybami", " ze szpinakiem", " z krewetkami", " z kurczakiem", " z pesto"
  , " z gorgonzolą", " z pomidorami", " z orzechami"
  ]

-- A non-negative number scattered from `x`: three xorshift rounds, exact in
-- 32-bit integers.
scrambled :: Int -> Int
scrambled x = zshr (round3 (round3 (round3 (x + 0x2545F491)))) 1
  where
  round3 a =
    let
      b = xor a (shl a 13)
      c = xor b (zshr b 17)
    in xor c (shl c 5)

-- The local time, in seconds.
localNow :: Effect Time
localNow = do
  Milliseconds sinceEpoch <- unInstant <$> now
  Minutes offset <- getTimezoneOffset
  pure (floor (sinceEpoch / 1000.0) - 60 * round offset)

-- An answer's latency, somewhere between the two bounds in milliseconds.
answering :: { least :: Int, most :: Int } -> Aff Unit
answering { least, most } = do
  ms <- liftEffect (randomInt least most)
  delay (Milliseconds (toNumber ms))
