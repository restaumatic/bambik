module Diner.Fixtures
  ( busyHall
  , busyKitchen
  , busyPass
  , busyReport
  , busyStatistics
  , busyStockroom
  , dinnerService
  , emptyDiner
  , rushHour
  , soldOut
  ) where

import Prelude (map, (*), (+))

import Diner (Diner, Station, Time)
import Diner.Service (dayAt, standardLoad)

-- Initial models the part tests (`test/View/*.test.mjs`, `npm run part --
-- diner`) run a part of the view from.

-- | The restaurant mid-service: three orders paid, one cooking, one waiting
-- | at the pass, and table 1 free to order.
dinnerService :: Diner
dinnerService =
  { "Grupa magazynu": ."Wszystkie" {}
  , "Grupa menu": ."Wszystkie" {}
  , "Grupa statystyk": ."Wszystkie" {}
  , "Stanowisko": ."Sala" {}
  , "Szukaj w magazynie": ""
  , "Szukaj w menu": ""
  , "Szukaj w statystykach": ""
  , now: at 9 10
  , perPage: { list: 10, tiles: 24 }
  , table: 1
  , tables: 6
  , turned: []
  , dishes:
    [ { name: "Carbonara", group: ."Makarony" {}, "Cena": "10,50", "Porcje": .count 6 }
    , { name: "Margherita", group: ."Dania główne" {}, "Cena": "9,50", "Porcje": .count 8 }
    , { name: "Risotto", group: ."Dania główne" {}, "Cena": "12,00", "Porcje": .count 3 }
    , { name: "Tiramisu", group: ."Desery" {}, "Cena": "6,50", "Porcje": .count 2 }
    ]
  , orders:
    [ { number: 1
      , table: 2
      , stage: .paid { placed: at 8 5, total: 2750 }
      , lines:
        [ { dish: "Carbonara", price: 1050, quantity: 2, state: .plated {} }
        , { dish: "Tiramisu", price: 650, quantity: 1, state: .plated {} }
        ]
      }
    , { number: 2
      , table: 5
      , stage: .paid { placed: at 8 20, total: 2150 }
      , lines:
        [ { dish: "Risotto", price: 1200, quantity: 1, state: .plated {} }
        , { dish: "Margherita", price: 950, quantity: 1, state: .plated {} }
        ]
      }
    , { number: 3
      , table: 1
      , stage: .paid { placed: at 8 41, total: 2850 }
      , lines:
        [ { dish: "Margherita", price: 950, quantity: 3, state: .plated {} } ]
      }
    , { number: 4
      , table: 3
      , stage: .cooking { placed: at 8 58 }
      , lines:
        [ { dish: "Margherita", price: 950, quantity: 2, state: .plated {} }
        , { dish: "Tiramisu", price: 650, quantity: 1, state: .ordered {} }
        ]
      }
    , { number: 5
      , table: 4
      , stage: .ready { placed: at 9 4 }
      , lines:
        [ { dish: "Carbonara", price: 1050, quantity: 1, state: .plated {} }
        , { dish: "Risotto", price: 1200, quantity: 1, state: .plated {} }
        ]
      }
    ]
  }

-- | No order anywhere: every station shows its empty state.
emptyDiner :: Diner
emptyDiner = dinnerService { orders = [] }

-- | Every dish out of stock.
soldOut :: Diner
soldOut = dinnerService { dishes = map (_ { "Porcje" = .count 0 }) dinnerService.dishes }

-- | Four orders in the kitchen, one at the pass, one being taken.
rushHour :: Diner
rushHour = dinnerService
  { now = at 12 20
  , table = 6
  , orders =
    [ { number: 11, table: 1, stage: .cooking { placed: at 12 2 }, lines: [ { dish: "Carbonara", price: 1050, quantity: 2, state: .plated {} }, { dish: "Tiramisu", price: 650, quantity: 2, state: .ordered {} } ] }
    , { number: 12, table: 2, stage: .cooking { placed: at 12 5 }, lines: [ { dish: "Risotto", price: 1200, quantity: 1, state: .ordered {} } ] }
    , { number: 13, table: 3, stage: .cooking { placed: at 12 9 }, lines: [ { dish: "Margherita", price: 950, quantity: 3, state: .ordered {} } ] }
    , { number: 14, table: 4, stage: .cooking { placed: at 12 14 }, lines: [ { dish: "Margherita", price: 950, quantity: 1, state: .plated {} }, { dish: "Carbonara", price: 1050, quantity: 1, state: .ordered {} } ] }
    , { number: 9, table: 5, stage: .ready { placed: at 11 50 }, lines: [ { dish: "Risotto", price: 1200, quantity: 2, state: .plated {} } ] }
    , { number: 10, table: 6, stage: .taking {}, lines: [ { dish: "Tiramisu", price: 650, quantity: 1, state: .ordered {} } ] }
    ]
  }

-- | A busy day in a large restaurant, at one in the afternoon, worked at
-- | each station in turn: a long menu, a full hall, a couple of hundred orders paid.
-- | The hall serves table 5, one of the free ones.
busyStockroom :: Diner
busyStockroom = busyAt (."Magazyn" {})

busyHall :: Diner
busyHall = (busyAt (."Sala" {})) { table = 5 }

busyKitchen :: Diner
busyKitchen = busyAt (."Kuchnia" {})

busyPass :: Diner
busyPass = busyAt (."Wydawka" {})

busyReport :: Diner
busyReport = busyAt (."Raport" {})

busyStatistics :: Diner
busyStatistics = busyAt (."Statystyki" {})

busyAt :: Station -> Diner
busyAt station =
  { "Grupa magazynu": ."Wszystkie" {}
  , "Grupa menu": ."Wszystkie" {}
  , "Grupa statystyk": ."Wszystkie" {}
  , "Stanowisko": station
  , "Szukaj w magazynie": ""
  , "Szukaj w menu": ""
  , "Szukaj w statystykach": ""
  , dishes: day.dishes
  , now: day.now
  , orders: day.orders
  , perPage: { list: 10, tiles: 24 }
  , table: 1
  , tables: day.tables
  , turned: []
  }
  where
  day = dayAt standardLoad (at 13 0)

-- A time of the day the fixtures are set on.
at :: Int -> Int -> Time
at hours minutes = 20777 * 86400 + hours * 3600 + minutes * 60
