module Diner.Fixtures
  ( emptyDiner
  , rushHour
  , soldOut
  ) where

import Prelude (map)

import Diner (Diner, dinnerService)

-- Initial models the part tests (`MDC3/*.test.mjs`, `npm run part --
-- diner`) run a part of the view from; `dinnerService` is the app's own.

-- | No order anywhere: every station shows its empty state.
emptyDiner :: Diner
emptyDiner = dinnerService { orders = [] }

-- | Every dish out of stock.
soldOut :: Diner
soldOut = dinnerService { dishes = map (_ { "Porcje" = .count 0 }) dinnerService.dishes }

-- | Four tables in the kitchen, two at the pass, one order being taken.
rushHour :: Diner
rushHour = dinnerService
  { "Stolik" = ."6" {}
  , orders =
    [ { number: 11, table: ."1" {}, stage: .cooking { placed: { hours: 12, minutes: 2 } }, lines: [ { dish: "Carbonara", quantity: 2, state: .plated {} }, { dish: "Tiramisu", quantity: 2, state: .ordered {} } ] }
    , { number: 12, table: ."2" {}, stage: .cooking { placed: { hours: 12, minutes: 5 } }, lines: [ { dish: "Risotto", quantity: 1, state: .ordered {} } ] }
    , { number: 13, table: ."3" {}, stage: .cooking { placed: { hours: 12, minutes: 9 } }, lines: [ { dish: "Margherita", quantity: 3, state: .ordered {} } ] }
    , { number: 14, table: ."4" {}, stage: .cooking { placed: { hours: 12, minutes: 14 } }, lines: [ { dish: "Margherita", quantity: 1, state: .plated {} }, { dish: "Carbonara", quantity: 1, state: .ordered {} } ] }
    , { number: 9, table: ."5" {}, stage: .ready { placed: { hours: 11, minutes: 50 } }, lines: [ { dish: "Risotto", quantity: 2, state: .plated {} } ] }
    , { number: 10, table: ."6" {}, stage: .taking {}, lines: [ { dish: "Tiramisu", quantity: 1, state: .ordered {} } ] }
    ]
  }
