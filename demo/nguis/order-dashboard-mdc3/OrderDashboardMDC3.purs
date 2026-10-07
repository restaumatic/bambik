module OrderDashboardMDC3 (orderDashboardMDC3) where

import Prelude ((#), ($), Unit, identity)

import DashboardControlsMDC3 (board, gauge, leaderboard, rangePicker, statTile, trendChart)
import Effect (Effect)
import OrderDashboardViewModel (kitchenLoad, openingDay, orderFlow, ordersArrive, ordersCount, revenue, tickPeriod, topDishes)
import PUI (blankStatus, fold, looped, ticks, with)
import PUI.Web ((<+>), choice, shown)
import PUI.Web.MDC3 (body, topAppBar)
import QualifiedDo.Semigroupoid as Semigroupoid

orderDashboardMDC3 :: Effect Unit
orderDashboardMDC3 =
  body $
    topAppBar @"Order Dashboard" $ ( Semigroupoid.do
      rangePicker @"Showing" {}
        (choice @"Last minute" <+> choice @"Last 15 min" <+> choice @"Since open")
      board $ Semigroupoid.do
        statTile "Orders placed" ordersCount # shown
        statTile "Revenue (EUR)" revenue # shown
        gauge "Kitchen load" kitchenLoad # shown
        trendChart "Order flow" orderFlow # shown
        leaderboard "Top dishes" topDishes # shown
      blankStatus @"Orders arrived" # ticks tickPeriod
      blankStatus @"Orders arrived" # fold ordersArrive
    ) # looped
      @( tick :: Int
       , orders :: Array { id :: Int, dish :: String, total :: Number, at :: Int }
       , "Showing" :: [ "Last minute" :: {}, "Last 15 min" :: {}, "Since open" :: {} ]
       ) # with openingDay
