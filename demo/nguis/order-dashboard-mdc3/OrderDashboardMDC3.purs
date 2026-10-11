module OrderDashboardMDC3 (orderDashboardMDC3) where

import Prelude ((#), ($), Unit)

import DashboardControlsMDC3 (board, gauge, leaderboard, rangePicker, statTile, trendChart)
import Effect (Effect)
import OrderDashboardViewModel (kitchenLoad, openingDay, orderFlow, ordersArrive, ordersCount, revenue, tickPeriod, topDishes)
import PUI (blankStatus, fold, looped, ticks, with)
import PUI.Pipeline as Pipeline
import PUI.Web ((<+>), choice)
import PUI.Web.MDC3 (body, topAppBar)

orderDashboardMDC3 :: Effect Unit
orderDashboardMDC3 =
  body $
    topAppBar "Order Dashboard" $ ( Pipeline.do
      rangePicker @"Showing" {}
        (choice @"Last minute" <+> choice @"Last 15 min" <+> choice @"Since open")
      board $ Pipeline.do
        statTile "Orders placed" ordersCount
        statTile "Revenue (EUR)" revenue
        gauge "Kitchen load" kitchenLoad
        trendChart "Order flow" orderFlow
        leaderboard "Top dishes" topDishes
      blankStatus @"Orders arrived" # ticks tickPeriod
      blankStatus @"Orders arrived" # fold ordersArrive
    ) # looped
      @( tick :: Int
       , orders :: Array { id :: Int, dish :: String, total :: Number, at :: Int }
       , "Showing" :: [ "Last minute" :: {}, "Last 15 min" :: {}, "Since open" :: {} ]
       ) # with openingDay
