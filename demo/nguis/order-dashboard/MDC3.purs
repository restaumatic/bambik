module OrderDashboard.MDC3 (orderDashboard) where

import Prelude ((#), ($), Unit)

import OrderDashboard.ControlsMDC3 (board, gauge, leaderboard, rangePicker, statTile, trendChart)
import Effect (Effect)
import OrderDashboard (kitchenLoad, openingDay, orderFlow, ordersArrive, ordersCount, revenue, tickPeriod, topDishes)
import PUI (fold, looped, ticks, with)
import PUI.Web ((<+>), choice)
import PUI.Web.MDC3 (body, topAppBar)
import QualifiedDo.Semigroupoid as Semigroupoid

orderDashboard :: Effect Unit
orderDashboard =
  body $ topAppBar "Order Dashboard" $ Semigroupoid.do
    rangePicker @"Showing"
      (choice @"Last minute" <+> choice @"Last 15 min" <+> choice @"Since open")
    board $ Semigroupoid.do
      statTile "Orders placed" ordersCount
      statTile "Revenue (EUR)" revenue
      gauge "Kitchen load" kitchenLoad
      trendChart "Order flow" orderFlow
      leaderboard "Top dishes" topDishes
    ticks @"Orders arrived" tickPeriod
    fold @"Orders arrived" ordersArrive
  # looped
    @( tick :: Int
     , orders :: Array { id :: Int, dish :: String, total :: Number, at :: Int }
     , "Showing" :: [ "Last minute" :: {}, "Last 15 min" :: {}, "Since open" :: {} ]
     ) # with openingDay
