module OrderDashboardMDC3 (orderDashboardMDC3) where

import Prelude ((#), ($), Unit)

import DashboardControlsMDC3 (board, gauge, leaderboard, rangePicker, statTile, trendChart)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import OrderDashboardViewModel (kitchenLoad, openingDay, orderFlow, ordersArrive, ordersCount, revenue, tickPeriod, topDishes)
import PUI (fold, looped, prefixed, ticks, unshown, with)
import PUI.Web ((<+>), choice)
import PUI.Web.MDC3 (body, topAppBar)
import QualifiedDo.Semigroupoid as Semigroupoid

orderDashboardMDC3 :: Effect Unit
orderDashboardMDC3 =
  body $
    topAppBar "Order Dashboard" $ ( Semigroupoid.do
      rangePicker @"Showing"
        (choice @"Last minute" <+> choice @"Last 15 min" <+> choice @"Since open")
      prefixed (board $ RecordToRecord.do
        statTile "Orders placed" ordersCount
        statTile "Revenue (EUR)" revenue
        gauge "Kitchen load" kitchenLoad
        trendChart "Order flow" orderFlow
        leaderboard "Top dishes" topDishes ) $ unshown @"Orders arrived" # ticks tickPeriod
      unshown @"Orders arrived" # fold ordersArrive
    ) # looped
      @( tick :: Int
       , orders :: Array { id :: Int, dish :: String, total :: Number, at :: Int }
       , "Showing" :: [ "Last minute" :: {}, "Last 15 min" :: {}, "Since open" :: {} ]
       ) # with openingDay
