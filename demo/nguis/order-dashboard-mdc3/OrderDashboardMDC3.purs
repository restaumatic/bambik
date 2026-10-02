module OrderDashboardMDC3 (orderDashboardMDC3) where

import Prelude (Unit, ($), (#))

import DashboardControlsMDC3 (board, gauge, leaderboard, rangePicker, statTile, trendChart)
import Effect (Effect)
import OrderDashboardLogic (kitchenLoad, openingDay, orderFlow, ordersArrive, ordersCount, revenue, tickPeriod, topDishes)
import PUI (every, mvu)
import PUI.Web ((<+>), choice, shown)
import PUI.Web.MDC3 (body, topAppBar)
import QualifiedDo.Semigroupoid as Semigroupoid

orderDashboardMDC3 :: Effect Unit
orderDashboardMDC3 =
  body $
    topAppBar @"Order Dashboard" $ ( Semigroupoid.do
      every tickPeriod ordersArrive
      rangePicker @"Showing" {}
        (choice @"Last minute" <+> choice @"Last 15 min" <+> choice @"Since open")
      board $ Semigroupoid.do
        statTile @"Orders placed" ordersCount # shown
        statTile @"Revenue (EUR)" revenue # shown
        gauge @"Kitchen load" kitchenLoad # shown
        trendChart @"Order flow" orderFlow # shown
        leaderboard @"Top dishes" topDishes # shown
    ) # mvu openingDay
