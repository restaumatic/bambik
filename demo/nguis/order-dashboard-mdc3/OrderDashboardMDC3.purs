module OrderDashboardMDC3 (orderDashboardMDC3) where

import Prelude (Unit, ($), (#))

import DashboardControlsMDC3 (board, gauge, leaderboard, rangePicker, statTile, trendChart)
import Effect (Effect)
import OrderDashboardLogic (kitchenLoad, openingDay, orderFlow, ordersArrive, ordersCount, revenue, tickPeriod, topDishes)
import PUI (every, mvu, required)
import PUI.Web (choice)
import PUI.Web.HTML (shown)
import PUI.Web.MDC3 (body, topAppBar)
import QualifiedDo.Category as Category

orderDashboardMDC3 :: Effect Unit
orderDashboardMDC3 =
  body $
    topAppBar { title: "Order Dashboard" } $ ( Category.do
      every tickPeriod ordersArrive
      rangePicker @"Showing" {}
        [ choice @"Last minute", choice @"Last 15 min", choice @"Since open" ] # required
      board $ Category.do
        statTile @"Orders" { unit: "placed" } ordersCount # shown
        statTile @"Revenue" { unit: "EUR" } revenue # shown
        gauge @"Kitchen load" kitchenLoad # shown
        trendChart @"Order flow" orderFlow # shown
        leaderboard @"Top dishes" topDishes # shown
    ) # mvu openingDay
