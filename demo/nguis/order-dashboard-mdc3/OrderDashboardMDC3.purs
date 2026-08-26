module OrderDashboardMDC3 (orderDashboardMDC3) where

import Prelude (Unit, ($), (#))

import DashboardControlsMDC3 (board, gauge, leaderboard, rangePicker, statTile, trendChart)
import Effect (Effect)
import OrderDashboardLogic (kitchenLoad, openingDay, orderFlow, ordersArrive, ordersCount, revenue, tickPeriod, topDishes)
import PUI (every, mvu, projected, required)
import PUI.Web (choice)
import PUI.Web.HTML (shown, body)
import PUI.Web.MDC3 (elevation5, topAppBar)
import QualifiedDo.Category as Category

orderDashboardMDC3 :: Effect Unit
orderDashboardMDC3 =
  body $
    elevation5 $
      topAppBar { title: "Order Dashboard" } $ ( Category.do
          every tickPeriod ordersArrive
          rangePicker @"Showing" {}
            [ choice @"Last minute", choice @"Last 15 min", choice @"Since open" ] # required
          board $ Category.do
            (statTile { label: "Orders", unit: "placed" } # projected ordersCount) # shown
            (statTile { label: "Revenue", unit: "EUR" } # projected revenue) # shown
            (gauge { label: "Kitchen load" } # projected kitchenLoad) # shown
            (trendChart { label: "Order flow" } # projected orderFlow) # shown
            (leaderboard { label: "Top dishes" } # projected topDishes) # shown
      ) # mvu openingDay
