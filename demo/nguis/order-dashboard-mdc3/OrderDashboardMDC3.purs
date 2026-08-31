module OrderDashboardMDC3 (orderDashboardMDC3) where

import Prelude (Unit, ($), (#))

import DashboardControlsMDC3 (board, gauge, leaderboard, rangePicker, statTile, trendChart)
import Effect (Effect)
import OrderDashboardLogic (openingDay, ordersArrive, presentDashboard, tickPeriod)
import PUI (atField, every, mvu, required, settled)
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
            (statTile { label: "Orders", unit: "placed" } # atField @"ordersPlaced") # shown
            (statTile { label: "Revenue", unit: "EUR" } # atField @"revenue") # shown
            (gauge { label: "Kitchen load" } # atField @"kitchenLoad") # shown
            (trendChart { label: "Order flow" } # atField @"orderFlow") # shown
            (leaderboard { label: "Top dishes" } # atField @"topDishes") # shown
      ) # settled presentDashboard # mvu openingDay
