module OrderDashboardMDC3 (orderDashboardMDC3) where

import Prelude (Unit, ($), (#))

import DashboardControlsMDC3 (board, gauge, leaderboard, rangePicker, statTile, trendChart)
import Effect (Effect)
import OrderDashboardLogic (kitchenLoad, openingDay, orderFlow, ordersArrive, ordersCount, revenue, tickPeriod, topDishes)
import PUI (completed, tapped, every, mvu, projected, required)
import PUI.Web (choice)
import PUI.Web.HTML (body)
import PUI.Web.MDC3 (elevation5, topAppBar)
import QualifiedDo.Semigroupoid as Semigroupoid

orderDashboardMDC3 :: Effect Unit
orderDashboardMDC3 =
  body $
    elevation5 $
      topAppBar { title: "Order Dashboard" } $ ( Semigroupoid.do
          every tickPeriod ordersArrive
          rangePicker @"Showing" {}
            [ choice @"Last minute", choice @"Last 15 min", choice @"Since open" ] # required # completed
          board $ Semigroupoid.do
            statTile { label: "Orders", unit: "placed" } # projected ordersCount # tapped
            statTile { label: "Revenue", unit: "EUR" } # projected revenue # tapped
            gauge { label: "Kitchen load" } # projected kitchenLoad # tapped
            trendChart { label: "Order flow" } # projected orderFlow # tapped
            leaderboard { label: "Top dishes" } # projected topDishes # tapped
      ) # mvu openingDay
