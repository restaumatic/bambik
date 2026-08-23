module OrderDashboardMDC3 (orderDashboardMDC3) where

import Prelude (identity, Unit, ($), (#))

import DashboardControlsMDC3 (board, gauge, leaderboard, rangePicker, statTile, trendChart)
import Effect (Effect)
import OrderDashboardLogic (kitchenLoad, openingDay, orderFlow, ordersArrive, ordersCount, revenue, tickPeriod, topDishes)
import PUI (completed, every, mvu, projected, required)
import PUI.Web (choice)
import PUI.Web.HTML (shownAs, body)
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
            shownAs identity (statTile { label: "Orders", unit: "placed" } # projected ordersCount)
            shownAs identity (statTile { label: "Revenue", unit: "EUR" } # projected revenue)
            shownAs identity (gauge { label: "Kitchen load" } # projected kitchenLoad)
            shownAs identity (trendChart { label: "Order flow" } # projected orderFlow)
            shownAs identity (leaderboard { label: "Top dishes" } # projected topDishes)
      ) # mvu openingDay
