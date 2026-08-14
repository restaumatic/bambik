module OrderDashboardMDC3 (orderDashboardMDC3) where

import Prelude (Unit, ($), (#))

import DashboardControlsMDC3 (board, gauge, leaderboard, rangePicker, statTile, trendChart)
import Effect (Effect)
import OrderDashboardLogic (kitchenLoad, openingDay, orderFlow, ordersArrive, ordersCount, revenue, tickPeriod, topDishes)
import PUI (completed, displayed, every, mvu, projected, required)
import PUI.Web.HTML (body)
import PUI.Web.MDC3 (elevation5, topAppBar)
import QualifiedDo.Semigroupoid as Semigroupoid

orderDashboardMDC3 :: Effect Unit
orderDashboardMDC3 =
  body $
    elevation5 $
      topAppBar { title: "Order Dashboard" } $ ( Semigroupoid.do
          every tickPeriod ordersArrive
          rangePicker @"window" { label: "Showing" }
            [ { value: .lastMinute {}, label: "Last minute" }
            , { value: .lastQuarter {}, label: "Last 15 min" }
            , { value: .sinceOpen {}, label: "Since open" }
            ] # required # completed
          board $ Semigroupoid.do
            statTile { label: "Orders", unit: "placed" } # projected @"value" ordersCount # displayed
            statTile { label: "Revenue", unit: "EUR" } # projected @"value" revenue # displayed
            gauge { label: "Kitchen load" } # projected @"value" kitchenLoad # displayed
            trendChart { label: "Order flow" } # projected @"value" orderFlow # displayed
            leaderboard { label: "Top dishes" } # projected @"value" topDishes # displayed
      ) # mvu openingDay
