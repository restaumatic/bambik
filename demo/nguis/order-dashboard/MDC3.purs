module OrderDashboard.MDC3 (orderDashboard) where

import Prelude ((#), ($), Unit)

import OrderDashboard.ControlsMDC3 (board, gauge, leaderboard, rangePicker, statTile, trendChart)
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import OrderDashboard (kitchenLoad, openingDay, orderFlow, ordersArrive, ordersCount, revenue, tickPeriod, topDishes)
import PUI (fold, looped, ticks, with)
import PUI.Web.MDC3 (body, segment, topAppBar)
import QualifiedDo.Semigroupoid as Semigroupoid

orderDashboard :: Effect Unit
orderDashboard =
  body $ topAppBar "Order Dashboard" $ Semigroupoid.do
    rangePicker @"Showing" $ VariantToVariant.do
      segment @"Last minute"
      segment @"Last 15 min"
      segment @"Since open"
    board $ Semigroupoid.do
      statTile "Orders placed" ordersCount
      statTile "Revenue (EUR)" revenue
      gauge "Kitchen load" kitchenLoad
      trendChart "Order flow" orderFlow
      leaderboard "Top dishes" topDishes
    ticks @"Orders arrived" tickPeriod
    fold @"Orders arrived" ordersArrive
  # looped # with openingDay
