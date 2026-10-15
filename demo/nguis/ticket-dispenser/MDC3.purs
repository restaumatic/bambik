module TicketDispenser.MDC3 (ticketDispenser) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import PUI (fold, looped, with)
import PUI.Web (inCaseAt, staticText, text)
import PUI.Web.MDC3 (body, bodyMedium, button, displaySmall)
import QualifiedDo.Semigroupoid as Semigroupoid
import TicketDispenser (emptyQueue, issue, servingLine, ticketLine)

ticketDispenser :: Effect Unit
ticketDispenser =
  body $ Semigroupoid.do
    displaySmall $ Semigroupoid.do
      staticText "—" # inCaseAt @"waiting" @"display"
      text ticketLine # inCaseAt @"serving" @"display"
    bodyMedium $ Semigroupoid.do
      staticText "Press the button to draw the first ticket." # inCaseAt @"waiting" @"display"
      text servingLine # inCaseAt @"serving" @"display"
    button @"Take a number"
    fold @"Take a number" issue
  # looped # with emptyQueue
