module TicketDispenser.MDC2 (ticketDispenser) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import PUI (fold, looped, with)
import PUI.Web (inCaseAt, staticText, text)
import PUI.Web.MDC2 (body, body2, button, headline3)
import QualifiedDo.Semigroupoid as Semigroupoid
import TicketDispenser (emptyQueue, issue, servingLine, ticketLine)

ticketDispenser :: Effect Unit
ticketDispenser =
  body $ Semigroupoid.do
    headline3 $ Semigroupoid.do
      staticText "—" # inCaseAt @"waiting" @"display"
      text ticketLine # inCaseAt @"serving" @"display"
    body2 $ Semigroupoid.do
      staticText "Press the button to draw the first ticket." # inCaseAt @"waiting" @"display"
      text servingLine # inCaseAt @"serving" @"display"
    button @"Take a number"
    fold @"Take a number" issue
  # looped # with emptyQueue
