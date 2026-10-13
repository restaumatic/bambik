module TicketDispenserMDC3 (ticketDispenserMDC3) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import PUI (fold, looped, unshown, with)
import PUI.Web (inCaseAt, staticText, text)
import PUI.Web.MDC3 (body, bodyMedium, button, displaySmall)
import QualifiedDo.Semigroupoid as Semigroupoid
import TicketDispenserViewModel (emptyQueue, issue, servingLine, ticketLine)

ticketDispenserMDC3 :: Effect Unit
ticketDispenserMDC3 =
  body $ Semigroupoid.do
    displaySmall $ Semigroupoid.do
      staticText "—" # inCaseAt @"waiting" @"display"
      text ticketLine # inCaseAt @"serving" @"display"
    bodyMedium $ Semigroupoid.do
      staticText "Press the button to draw the first ticket." # inCaseAt @"waiting" @"display"
      text servingLine # inCaseAt @"serving" @"display"
    button @"Take a number"
    unshown @"Take a number" # fold issue
  # looped
    @( display :: [ waiting :: {}, serving :: { number :: Int } ]
     , next :: Int
     ) # with emptyQueue
