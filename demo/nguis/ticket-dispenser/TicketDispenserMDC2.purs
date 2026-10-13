module TicketDispenserMDC2 (ticketDispenserMDC2) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import PUI (fold, looped, unshown, with)
import PUI.Web (inCaseAt, staticText, text)
import PUI.Web.MDC2 (body, body2, button, headline3)
import QualifiedDo.Semigroupoid as Semigroupoid
import TicketDispenserViewModel (emptyQueue, issue, servingLine, ticketLine)

ticketDispenserMDC2 :: Effect Unit
ticketDispenserMDC2 =
  body $ Semigroupoid.do
    headline3 $ Semigroupoid.do
      staticText "—" # inCaseAt @"waiting" @"display"
      text ticketLine # inCaseAt @"serving" @"display"
    body2 $ Semigroupoid.do
      staticText "Press the button to draw the first ticket." # inCaseAt @"waiting" @"display"
      text servingLine # inCaseAt @"serving" @"display"
    button @"Take a number"
    unshown @"Take a number" # fold issue
  # looped
    @( display :: [ waiting :: {}, serving :: { number :: Int } ]
     , next :: Int
     ) # with emptyQueue
