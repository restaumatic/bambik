module Venue.MDC2 (venue) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import PUI (acted, actedAt, actedOf, looped, with)
import PUI.Web (inCaseAt, text, textAt)
import PUI.Web.MDC2 (body, body2, card, columnHeader, dataCell, dataRow, dataTable, filledTextField, group, headline6, list, listItem, option, segment, segmentedButton, select, sliderLive, subtitle2)
import QualifiedDo.Semigroupoid as Semigroupoid
import Venue (addressLine, dayLine, deliveryLine, signLine, tradingDays, trattoria, zoneLine)

venue :: Effect Unit
venue =
  body $ Semigroupoid.do
    filledTextField @"Name"
    select @"Cuisine" $ VariantToVariant.do
      option @"Italian"
      option @"Japanese"
      option @"Mexican"
    group @"Address" $ Semigroupoid.do
      filledTextField @"Street"
      filledTextField @"City"
    group @"Opening hours" $ dataTable "Opening hours"
      ( Semigroupoid.do
        columnHeader "Day"
        columnHeader "Trading"
        columnHeader "Opens"
        columnHeader "Closes" )
      ( ( dataRow $ Semigroupoid.do
        dataCell $ textAt @"day"
        dataCell $ segmentedButton @"Trading" $ VariantToVariant.do
          segment @"Open"
          segment @"Closed"
        dataCell $ filledTextField @"Opens" # inCaseAt @"Open" @"Trading"
        dataCell $ filledTextField @"Closes" # inCaseAt @"Open" @"Trading" ) # acted @"day" )
    group @"Delivery" $ Semigroupoid.do
      segmentedButton @"Service" $ VariantToVariant.do
        segment @"None"
        segment @"Own couriers"
        segment @"Partner"
      filledTextField @"Partner name" # inCaseAt @"Partner" @"Service"
      ( list $
        ( listItem $ Semigroupoid.do
          subtitle2 $ text zoneLine
          sliderLive @"Fee" ) # actedAt @"zone" @"zones" ) # inCaseAt @"Own couriers" @"Service"
    card $ Semigroupoid.do
      headline6 $ text signLine
      body2 $ text addressLine
      list $ ( listItem $ text dayLine ) # actedOf @"day" tradingDays
      body2 $ text deliveryLine
  # looped # with trattoria
