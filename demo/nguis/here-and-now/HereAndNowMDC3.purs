module HereAndNowMDC3 (hereAndNowMDC3) where

import Prelude (($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import HereAndNowViewModel (clockPeriod, connectionLine, schemeLine, whenLine, whereLine)
import PUI.Web (clock, colorScheme, geolocated, online, text)
import PUI.Web.MDC3 (body, bodyMedium, bodySmall, headlineSmall)
import QualifiedDo.Semigroupoid as Semigroupoid

hereAndNowMDC3 :: Effect Unit
hereAndNowMDC3 =
  body $ Semigroupoid.do
    RecordToRecord.do
      geolocated @"position"
      online @"connection"
      clock @"time" clockPeriod
      colorScheme @"scheme"
    headlineSmall (text whereLine)
    bodyMedium (text whenLine)
    bodySmall (text connectionLine)
    bodySmall (text schemeLine)
