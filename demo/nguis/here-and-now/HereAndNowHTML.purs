module HereAndNowHTML (hereAndNowHTML) where

import Prelude (($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import HereAndNowViewModel (clockPeriod, connectionLine, schemeLine, whenLine, whereLine)
import PUI.Web (clock, colorScheme, geolocated, online, text)
import PUI.Web.HTML (body, div, h2, p)
import QualifiedDo.Semigroupoid as Semigroupoid

hereAndNowHTML :: Effect Unit
hereAndNowHTML =
  body $ div $ Semigroupoid.do
    RecordToRecord.do
      geolocated @"position"
      online @"connection"
      clock @"time" clockPeriod
      colorScheme @"scheme"
    h2 (text whereLine)
    p (text whenLine)
    p (text connectionLine)
    p (text schemeLine)
