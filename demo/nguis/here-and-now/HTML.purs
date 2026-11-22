module HereAndNow.HTML (hereAndNow) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import HereAndNow (clockPeriod, connectionLine, schemeLine, whenLine, whereLine)
import PUI.Web (colorScheme, geolocated, online, text, timestamped)
import PUI.Web.HTML (body, div, h2, p)
import QualifiedDo.Semigroupoid as Semigroupoid

hereAndNow :: Effect Unit
hereAndNow =
  body $ div $ Semigroupoid.do
    RecordToRecord.do
      geolocated @"position"
      online @"connection"
      colorScheme @"scheme"
    h2 $ text whereLine
    ( p $ text whenLine ) # timestamped @"time" clockPeriod
    p $ text connectionLine
    p $ text schemeLine
