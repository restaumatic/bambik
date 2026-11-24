module HereAndNow.HTML (hereAndNow) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import HereAndNow (clockPeriod, connectionLine, languageLine, motionLine, pointerLine, schemeLine, whenLine, whereLine, windowLine)
import PUI.Web (colorScheme, geolocated, language, online, pointer, reducedMotion, text, timeZone, timestamped, viewport)
import PUI.Web.HTML (body, div, h2, p)
import QualifiedDo.Semigroupoid as Semigroupoid

hereAndNow :: Effect Unit
hereAndNow =
  body $ div $ Semigroupoid.do
    ( h2 $ text whereLine ) # geolocated @"position"
    ( p $ text whenLine ) # timeZone @"zone" # timestamped @"time" clockPeriod
    ( p $ text connectionLine ) # online @"connection"
    ( p $ text schemeLine ) # colorScheme @"scheme"
    ( p $ text motionLine ) # reducedMotion @"motion"
    ( p $ text pointerLine ) # pointer @"pointer"
    ( p $ text windowLine ) # viewport @"window"
    ( p $ text languageLine ) # language @"language"
