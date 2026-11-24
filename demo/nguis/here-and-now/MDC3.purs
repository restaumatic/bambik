module HereAndNow.MDC3 (hereAndNow) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import HereAndNow (clockPeriod, connectionLine, languageLine, motionLine, pointerLine, schemeLine, whenLine, whereLine, windowLine)
import PUI.Web (colorScheme, geolocated, language, online, pointer, reducedMotion, text, timeZone, timestamped, viewport)
import PUI.Web.MDC3 (body, bodyMedium, bodySmall, headlineSmall)
import QualifiedDo.Semigroupoid as Semigroupoid

hereAndNow :: Effect Unit
hereAndNow =
  body $ Semigroupoid.do
    ( headlineSmall $ text whereLine ) # geolocated @"position"
    ( bodyMedium $ text whenLine ) # timeZone @"zone" # timestamped @"time" clockPeriod
    ( bodySmall $ text connectionLine ) # online @"connection"
    ( bodySmall $ text schemeLine ) # colorScheme @"scheme"
    ( bodySmall $ text motionLine ) # reducedMotion @"motion"
    ( bodySmall $ text pointerLine ) # pointer @"pointer"
    ( bodySmall $ text windowLine ) # viewport @"window"
    ( bodySmall $ text languageLine ) # language @"language"
