module IncidentReport.MDC3 (incidentReport) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import IncidentReport (clockPeriod, fileReport, noDescription, referenceLine, reportLine)
import PUI (acknowledged, action, introduced, supplied)
import PUI.Web (geolocated, geolocating, staticText, text, timestamped, timestamping)
import PUI.Web.HTML (filePicked, label)
import PUI.Web.MDC3 (body, bodyMedium, button, filledTextField, indeterminateLinearProgress, modal)
import QualifiedDo.Semigroupoid as Semigroupoid

incidentReport :: Effect Unit
incidentReport =
  body $ Semigroupoid.do
    filledTextField @"Description" # introduced @"Description" noDescription
    ( label $ Semigroupoid.do
      staticText "Photo "
      filePicked @"Photo" ) # supplied
    ( bodyMedium $ text reportLine ) # timestamped @"time" clockPeriod # geolocated @"position"
    button @"File the report" # timestamping @"time" # geolocating @"position"
    indeterminateLinearProgress # action @"File the report" fileReport
    ( modal @"Report filed" $ bodyMedium $ text referenceLine ) # acknowledged
