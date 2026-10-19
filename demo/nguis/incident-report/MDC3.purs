module IncidentReport.MDC3 (incidentReport) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import IncidentReport (clockPeriod, fileReport, referenceLine, reportLine)
import PUI (action)
import PUI.Web (clock, geolocated, staticText, text)
import PUI.Web.HTML (filePicked, label)
import PUI.Web.MDC3 (acknowledged, body, bodyMedium, button, indeterminateLinearProgress)
import QualifiedDo.Semigroupoid as Semigroupoid

incidentReport :: Effect Unit
incidentReport =
  body $ Semigroupoid.do
    RecordToRecord.do
      geolocated @"position"
      clock @"time" clockPeriod
      label $ Semigroupoid.do
        staticText "Photo "
        filePicked @"Photo"
    bodyMedium $ text reportLine
    button @"File the report"
    indeterminateLinearProgress # action @"File the report" fileReport
    acknowledged @"Report filed" $ bodyMedium $ text referenceLine
