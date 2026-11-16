module IncidentReport.MDC3 (incidentReport) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import IncidentReport (clockPeriod, fileReport, noDescription, referenceLine, reportLine)
import PUI (acknowledged, action, introduced, supplied)
import PUI.Web (clock, geolocated, staticText, text)
import PUI.Web.HTML (filePicked, label)
import PUI.Web.MDC3 (body, bodyMedium, button, filledTextField, indeterminateLinearProgress, modal)
import QualifiedDo.Semigroupoid as Semigroupoid

incidentReport :: Effect Unit
incidentReport =
  body $ Semigroupoid.do
    RecordToRecord.do
      geolocated @"position"
      clock @"time" clockPeriod
    filledTextField @"Description" # introduced @"Description" noDescription
    ( label $ Semigroupoid.do
      staticText "Photo "
      filePicked @"Photo" ) # supplied
    bodyMedium $ text reportLine
    button @"File the report"
    indeterminateLinearProgress # action @"File the report" fileReport
    ( modal @"Report filed" $ bodyMedium $ text referenceLine ) # acknowledged
