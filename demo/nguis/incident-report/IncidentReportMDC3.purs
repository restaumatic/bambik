module IncidentReportMDC3 (incidentReportMDC3) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import IncidentReportViewModel (clockPeriod, fileReport, filedLine, reportLine)
import PUI (action, atCase)
import PUI.Web (clock, geolocated, staticText, text)
import PUI.Web.HTML (filePicked, label)
import PUI.Web.MDC3 (body, bodyMedium, button, indeterminateLinearProgress, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

incidentReportMDC3 :: Effect Unit
incidentReportMDC3 =
  body $ Semigroupoid.do
    RecordToRecord.do
      geolocated @"position"
      clock @"time" clockPeriod
      label $ Semigroupoid.do
        staticText "Photo "
        filePicked @"Photo"
    bodyMedium (text reportLine)
    button @"File the report"
    ( VariantToRecord.do
      indeterminateLinearProgress
      snackbar @"Report filed" filedLine ) # action @( "Report filed" :: { reference :: Int } ) fileReport # atCase @"File the report"
