module Quiz.MDC3 (quiz) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (fold, joined, looped, reset, with)
import PUI.Web (provided, text, textAt)
import PUI.Web.MDC3 (body, bodyLarge, button', headlineMedium, headlineSmall, linearProgress, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid
import Quiz (answer, askedPrompt, choices, finalScoreLine, freshQuizRun, questionLine, quizPhase, quizProgress)

quiz :: Effect Unit
quiz =
  body $ Semigroupoid.do
    linearProgress quizProgress
    RecordToVariant.do
      ( Semigroupoid.do
        bodyLarge $ text questionLine
        headlineMedium $ text askedPrompt
        listOf @"Question answered" @"key" choices $ textAt @"choice" ) # provided @"asking" quizPhase # joined @"Question answered"
      ( Semigroupoid.do
        headlineSmall $ text finalScoreLine
        button' @"Restart" { icon: "replay" } ) # provided @"finished" quizPhase
    VariantToRecord.do
      fold @"Question answered" answer
      reset @"Restart" freshQuizRun
  # looped # with freshQuizRun
