module Quiz.MDC2 (quiz) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (fold, joined, looped, reset, with)
import PUI.Web (provided, text, textAt)
import PUI.Web.MDC2 (body, body1, button', headline5, headline6, linearProgress, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid
import Quiz (answer, askedPrompt, choices, finalScoreLine, freshQuizRun, questionLine, quizPhase, quizProgress)

quiz :: Effect Unit
quiz =
  body $ Semigroupoid.do
    linearProgress quizProgress
    body1 $ text questionLine
    RecordToVariant.do
      ( Semigroupoid.do
        headline5 $ text askedPrompt
        listOf @"Question answered" @"key" @( key :: Int, label :: String ) choices $ textAt @"label" ) # provided @"asking" @( asking :: {}, finished :: {} ) quizPhase # joined
      ( Semigroupoid.do
        headline6 $ text finalScoreLine
        button' @"Restart" { icon: "replay" } ) # provided @"finished" quizPhase
    VariantToRecord.do
      fold @"Question answered" answer
      reset @"Restart" freshQuizRun
  # looped @( question :: Int, correct :: Int ) # with freshQuizRun
