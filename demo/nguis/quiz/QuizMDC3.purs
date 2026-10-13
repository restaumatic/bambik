module QuizMDC3 (quizMDC3) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (fold, joined, looped, reset, unshown, with)
import PUI.Web (provided, text, textAt)
import PUI.Web.MDC3 (body, bodyLarge, button', headlineMedium, headlineSmall, linearProgress, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid
import QuizViewModel (answer, askedPrompt, choices, finalScoreLine, freshQuizRun, questionLine, quizPhase, quizProgress)

quizMDC3 :: Effect Unit
quizMDC3 =
  body $ Semigroupoid.do
    linearProgress quizProgress
    bodyLarge $ text questionLine
    RecordToVariant.do
      ( Semigroupoid.do
        headlineMedium (text askedPrompt)
        listOf @"Question answered" @"key" @( key :: Int, label :: String ) choices (textAt @"label") ) # provided @"asking" @( asking :: {}, finished :: {} ) quizPhase # joined @"Question answered"
      ( Semigroupoid.do
        headlineSmall (text finalScoreLine)
        button' @"Restart" { icon: "replay" } ) # provided @"finished" quizPhase
    VariantToRecord.do
      unshown @"Question answered" # fold answer
      unshown @"Restart" # reset freshQuizRun
  # looped @( question :: Int, correct :: Int ) # with freshQuizRun
