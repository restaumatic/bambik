module QuizMDC3 (quizMDC3) where

import Prelude ((#), ($), Unit, const)

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (fold, joined, looped, with)
import PUI.Web (provided, shown, text)
import PUI.Web.MDC3 (body, bodyLarge, button, headlineMedium, headlineSmall, linearProgress, listOf, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid
import QuizViewModel (answer, askedPrompt, finalScoreLine, freshQuizRun, questionAnsweredLine, questionLine, quizPhase, quizProgress, quizRestartedLine)

quizMDC3 :: Effect Unit
quizMDC3 =
  body $
    Semigroupoid.do
      linearProgress quizProgress # shown
      ( bodyLarge $ text questionLine ) # shown
      RecordToVariant.do
        ( Semigroupoid.do
          headlineMedium (text askedPrompt) # shown
          listOf @"Question answered" @"key" {} _.choices (text _.label) ) # provided @"asking" @( asking :: { prompt :: String, choices :: Array { key :: Int, label :: String } }, finished :: { correct :: Int } ) quizPhase # joined @"Question answered"
        ( Semigroupoid.do
          headlineSmall (text finalScoreLine) # shown
          button @"Restart" { icon: "replay" } ) # provided @"finished" quizPhase
      VariantToRecord.do
        snackbar @"Question answered" questionAnsweredLine # fold answer
        snackbar @"Restart" quizRestartedLine # fold (const freshQuizRun)
    # looped @( question :: Int, correct :: Int ) # with freshQuizRun
