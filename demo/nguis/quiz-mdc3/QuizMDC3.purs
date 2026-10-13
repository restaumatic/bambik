module QuizMDC3 (quizMDC3) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (fold, joined, looped, prefixed, reset, unshown, with)
import PUI.Web (provided, text, textAt)
import PUI.Web.MDC3 (body, bodyLarge, buttonWith, headlineMedium, headlineSmall, linearProgress, listOfAt)
import QualifiedDo.Semigroupoid as Semigroupoid
import QuizViewModel (answer, askedPrompt, finalScoreLine, freshQuizRun, questionLine, quizPhase, quizProgress)

quizMDC3 :: Effect Unit
quizMDC3 =
  body $ Semigroupoid.do
    prefixed ( RecordToRecord.do
      linearProgress quizProgress
      bodyLarge $ text questionLine ) $ RecordToVariant.do
      ( prefixed (headlineMedium (text askedPrompt)) $ listOfAt @"Question answered" @"key" @"choices" (textAt @"label") ) # provided @"asking"
        @( asking :: { prompt :: String, choices :: Array { key :: Int, label :: String } }
         , finished :: { correct :: Int }
         ) quizPhase # joined @"Question answered"
      ( prefixed (headlineSmall (text finalScoreLine)) $ buttonWith @"Restart" { icon: "replay" } ) # provided @"finished" quizPhase
    VariantToRecord.do
      unshown @"Question answered" # fold answer
      unshown @"Restart" # reset freshQuizRun
  # looped @( question :: Int, correct :: Int ) # with freshQuizRun
