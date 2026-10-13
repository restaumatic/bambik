module QuizMDC2 (quizMDC2) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (fold, joined, looped, prefixed, reset, unshown, with)
import PUI.Web (provided, text, textAt)
import PUI.Web.MDC2 (body, body1, buttonWith, headline5, headline6, linearProgress, listOfAt)
import QualifiedDo.Semigroupoid as Semigroupoid
import QuizViewModel (answer, askedPrompt, finalScoreLine, freshQuizRun, questionLine, quizPhase, quizProgress)

quizMDC2 :: Effect Unit
quizMDC2 =
  body $ Semigroupoid.do
    prefixed ( RecordToRecord.do
      linearProgress quizProgress
      body1 $ text questionLine ) $ RecordToVariant.do
      ( prefixed (headline5 (text askedPrompt)) $ listOfAt @"Question answered" @"key" @"choices" (textAt @"label") ) # provided @"asking"
        @( asking :: { prompt :: String, choices :: Array { key :: Int, label :: String } }
         , finished :: { correct :: Int }
         ) quizPhase # joined @"Question answered"
      ( prefixed (headline6 (text finalScoreLine)) $ buttonWith @"Restart" { icon: "replay" } ) # provided @"finished" quizPhase
    VariantToRecord.do
      unshown @"Question answered" # fold answer
      unshown @"Restart" # reset freshQuizRun
  # looped @( question :: Int, correct :: Int ) # with freshQuizRun
