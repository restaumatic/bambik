module QuizMDC2 (quizMDC2) where

import Prelude ((#), ($), Unit, const)

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (fold, joined, looped, with)
import PUI.Web (provided, shown, text)
import PUI.Web.MDC2 (body, body1, button, headline5, headline6, linearProgress, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid
import QuizViewModel (answer, askedPrompt, finalScoreLine, freshQuizRun, questionLine, quizPhase, quizProgress)

quizMDC2 :: Effect Unit
quizMDC2 =
  body $
    ( Semigroupoid.do
      linearProgress @"Progress" quizProgress # shown
      ( body1 $ text questionLine ) # shown
      RecordToVariant.do
        ( Semigroupoid.do
          headline5 (text askedPrompt) # shown
          listOf @"answered" @"key" {} _.choices (text _.label) ) # provided @"asking" @( asking :: { prompt :: String, choices :: Array { key :: Int, label :: String } }, finished :: { correct :: Int } ) quizPhase # joined @"answered"
        ( Semigroupoid.do
          headline6 (text finalScoreLine) # shown
          button @"Restart" { icon: "replay" } ) # provided @"finished" quizPhase
      VariantToRecord.do
        fold @"answered" answer
        fold @"Restart" (const freshQuizRun)
    ) # looped @( question :: Int, correct :: Int ) # with freshQuizRun
