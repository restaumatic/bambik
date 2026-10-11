module QuizMDC2 (quizMDC2) where

import Prelude ((#), ($), Unit, const)

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (blankStatus, fold, joined, looped, with)
import PUI.Pipeline as Pipeline
import PUI.Web (provided, text, textAt)
import PUI.Web.MDC2 (body, body1, button, headline5, headline6, linearProgress, listOfAt)
import QuizViewModel (answer, askedPrompt, finalScoreLine, freshQuizRun, questionLine, quizPhase, quizProgress)

quizMDC2 :: Effect Unit
quizMDC2 =
  body $ Pipeline.do
    linearProgress quizProgress
    body1 $ text questionLine
    RecordToVariant.do
      ( Pipeline.do
        headline5 (text askedPrompt)
        listOfAt @"Question answered" @"key" @"choices" {} (textAt @"label") ) # provided @"asking"
          @( asking :: { prompt :: String, choices :: Array { key :: Int, label :: String } }
           , finished :: { correct :: Int }
           ) quizPhase # joined @"Question answered"
      ( Pipeline.do
        headline6 (text finalScoreLine)
        button @"Restart" { icon: "replay" } ) # provided @"finished" quizPhase
    VariantToRecord.do
      blankStatus @"Question answered" # fold answer
      blankStatus @"Restart" # fold (const freshQuizRun)
  # looped @( question :: Int, correct :: Int ) # with freshQuizRun
