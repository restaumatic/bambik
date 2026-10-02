module QuizMDC2 (quizMDC2) where

import Prelude ((#), ($), Unit, const)

import Data.Variant (match)
import Effect (Effect)
import PUI (mvu, state, updated)
import PUI.Web (provided, shown, text)
import PUI.Web.MDC2 (body, body1, button, headline5, headline6, linearProgress, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid
import QuizViewModel (answer, askedPrompt, finalScoreLine, freshQuizRun, questionLine, quizPhase, quizProgress)

quizMDC2 :: Effect Unit
quizMDC2 =
  body $
    ( Semigroupoid.do
      state @"question" @Int
      state @"correct" @Int
      linearProgress @"Progress" quizProgress # shown
      ( body1 $ text questionLine ) # shown
      ( Semigroupoid.do
        state @"prompt" @String
        headline5 (text askedPrompt) # shown
        listOf @"answered" @"key" @Int {} _.choices (text _.label) ) # provided @"asking" quizPhase # updated (match { answered: answer })
      ( Semigroupoid.do
        state @"correct" @Int
        headline6 (text finalScoreLine) # shown
        button @"Restart" { icon: "replay" } ) # provided @"finished" quizPhase # updated (match { "Restart": const (const freshQuizRun) })
    ) # mvu freshQuizRun
