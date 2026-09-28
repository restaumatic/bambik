module QuizMDC3 (quizMDC3) where

import Prelude ((#), ($), Unit, const)

import Data.Variant (match)
import Effect (Effect)
import PUI (mvu, updated)
import PUI.Web (provided, shown, text)
import PUI.Web.MDC3 (body, bodyLarge, button, headlineMedium, headlineSmall, linearProgress, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid
import QuizLogic (answer, askedPrompt, finalScoreLine, freshQuizRun, questionLine, quizPhase, quizProgress)

quizMDC3 :: Effect Unit
quizMDC3 =
  body $
    ( Semigroupoid.do
      linearProgress @"Progress" quizProgress # shown
      ( bodyLarge $ text questionLine ) # shown
      ( Semigroupoid.do
        headlineMedium (text askedPrompt) # shown
        listOf @"picked" @"key" {} _.choices (text _.label) ) # provided @"asking" quizPhase # updated (match { picked: answer })
      ( Semigroupoid.do
        headlineSmall (text finalScoreLine) # shown
        button @"Restart" { icon: "replay" } ) # provided @"finished" quizPhase # updated (match { "Restart": const (const freshQuizRun) })
    ) # mvu freshQuizRun
