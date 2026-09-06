module QuizMDC3 (quizMDC3) where

import Prelude ((#), ($), Unit, const)

import Data.Variant (match)
import Effect (Effect)
import PUI (mvu, toCase, updated)
import PUI.Web.HTML (shown, provided, text)
import PUI.Web.MDC3 (body, bodyLarge, button, card, headlineMedium, headlineSmall, linearProgress, listOf)
import QualifiedDo.Category as Category
import QuizLogic (answer, askedPrompt, finalScoreLine, freshQuizRun, questionLine, quizPhase, quizProgress)

quizMDC3 :: Effect Unit
quizMDC3 =
  body $
    card $ ( Category.do
      linearProgress @"Progress" quizProgress # shown
      ( bodyLarge $ text questionLine ) # shown
      ( Category.do
        headlineMedium (text askedPrompt) # shown
        listOf {} _.choices (text _.label) # toCase @"picked" _.key ) # provided @"asking" quizPhase # updated (match { picked: answer })
      ( Category.do
        headlineSmall (text finalScoreLine) # shown
        button @"Restart" { icon: "replay" } ) # provided @"finished" quizPhase # updated (match { "Restart": const (const freshQuizRun) })
    ) # mvu freshQuizRun
