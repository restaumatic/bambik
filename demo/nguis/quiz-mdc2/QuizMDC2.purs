module QuizMDC2 (quizMDC2) where

import Prelude ((#), ($), Unit, const)

import Data.Variant (match)
import Effect (Effect)
import PUI (mvu, toCase, updated)
import PUI.Web.HTML (shown, body, provided, text)
import PUI.Web.MDC2 (body1, button, card, elevation20, headline5, headline6, linearProgress, listOf)
import QualifiedDo.Category as Category
import QuizLogic (answer, askedChoices, askedPrompt, finalScoreLine, freshQuizRun, questionLine, quizPhase, quizProgress)

quizMDC2 :: Effect Unit
quizMDC2 =
  body $
    elevation20 $
      card $ ( Category.do
          linearProgress @"Progress" quizProgress # shown
          ( body1 $ text questionLine ) # shown
          ( Category.do
              headline5 (text askedPrompt) # shown
              listOf {} askedChoices (text _.label) # toCase @"picked" _.key ) # provided @"asking" quizPhase # updated (match { picked: answer })
          ( Category.do
              headline6 (text finalScoreLine) # shown
              button @"Restart" { icon: "replay" } ) # provided @"finished" quizPhase # updated (match { "Restart": const (const freshQuizRun) })
      ) # mvu freshQuizRun
