module QuizMDC3 (quizMDC3) where

import Prelude ((#), ($), Unit, const)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (forProperty, mvu, settled, toCase, updated)
import PUI.Web.HTML (shown, body, provided, text)
import PUI.Web.MDC3 (bodyLarge, button, card, elevation5, headlineMedium, headlineSmall, linearProgress, listOf)
import QualifiedDo.Category as Category
import QuizLogic (answer, freshQuizRun, presentQuiz, quizPhase)

quizMDC3 :: Effect Unit
quizMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          ( RecordToRecord.do
              linearProgress @"progress"
              bodyLarge (text @"questionLine") ) # shown
          ( Category.do
              headlineMedium (text @"prompt") # shown
              listOf {} _.choices (text @"label" # forProperty) # toCase @"picked" _.key ) # provided @"asking" quizPhase # updated (match { picked: answer })
          ( Category.do
              headlineSmall (text @"finalScoreLine") # shown
              button @"Restart" { icon: "replay" } ) # provided @"finished" quizPhase # updated (match { "Restart": const (const freshQuizRun) })
      ) # settled presentQuiz # mvu freshQuizRun
