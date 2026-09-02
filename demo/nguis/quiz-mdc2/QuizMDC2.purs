module QuizMDC2 (quizMDC2) where

import Prelude ((#), ($), Unit, const)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (forProperty, mvu, settled, toCase, updated)
import PUI.Web.HTML (shown, body, provided, text)
import PUI.Web.MDC2 (body1, button, card, elevation20, headline5, headline6, linearProgress, listOf)
import QualifiedDo.Category as Category
import QuizLogic (answer, freshQuizRun, presentQuiz, quizPhase)

quizMDC2 :: Effect Unit
quizMDC2 =
  body $
    elevation20 $
      card $ ( Category.do
          ( RecordToRecord.do
              linearProgress @"progress"
              body1 (text @"questionLine") ) # shown
          ( Category.do
              headline5 (text @"prompt") # shown
              listOf {} _.choices (text @"label" # forProperty) # toCase @"picked" _.key ) # provided @"asking" quizPhase # updated (match { picked: answer })
          ( Category.do
              headline6 (text @"finalScoreLine") # shown
              button @"Restart" { icon: "replay" } ) # provided @"finished" quizPhase # updated (match { "Restart": const (const freshQuizRun) })
      ) # settled presentQuiz # mvu freshQuizRun
