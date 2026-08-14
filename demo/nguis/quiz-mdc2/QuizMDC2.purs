module QuizMDC2 (quizMDC2) where

import Prelude (identity, (#), ($), Unit, const, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (asCase, completed, displayed, forField, mvu, forProperty, projected, toCase, updated)
import PUI.Web.HTML (body, provided, staticText, text)
import PUI.Web.MDC2 (body1, button, card, elevation20, headline5, headline6, linearProgress, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid
import QuizLogic (answer, currentQuestion, finalOutcome, freshQuizRun, progressFraction, questionCountText, questionNumberText)

quizMDC2 :: Effect Unit
quizMDC2 =
  body $
    elevation20 $
      card { caption: "Quiz" } $ ( Semigroupoid.do
          ( RecordToRecord.do
              linearProgress @"value" # projected @"value" progressFraction
              body1 RecordToRecord.do
                staticText "Question "
                text @"value" # projected @"value" questionNumberText
                staticText " of "
                staticText questionCountText
                staticText " · Score "
                text @"value" # forField @"correct" show) # completed
          ( Semigroupoid.do
              headline5 (text @"prompt") # completed
              listOf {} _.choices (text @"value" # forProperty @"value" @"label" identity) # toCase @"picked" _.key) # provided currentQuestion # updated (match { picked: answer })
          ( Semigroupoid.do
              headline6 ( RecordToRecord.do
                  staticText "Final score: "
                  text @"value" # forField @"correct" show
                  staticText " / "
                  text @"value" # forField @"total" show) # displayed
              button { label: "Restart", icon: "replay" } # asCase @"clicked" @"restarted") # provided finalOutcome # updated (match { restarted: const (const freshQuizRun) })
      ) # mvu freshQuizRun
