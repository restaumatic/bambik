module QuizMDC3 (quizMDC3) where

import Prelude (identity, (#), ($), Unit, const, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (projection, mvu, forProperty, projected, toCase, updated)
import PUI.Web.HTML (shown, shownAlways, body, provided, staticText, text)
import PUI.Web.MDC3 (bodyLarge, button, card, elevation5, headlineMedium, headlineSmall, linearProgress, listOf)
import QualifiedDo.Semigroupoid as Pipeline
import QuizLogic (answer, currentQuestion, finalOutcome, freshQuizRun, progressFraction, questionCountText, questionNumberText)

quizMDC3 :: Effect Unit
quizMDC3 =
  body $
    elevation5 $
      card $ ( Pipeline.do
          ( RecordToRecord.do
              linearProgress @"progress" # projected progressFraction
              bodyLarge RecordToRecord.do
                staticText "Question "
                text @"questionNumber" # projected questionNumberText
                staticText " of "
                staticText questionCountText
                staticText " · Score "
                text @"correct" # projection show ) # shownAlways
          ( Pipeline.do
              headlineMedium (shown @"prompt" identity)
              listOf {} _.choices (text @"label" # forProperty) # toCase @"picked" _.key) # provided currentQuestion # updated (match { picked: answer })
          ( Pipeline.do
              ( headlineSmall $ RecordToRecord.do
                  staticText "Final score: "
                  text @"correct" # projection show
                  staticText " / "
                  text @"total" # projection show) # shownAlways
              button @"Restart" { icon: "replay" }) # provided finalOutcome # updated (match { "Restart": const (const freshQuizRun) })
      ) # mvu freshQuizRun
