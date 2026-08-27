module QuizMDC3 (quizMDC3) where

import Prelude ((#), ($), Unit, const, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (projection, mvu, forProperty, projected, toCase, updated)
import PUI.Web.HTML (shown, body, provided, staticText, text)
import PUI.Web.MDC3 (bodyLarge, button, card, elevation5, headlineMedium, headlineSmall, linearProgress, listOf)
import QualifiedDo.Category as Category
import QuizLogic (answer, freshQuizRun, progressFraction, questionCountText, questionNumberText, quizPhase)

quizMDC3 :: Effect Unit
quizMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          ( RecordToRecord.do
              linearProgress @"progress" # projected progressFraction
              bodyLarge RecordToRecord.do
                staticText "Question "
                text @"questionNumber" # projected questionNumberText
                staticText " of "
                staticText questionCountText
                staticText " · Score "
                text @"correct" # projection show ) # shown
          ( Category.do
              headlineMedium (text @"prompt") # shown
              listOf {} _.choices (text @"label" # forProperty) # toCase @"picked" _.key ) # provided @"asking" quizPhase # updated (match { picked: answer })
          ( Category.do
              ( headlineSmall $ RecordToRecord.do
                  staticText "Final score: "
                  text @"correct" # projection show
                  staticText " / "
                  text @"total" # projection show ) # shown
              button @"Restart" { icon: "replay" } ) # provided @"finished" quizPhase # updated (match { "Restart": const (const freshQuizRun) })
      ) # mvu freshQuizRun
