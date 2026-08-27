module QuizMDC2 (quizMDC2) where

import Prelude ((#), ($), Unit, const, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (forProperty, mvu, projection, toCase, updated)
import PUI.Web.HTML (shown, body, provided, staticText, text)
import PUI.Web.MDC2 (body1, button, card, elevation20, headline5, headline6, linearProgress, listOf)
import QualifiedDo.Category as Category
import QuizLogic (answer, freshQuizRun, progressFraction, questionCountText, questionNumberText, quizPhase)

quizMDC2 :: Effect Unit
quizMDC2 =
  body $
    elevation20 $
      card $ ( Category.do
          ( RecordToRecord.do
              linearProgress @"question" # projection progressFraction
              body1 RecordToRecord.do
                staticText "Question "
                text @"question" # projection questionNumberText
                staticText " of "
                staticText questionCountText
                staticText " · Score "
                text @"correct" # projection show ) # shown
          ( Category.do
              headline5 (text @"prompt") # shown
              listOf {} _.choices (text @"label" # forProperty) # toCase @"picked" _.key ) # provided @"asking" quizPhase # updated (match { picked: answer })
          ( Category.do
              ( headline6 $ RecordToRecord.do
                  staticText "Final score: "
                  text @"correct" # projection show
                  staticText " / "
                  text @"total" # projection show ) # shown
              button @"Restart" { icon: "replay" } ) # provided @"finished" quizPhase # updated (match { "Restart": const (const freshQuizRun) })
      ) # mvu freshQuizRun
