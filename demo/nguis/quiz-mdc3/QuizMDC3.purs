module QuizMDC3 (quizMDC3) where

import Prelude (identity, (#), ($), (+), (/), (<), (<#>), (==), Unit, const, min, show)

import Data.Array (index, length, mapWithIndex)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (asCase, completed, displayed, forField, mvu, forProperty, projected, toCase, updated)
import PUI.Web.HTML (body, provided, staticText, text)
import PUI.Web.MDC3 (bodyLarge, button, card, elevation5, headlineMedium, headlineSmall, linearProgress, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid

quizMDC3 :: Effect Unit
quizMDC3 =
  body $
    elevation5 $
      card { caption: "Quiz" } $ ( Semigroupoid.do
          ( RecordToRecord.do
              linearProgress # projected progressFraction
              bodyLarge RecordToRecord.do
                staticText "Question "
                text # projected questionNumberText
                staticText " of "
                staticText questionCountText
                staticText " · Score "
                text # forField @"correct" show) # completed
          ( Semigroupoid.do
              headlineMedium text # forField @"prompt" identity # completed
              listOf {} _.choices (text # forProperty @"label" identity) # toCase @"picked" _.key) # provided currentQuestion # updated (match { picked: answer })
          ( Semigroupoid.do
              headlineSmall ( RecordToRecord.do
                  staticText "Final score: "
                  text # forField @"correct" show
                  staticText " / "
                  text # forField @"total" show) # displayed
              button { label: "Restart", icon: "replay" } # asCase @"restarted") # provided finalOutcome # updated (match { restarted: const (const freshQuizRun) })
      ) # mvu freshQuizRun

questionCatalogue :: Array { prompt :: String, choices :: Array String, answer :: Int }
questionCatalogue =
  [ { prompt: "What is the capital of Australia?", choices: [ "Sydney", "Canberra", "Melbourne", "Perth" ], answer: 1 }
  , { prompt: "Which planet is known as the Red Planet?", choices: [ "Venus", "Jupiter", "Mars", "Mercury" ], answer: 2 }
  , { prompt: "Who painted the Mona Lisa?", choices: [ "Leonardo da Vinci", "Michelangelo", "Raphael", "Donatello" ], answer: 0 }
  , { prompt: "What is the largest ocean on Earth?", choices: [ "Atlantic", "Indian", "Arctic", "Pacific" ], answer: 3 }
  , { prompt: "How many continents are there?", choices: [ "five", "six", "seven", "eight" ], answer: 2 }
  ]

freshQuizRun :: { question :: Int, correct :: Int }
freshQuizRun = { question: 0, correct: 0 }

answer :: Int -> { question :: Int, correct :: Int } -> { question :: Int, correct :: Int }
answer choice run@{ question, correct } = case index questionCatalogue question of
  Just q -> { question: question + 1, correct: correct + if choice == q.answer then 1 else 0 }
  Nothing -> run

currentQuestion :: { question :: Int } -> Maybe { prompt :: String, choices :: Array { key :: Int, label :: String } }
currentQuestion { question } = index questionCatalogue question <#> \q ->
  { prompt: q.prompt, choices: mapWithIndex (\i label -> { key: i, label }) q.choices }


finalOutcome :: { question :: Int, correct :: Int } -> Maybe { correct :: Int, total :: Int }
finalOutcome { question, correct } =
  if question < length questionCatalogue then Nothing
  else Just { correct, total: length questionCatalogue }

progressFraction :: { question :: Int } -> Number
progressFraction { question } = toNumber question / toNumber (length questionCatalogue)

questionNumberText :: { question :: Int } -> String
questionNumberText { question } = show (min (question + 1) (length questionCatalogue))

questionCountText :: String
questionCountText = show (length questionCatalogue)
