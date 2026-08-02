module QuizMDC2 (quizMDC2) where

import Prelude ((#), ($), (+), (/), (<), (<#>), (==), Unit, const, min, show)

import Data.Array (index, length, mapWithIndex)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (asCase, completed, displayed, forField, forValue, mvu, forProperty, projected, toCase, updated)
import PUI.HTML (body, provided, staticText, text)
import PUI.MDC2 (body1, button, card, elevation20, headline5, headline6, linearProgress, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid

quizMDC2 :: Effect Unit
quizMDC2 =
  body $
    elevation20 $
      card { caption: "Quiz" } $ ( Semigroupoid.do
          ( RecordToRecord.do
              linearProgress # projected progressFraction
              body1 RecordToRecord.do
                staticText "Question "
                text # projected questionNumberText
                staticText " of "
                staticText questionCountText
                staticText " · Score "
                text # projected show # forField @"correct") # completed
          ( Semigroupoid.do
              headline5 text # forValue # forField @"prompt" # completed
              listOf {} questionChoices (text # forProperty @"label") # toCase @"picked" _.key) # provided currentQuestion # updated (match { picked: answer })
          ( Semigroupoid.do
              headline6 ( RecordToRecord.do
                  staticText "Final score: "
                  text # projected show # forField @"correct"
                  staticText " / "
                  text # projected show # forField @"total") # displayed
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

questionChoices :: { prompt :: String, choices :: Array { key :: Int, label :: String } } -> Array { key :: Int, label :: String }
questionChoices { choices } = choices

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
