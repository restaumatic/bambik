module QuizLogic (answer, currentQuestion, finalOutcome, freshQuizRun, progressFraction, questionCountText, questionNumberText) where

import Prelude ((+), (/), (<), (<#>), (==), min, show)

import Data.Array (index, length, mapWithIndex)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))

freshQuizRun :: { question :: Int, correct :: Int }
freshQuizRun = { question: 0, correct: 0 }

questionCatalogue :: Array { prompt :: String, choices :: Array String, answer :: Int }
questionCatalogue =
  [ { prompt: "What is the capital of Australia?", choices: [ "Sydney", "Canberra", "Melbourne", "Perth" ], answer: 1 }
  , { prompt: "Which planet is known as the Red Planet?", choices: [ "Venus", "Jupiter", "Mars", "Mercury" ], answer: 2 }
  , { prompt: "Who painted the Mona Lisa?", choices: [ "Leonardo da Vinci", "Michelangelo", "Raphael", "Donatello" ], answer: 0 }
  , { prompt: "What is the largest ocean on Earth?", choices: [ "Atlantic", "Indian", "Arctic", "Pacific" ], answer: 3 }
  , { prompt: "How many continents are there?", choices: [ "five", "six", "seven", "eight" ], answer: 2 }
  ]

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
