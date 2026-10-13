module Quiz (answer, askedPrompt, choices, finalScoreLine, freshQuizRun, questionLine, quizProgress, quizPhase) where

import Prelude (show, (+), (/), (<>), (==), min)

import Data.Array (index, length, mapWithIndex)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)

freshQuizRun :: { correct :: Int, question :: Int }
freshQuizRun = { question: 0, correct: 0 }

quizProgress :: { correct :: Int, question :: Int } -> Number
quizProgress { question } = toNumber question / toNumber (length questionCatalogue)

questionLine :: { correct :: Int, question :: Int } -> String
questionLine { question, correct } = "Question " <> show (min (question + 1) (length questionCatalogue)) <> " of " <> show (length questionCatalogue) <> " · Score " <> show correct

questionCatalogue :: Array { prompt :: String, choices :: Array String, answer :: Int }
questionCatalogue =
  [ { prompt: "What is the capital of Australia?", choices: [ "Sydney", "Canberra", "Melbourne", "Perth" ], answer: 1 }
  , { prompt: "Which planet is known as the Red Planet?", choices: [ "Venus", "Jupiter", "Mars", "Mercury" ], answer: 2 }
  , { prompt: "Who painted the Mona Lisa?", choices: [ "Leonardo da Vinci", "Michelangelo", "Raphael", "Donatello" ], answer: 0 }
  , { prompt: "What is the largest ocean on Earth?", choices: [ "Atlantic", "Indian", "Arctic", "Pacific" ], answer: 3 }
  , { prompt: "How many continents are there?", choices: [ "five", "six", "seven", "eight" ], answer: 2 }
  ]

answer
  :: { event :: Int, model :: { correct :: Int, question :: Int } }
  -> { correct :: Int, question :: Int }
answer { event: choice, model: run@{ question, correct } } = case index questionCatalogue question of
  Just q -> run { question = question + 1, correct = correct + if choice == q.answer then 1 else 0 }
  Nothing -> run

quizPhase :: { correct :: Int, question :: Int } -> [ asking :: {}, finished :: {} ]
quizPhase { question } = case index questionCatalogue question of
  Just _ -> .asking {}
  Nothing -> .finished {}

askedPrompt :: { correct :: Int, question :: Int } -> String
askedPrompt { question } = maybe "" _.prompt (index questionCatalogue question)

choices :: { correct :: Int, question :: Int } -> Array { key :: Int, label :: String }
choices { question } = maybe [] (\q -> mapWithIndex (\i label -> { key: i, label }) q.choices) (index questionCatalogue question)

finalScoreLine :: { correct :: Int, question :: Int } -> String
finalScoreLine { correct } = "Final score: " <> show correct <> " / " <> show (length questionCatalogue)
