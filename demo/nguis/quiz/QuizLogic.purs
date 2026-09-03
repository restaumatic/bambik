module QuizLogic (answer, askedChoices, askedPrompt, finalScoreLine, freshQuizRun, questionLine, quizProgress, quizPhase) where

import Prelude (show, (+), (/), (<>), (==), min)

import Data.Array (index, length, mapWithIndex)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))

freshQuizRun :: { question :: Int, correct :: Int }
freshQuizRun = { question: 0, correct: 0 }

quizProgress :: { question :: Int, correct :: Int } -> Number
quizProgress { question } = toNumber question / toNumber (length questionCatalogue)

questionLine :: { question :: Int, correct :: Int } -> String
questionLine { question, correct } = "Question " <> show (min (question + 1) (length questionCatalogue)) <> " of " <> show (length questionCatalogue) <> " · Score " <> show correct

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

-- the run is asking while the catalogue has a question left, finished after
quizPhase :: { question :: Int, correct :: Int } -> [ asking :: { prompt :: String, choices :: Array { key :: Int, label :: String } }, finished :: { correct :: Int } ]
quizPhase { question, correct } = case index questionCatalogue question of
  Just q -> .asking { prompt: q.prompt, choices: mapWithIndex (\i label -> { key: i, label }) q.choices }
  Nothing -> .finished { correct }

askedPrompt :: { prompt :: String, choices :: Array { key :: Int, label :: String } } -> String
askedPrompt { prompt } = prompt

askedChoices :: { prompt :: String, choices :: Array { key :: Int, label :: String } } -> Array { key :: Int, label :: String }
askedChoices { choices } = choices

finalScoreLine :: { correct :: Int } -> String
finalScoreLine { correct } = "Final score: " <> show correct <> " / " <> show (length questionCatalogue)
