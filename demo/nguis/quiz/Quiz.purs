module Quiz (QuizRun, answer, askedPrompt, choices, finalScoreLine, freshQuizRun, questionLine, quizPhase, quizProgress) where

import Prelude (show, (+), (/), (<>), (==))

import Data.Array (index, length, mapWithIndex)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)

type QuizRun = { correct :: Int, question :: Int }

freshQuizRun :: QuizRun
freshQuizRun = { question: 0, correct: 0 }

quizProgress :: QuizRun -> Number
quizProgress { question } = toNumber question / toNumber (length questionCatalogue)

questionLine :: QuizRun -> String
questionLine { question, correct } = "Question " <> show (question + 1) <> " of " <> show (length questionCatalogue) <> " · Score " <> show correct

questionCatalogue :: Array { prompt :: String, choices :: Array String, answer :: Int }
questionCatalogue =
  [ { prompt: "What is the capital of Australia?", choices: [ "Sydney", "Canberra", "Melbourne", "Perth" ], answer: 1 }
  , { prompt: "Which planet is known as the Red Planet?", choices: [ "Venus", "Jupiter", "Mars", "Mercury" ], answer: 2 }
  , { prompt: "Who painted the Mona Lisa?", choices: [ "Leonardo da Vinci", "Michelangelo", "Raphael", "Donatello" ], answer: 0 }
  , { prompt: "What is the largest ocean on Earth?", choices: [ "Atlantic", "Indian", "Arctic", "Pacific" ], answer: 3 }
  , { prompt: "How many continents are there?", choices: [ "five", "six", "seven", "eight" ], answer: 2 }
  ]

answer :: { event :: Int, model :: QuizRun } -> QuizRun
answer { event: choice, model: run@{ question, correct } } = case index questionCatalogue question of
  Just q -> run { question = question + 1, correct = correct + if choice == q.answer then 1 else 0 }
  Nothing -> run

quizPhase :: QuizRun -> [ asking :: {}, finished :: {} ]
quizPhase { question } = case index questionCatalogue question of
  Just _ -> .asking {}
  Nothing -> .finished {}

askedPrompt :: QuizRun -> String
askedPrompt { question } = maybe "" _.prompt (index questionCatalogue question)

choices :: QuizRun -> Array { choice :: String, key :: Int }
choices { question } = maybe [] (\q -> mapWithIndex (\i choice -> { key: i, choice }) q.choices) (index questionCatalogue question)

finalScoreLine :: QuizRun -> String
finalScoreLine { correct } = "Final score: " <> show correct <> " / " <> show (length questionCatalogue)
