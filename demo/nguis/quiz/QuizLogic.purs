module QuizLogic (answer, freshQuizRun, presentQuiz, questionCountText, quizPhase) where

import Prelude (show, (+), (/), (==), min)

import Data.Array (index, length, mapWithIndex)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))

freshQuizRun :: { question :: Int, correct :: Int, progress :: Number, questionText :: String, scoreText :: String }
freshQuizRun = presentQuiz { question: 0, correct: 0, progress: 0.0, questionText: "", scoreText: "" }

presentQuiz :: { question :: Int, correct :: Int, progress :: Number, questionText :: String, scoreText :: String } -> { question :: Int, correct :: Int, progress :: Number, questionText :: String, scoreText :: String }
presentQuiz r = r
  { progress = toNumber r.question / toNumber (length questionCatalogue)
  , questionText = show (min (r.question + 1) (length questionCatalogue))
  , scoreText = show r.correct
  }

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
quizPhase :: { question :: Int, correct :: Int } -> [ asking :: { prompt :: String, choices :: Array { key :: Int, label :: String } }, finished :: { correctText :: String, totalText :: String } ]
quizPhase { question, correct } = case index questionCatalogue question of
  Just q -> .asking { prompt: q.prompt, choices: mapWithIndex (\i label -> { key: i, label }) q.choices }
  Nothing -> .finished { correctText: show correct, totalText: show (length questionCatalogue) }

questionCountText :: String
questionCountText = show (length questionCatalogue)
