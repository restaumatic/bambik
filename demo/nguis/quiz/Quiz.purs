module Quiz (quiz) where

import Prelude ((#), ($), (+), (/), (<), (<>), (==), Unit, min, show)

import Data.Array (index, length, mapWithIndex)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap, rmap)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (completed, forValue, mvu, projection, updates)
import PUI.HTML (attr, body, shownWhen, text)
import PUI.MDC (body1, button, card, elevation20, headline5, headline6, linearProgress, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid

type QuizRun =
  { question :: Int
  , correct :: Int
  }

quiz :: Effect Unit
quiz =
  body $
    elevation20 $
      card { caption: "Quiz" } $ ( Semigroupoid.do
          ( RecordToRecord.do
              linearProgress # projection progressFraction # forValue
              body1 (text # projection standing # forValue)
          ) # completed
          shownWhen inProgress $ Semigroupoid.do
            headline5 (text # projection currentPrompt # forValue) # completed
            attr "style" "border: 1px solid #ccc;"
              ( listOf {} (text # projection _.label # forValue)
              ) # rmap (\e -> .picked e.key :: [ picked :: Int ]) # lcmap choiceEntries # updates (match { picked: answer })
          shownWhen finished $ Semigroupoid.do
            headline6 (text # projection finalScore # forValue) # completed
            button { label: "Restart", icon: "replay" } # updates (match { clicked: \r _ -> restart r })
      ) # mvu freshQuizRun

type Question =
  { prompt :: String
  , choices :: Array String
  , answer :: Int
  }

questionCatalogue :: Array Question
questionCatalogue =
  [ { prompt: "What is the capital of Australia?", choices: [ "Sydney", "Canberra", "Melbourne", "Perth" ], answer: 1 }
  , { prompt: "Which planet is known as the Red Planet?", choices: [ "Venus", "Jupiter", "Mars", "Mercury" ], answer: 2 }
  , { prompt: "Who painted the Mona Lisa?", choices: [ "Leonardo da Vinci", "Michelangelo", "Raphael", "Donatello" ], answer: 0 }
  , { prompt: "What is the largest ocean on Earth?", choices: [ "Atlantic", "Indian", "Arctic", "Pacific" ], answer: 3 }
  , { prompt: "How many continents are there?", choices: [ "five", "six", "seven", "eight" ], answer: 2 }
  ]

freshQuizRun :: QuizRun
freshQuizRun = { question: 0, correct: 0 }

restart :: QuizRun -> QuizRun
restart _ = freshQuizRun

answer :: Int -> QuizRun -> QuizRun
answer choice run = case index questionCatalogue run.question of
  Just q -> { question: run.question + 1, correct: run.correct + if choice == q.answer then 1 else 0 }
  Nothing -> run

inProgress :: QuizRun -> Boolean
inProgress run = run.question < length questionCatalogue

finished :: QuizRun -> Boolean
finished run = length questionCatalogue < run.question + 1

currentPrompt :: QuizRun -> String
currentPrompt run = case index questionCatalogue run.question of
  Just q -> q.prompt
  Nothing -> ""

type Choice = { key :: Int, label :: String }

choiceEntries :: QuizRun -> Array Choice
choiceEntries run = case index questionCatalogue run.question of
  Just q -> mapWithIndex (\i label -> { key: i, label }) q.choices
  Nothing -> []

progressFraction :: QuizRun -> Number
progressFraction run = toNumber run.question / toNumber (length questionCatalogue)

standing :: QuizRun -> String
standing run = "Question " <> show (min (run.question + 1) (length questionCatalogue)) <> " of " <> show (length questionCatalogue) <> " · Score " <> show run.correct

finalScore :: QuizRun -> String
finalScore run = "Final score: " <> show run.correct <> " / " <> show (length questionCatalogue)
