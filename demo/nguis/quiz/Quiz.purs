module Quiz (quiz) where

import Prelude ((#), ($), (+), (/), (<), (<#>), (<>), (==), Unit, const, min, show)

import Data.Array (index, length, mapWithIndex)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap, rmap)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (asCase, completed, displayed, forField, forValue, mvu, projection, toCase, updates)
import PUI.HTML (body, provided, text)
import PUI.MDC (body1, button, card, elevation20, headline5, headline6, linearProgress, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid

quiz :: Effect Unit
quiz =
  body $
    elevation20 $
      card { caption: "Quiz" } $ ( Semigroupoid.do
          ( RecordToRecord.do
              linearProgress # projection progressFraction
              body1 text # projection standing) # completed
          ( Semigroupoid.do
              headline5 text # projection questionPrompt # completed
              listOf {} (text # projection _.label) # rmap _.key # toCase @"picked" # lcmap questionChoices) # provided # lcmap currentQuestion # updates (match { picked: answer })
          ( Semigroupoid.do
              headline6 text # forValue # forField @"summary" # displayed
              button { label: "Restart", icon: "replay" } # asCase @"restarted") # provided # lcmap finalOutcome # updates (match { restarted: const restart })
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

restart :: forall a. a -> { question :: Int, correct :: Int }
restart _ = freshQuizRun

answer :: Int -> { question :: Int, correct :: Int } -> { question :: Int, correct :: Int }
answer choice run = case index questionCatalogue run.question of
  Just q -> { question: run.question + 1, correct: run.correct + if choice == q.answer then 1 else 0 }
  Nothing -> run

currentQuestion :: forall r. { question :: Int | r } -> Maybe { prompt :: String, choices :: Array { key :: Int, label :: String } }
currentQuestion run = index questionCatalogue run.question <#> \q ->
  { prompt: q.prompt, choices: mapWithIndex (\i label -> { key: i, label }) q.choices }

questionPrompt :: { prompt :: String, choices :: Array { key :: Int, label :: String } } -> String
questionPrompt q = q.prompt

questionChoices :: forall r. { choices :: Array { key :: Int, label :: String } | r } -> Array { key :: Int, label :: String }
questionChoices q = q.choices

finalOutcome :: { question :: Int, correct :: Int } -> Maybe { summary :: String }
finalOutcome run =
  if run.question < length questionCatalogue then Nothing
  else Just { summary: "Final score: " <> show run.correct <> " / " <> show (length questionCatalogue) }

progressFraction :: { question :: Int, correct :: Int } -> Number
progressFraction run = toNumber run.question / toNumber (length questionCatalogue)

standing :: { question :: Int, correct :: Int } -> String
standing run = "Question " <> show (min (run.question + 1) (length questionCatalogue)) <> " of " <> show (length questionCatalogue) <> " · Score " <> show run.correct
