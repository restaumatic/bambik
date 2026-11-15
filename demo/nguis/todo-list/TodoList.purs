module TodoList (Event, Todo, Todos, isCompleted, itemsLeftLine, listed, visibleEntries) where

import Prelude ((#), (+), (<), (<>), (==), class Applicative, const, not, show)

import Data.Array (filter, foldl, length, snoc)
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.String (trim)
import Data.Variant (match)
import PUI (PUI, capture, fold, looped, with)
import QualifiedDo.Semigroupoid as Semigroupoid

type Todos =
  { "Todo" :: String
  , "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ]
  , todos :: Array Todo
  }

type Todo = { "Done" :: Boolean, id :: Int, title :: String }

type Event =
  [ "Add" :: Todos
  , "Clear completed" :: Todos
  , "List edited" :: Todos
  , "Todo typed" :: Todos
  ]

listed :: forall m. Applicative m => PUI m Todos Event -> PUI m {} Todos
listed view =
  Semigroupoid.do
    view
    VariantToRecord.do
      capture @"Todo typed"
      fold @"Add" addTodo
      capture @"List edited"
      fold @"Clear completed" clearCompleted
  # looped # with weekendChores

weekendChores :: Todos
weekendChores =
  { "Todo": ""
  , "Visibility": ."All" {}
  , todos:
    [ { id: 0, title: "Water the basil", "Done": true }
    , { id: 1, title: "Descale the kettle", "Done": false }
    , { id: 2, title: "Return the library books", "Done": false }
    ]
  }

addTodo :: Todos -> Todos
addTodo m@{ "Todo": entry, todos } =
  if trim entry == "" then m
  else m { todos = snoc todos { id: nextId todos, title: trim entry, "Done": false }, "Todo" = "" }

clearCompleted :: Todos -> Todos
clearCompleted m@{ todos } = m { todos = filter (not isCompleted) todos }

itemsLeftLine :: Todos -> String
itemsLeftLine { todos } = case length (filter (not isCompleted) todos) of
  1 -> "1 item left"
  n -> show n <> " items left"

visibleEntries :: Todos -> Array Todo
visibleEntries { todos, "Visibility": visibility } = filter (matches visibility) todos
  where
  matches v t = match { "All": const true, "Active": const (not (isCompleted t)), "Completed": const (isCompleted t) } v

isCompleted :: Todo -> Boolean
isCompleted { "Done": done } = done

nextId :: Array Todo -> Int
nextId = foldl (\n t -> if t.id < n then n else t.id + 1) 0
