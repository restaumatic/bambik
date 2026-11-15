module TodoList (Todo, Todos, addTodo, clearCompleted, isCompleted, itemsLeftLine, visibleEntries, weekendChores) where

import Prelude ((+), (<), (<>), (==), const, not, show)

import Data.Array (filter, foldl, length, snoc)
import Data.String (trim)
import Data.Variant (match)

type Todos =
  { "Todo" :: String
  , "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ]
  , todos :: Array Todo
  }

type Todo = { "Done" :: Boolean, id :: Int, title :: String }

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
