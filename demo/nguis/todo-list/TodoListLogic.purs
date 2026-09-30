module TodoListLogic (addTodo, clearCompleted, emptyTodoList, isCompleted, remainingItems, severalLine, soleLine, toggleTodo, visibleEntries) where

import Prelude ((<<<), (<>), (==), const, not, show)

import Data.Array (filter, length, mapWithIndex, modifyAt, snoc)
import Data.Maybe (fromMaybe)
import Data.String (trim)
import Data.Variant (match)

emptyTodoList :: { "What needs to be done?" :: String, todos :: Array { title :: String, status :: [ active :: {}, completed :: {} ] }, "Visibility" :: [ "All" :: {}, "Active" :: {}, "Completed" :: {} ] }
emptyTodoList = { "What needs to be done?": "", todos: [], "Visibility": ."All" {} }

addTodo :: forall r1. { "What needs to be done?" :: String, todos :: Array { title :: String, status :: [ active :: {}, completed :: {} ] } | r1 } -> { "What needs to be done?" :: String, todos :: Array { title :: String, status :: [ active :: {}, completed :: {} ] } | r1 }
addTodo m@{ "What needs to be done?": entry, todos } =
  if trim entry == "" then m
  else m { todos = snoc todos { title: trim entry, status: .active {} }, "What needs to be done?" = "" }

toggleTodo :: forall r1. Int -> { todos :: Array { title :: String, status :: [ active :: {}, completed :: {} ] } | r1 } -> { todos :: Array { title :: String, status :: [ active :: {}, completed :: {} ] } | r1 }
toggleTodo i m@{ todos } = m { todos = fromMaybe todos (modifyAt i (\t -> t { status = flipped t.status }) todos) }

clearCompleted :: forall r1. { todos :: Array { title :: String, status :: [ active :: {}, completed :: {} ] } | r1 } -> { todos :: Array { title :: String, status :: [ active :: {}, completed :: {} ] } | r1 }
clearCompleted m@{ todos } = m { todos = filter (isActive <<< _.status) todos }

itemsLeft :: forall r1. { todos :: Array { title :: String, status :: [ active :: {}, completed :: {} ] } | r1 } -> Int
itemsLeft { todos } = length (filter (isActive <<< _.status) todos)

remainingItems :: forall r1. { todos :: Array { title :: String, status :: [ active :: {}, completed :: {} ] } | r1 } -> [ sole :: { count :: Int }, several :: { count :: Int } ]
remainingItems { todos } =
  let count = itemsLeft { todos }
  in if count == 1 then .sole { count } else .several { count }

soleLine :: forall r1. { count :: Int | r1 } -> String
soleLine { count } = show count <> " item left"

severalLine :: forall r1. { count :: Int | r1 } -> String
severalLine { count } = show count <> " items left"

visibleEntries :: forall r1. { todos :: Array { title :: String, status :: [ active :: {}, completed :: {} ] }, "Visibility" :: [ "All" :: {}, "Active" :: {}, "Completed" :: {} ] | r1 } -> Array { key :: Int, title :: String, status :: [ active :: {}, completed :: {} ] }
visibleEntries { todos, "Visibility": visibility } = filter (matches visibility) (mapWithIndex (\i t -> { key: i, title: t.title, status: t.status }) todos)
  where
  matches v t = match { "All": const true, "Active": const (isActive t.status), "Completed": const (completed t.status) } v

completed :: [ active :: {}, completed :: {} ] -> Boolean
completed = match { active: const false, completed: const true }

isActive :: [ active :: {}, completed :: {} ] -> Boolean
isActive = not <<< completed

flipped :: [ active :: {}, completed :: {} ] -> [ active :: {}, completed :: {} ]
flipped = match { active: const (.completed {}), completed: const (.active {}) }

isCompleted :: forall r1. { key :: Int, title :: String, status :: [ active :: {}, completed :: {} ] | r1 } -> Boolean
isCompleted { status } = completed status

