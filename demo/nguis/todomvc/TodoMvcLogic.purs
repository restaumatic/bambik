module TodoMvcLogic (addTodo, clearCompleted, emptyTodoList, remainingItems, toggleTodo, visibleEntries) where

import Prelude ((==), const, not)

import Data.Array (filter, length, mapWithIndex, modifyAt, snoc)
import Data.Maybe (fromMaybe)
import Data.String (trim)
import Data.Variant (match)

emptyTodoList :: { entry :: String, todos :: Array { title :: String, done :: Boolean }, visibility :: [ all :: {}, active :: {}, completed :: {} ] }
emptyTodoList = { entry: "", todos: [], visibility: .all {} }

addTodo :: { entry :: String, todos :: Array { title :: String, done :: Boolean } } -> { entry :: String, todos :: Array { title :: String, done :: Boolean } }
addTodo m@{ entry, todos } =
  if trim entry == "" then m
  else m { todos = snoc todos { title: trim entry, done: false }, entry = "" }

toggleTodo :: Int -> { todos :: Array { title :: String, done :: Boolean } } -> { todos :: Array { title :: String, done :: Boolean } }
toggleTodo i m@{ todos } = m { todos = fromMaybe todos (modifyAt i (\t -> t { done = not t.done }) todos) }

clearCompleted :: { todos :: Array { title :: String, done :: Boolean } } -> { todos :: Array { title :: String, done :: Boolean } }
clearCompleted m@{ todos } = m { todos = filter (\t -> not t.done) todos }

itemsLeft :: { todos :: Array { title :: String, done :: Boolean } } -> Int
itemsLeft { todos } = length (filter (\t -> not t.done) todos)

remainingItems :: { todos :: Array { title :: String, done :: Boolean } } -> [ sole :: { count :: Int }, several :: { count :: Int } ]
remainingItems { todos } =
  let count = itemsLeft { todos }
  in if count == 1 then .sole { count } else .several { count }

visibleEntries :: { todos :: Array { title :: String, done :: Boolean }, visibility :: [ all :: {}, active :: {}, completed :: {} ] } -> Array { key :: Int, title :: String, done :: Boolean }
visibleEntries { todos, visibility } = filter (matches visibility) (mapWithIndex (\i t -> { key: i, title: t.title, done: t.done }) todos)
  where
  matches v t = match { all: const true, active: \_ -> not t.done, completed: \_ -> t.done } v
