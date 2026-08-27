module TodoMvcLogic (addTodo, clearCompleted, emptyTodoList, isCompleted, remainingItems, titleText, toggleTodo, visibleEntries) where

import Prelude ((==), const, not)

import Data.Array (filter, length, mapWithIndex, modifyAt, snoc)
import Data.Maybe (fromMaybe)
import Data.String (trim)
import Data.Variant (match)

emptyTodoList :: { "What needs to be done?" :: String, todos :: Array { title :: String, status :: [ active :: {}, completed :: {} ] }, "Visibility" :: [ "All" :: {}, "Active" :: {}, "Completed" :: {} ] }
emptyTodoList = { "What needs to be done?": "", todos: [], "Visibility": ."All" {} }

addTodo :: { "What needs to be done?" :: String, todos :: Array { title :: String, status :: [ active :: {}, completed :: {} ] } } -> { "What needs to be done?" :: String, todos :: Array { title :: String, status :: [ active :: {}, completed :: {} ] } }
addTodo m@{ "What needs to be done?": entry, todos } =
  if trim entry == "" then m
  else m { todos = snoc todos { title: trim entry, status: .active {} }, "What needs to be done?" = "" }

toggleTodo :: Int -> { todos :: Array { title :: String, status :: [ active :: {}, completed :: {} ] } } -> { todos :: Array { title :: String, status :: [ active :: {}, completed :: {} ] } }
toggleTodo i m@{ todos } = m { todos = fromMaybe todos (modifyAt i (\t -> t { status = flipped t.status }) todos) }

clearCompleted :: { todos :: Array { title :: String, status :: [ active :: {}, completed :: {} ] } } -> { todos :: Array { title :: String, status :: [ active :: {}, completed :: {} ] } }
clearCompleted m@{ todos } = m { todos = filter (\t -> not (completed t.status)) todos }

itemsLeft :: { todos :: Array { title :: String, status :: [ active :: {}, completed :: {} ] } } -> Int
itemsLeft { todos } = length (filter (\t -> not (completed t.status)) todos)

remainingItems :: { todos :: Array { title :: String, status :: [ active :: {}, completed :: {} ] } } -> [ sole :: { count :: Int }, several :: { count :: Int } ]
remainingItems { todos } =
  let count = itemsLeft { todos }
  in if count == 1 then .sole { count } else .several { count }

visibleEntries :: { todos :: Array { title :: String, status :: [ active :: {}, completed :: {} ] }, "Visibility" :: [ "All" :: {}, "Active" :: {}, "Completed" :: {} ] } -> Array { key :: Int, title :: String, status :: [ active :: {}, completed :: {} ] }
visibleEntries { todos, "Visibility": visibility } = filter (matches visibility) (mapWithIndex (\i t -> { key: i, title: t.title, status: t.status }) todos)
  where
  matches v t = match { "All": const true, "Active": \_ -> not (completed t.status), "Completed": \_ -> completed t.status } v

completed :: [ active :: {}, completed :: {} ] -> Boolean
completed = match { active: \_ -> false, completed: \_ -> true }

flipped :: [ active :: {}, completed :: {} ] -> [ active :: {}, completed :: {} ]
flipped = match { active: \_ -> .completed {}, completed: \_ -> .active {} }

isCompleted :: { key :: Int, title :: String, status :: [ active :: {}, completed :: {} ] } -> Boolean
isCompleted { status } = completed status

titleText :: { key :: Int, title :: String, status :: [ active :: {}, completed :: {} ] } -> String
titleText { title } = title
