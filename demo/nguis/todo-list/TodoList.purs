module TodoList (Status, Todo, Todos, addTodo, clearCompleted, emptyTodoList, isCompleted, remainingItems, severalLine, soleLine, toggleTodo, visibleEntries) where

import Prelude ((+), (<), (<<<), (<>), (==), const, map, not, show)

import Data.Array (filter, foldl, length, snoc)
import Data.String (trim)
import Data.Variant (match)

type Todos =
  { "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ]
  , "Todo" :: String
  , todos :: Array Todo
  }

type Todo = { id :: Int, status :: Status, title :: String }

type Status = [ active :: {}, completed :: {} ]

emptyTodoList :: Todos
emptyTodoList = { "Todo": "", todos: [], "Visibility": ."All" {} }

addTodo :: Todos -> Todos
addTodo m@{ "Todo": entry, todos } =
  if trim entry == "" then m
  else m { todos = snoc todos { id: nextId todos, title: trim entry, status: .active {} }, "Todo" = "" }

toggleTodo :: { event :: Int, model :: Todos } -> Todos
toggleTodo { event: id, model: m@{ todos } } = m { todos = map (\t -> if t.id == id then t { status = flipped t.status } else t) todos }

clearCompleted :: Todos -> Todos
clearCompleted m@{ todos } = m { todos = filter (isActive <<< _.status) todos }

itemsLeft :: forall r1. { todos :: Array Todo | r1 } -> Int
itemsLeft { todos } = length (filter (isActive <<< _.status) todos)

remainingItems :: Todos -> [ several :: {}, sole :: {} ]
remainingItems { todos } = if itemsLeft { todos } == 1 then .sole {} else .several {}

soleLine :: Todos -> String
soleLine { todos } = show (itemsLeft { todos }) <> " item left"

severalLine :: Todos -> String
severalLine { todos } = show (itemsLeft { todos }) <> " items left"

visibleEntries :: Todos -> Array Todo
visibleEntries { todos, "Visibility": visibility } = filter (matches visibility) todos
  where
  matches v t = match { "All": const true, "Active": const (isActive t.status), "Completed": const (completed t.status) } v

completed :: Status -> Boolean
completed = match { active: const false, completed: const true }

isActive :: Status -> Boolean
isActive = not <<< completed

flipped :: Status -> Status
flipped = match { active: const (.completed {}), completed: const (.active {}) }

isCompleted :: Todo -> Boolean
isCompleted { status } = completed status

nextId :: Array Todo -> Int
nextId = foldl (\n t -> if t.id < n then n else t.id + 1) 0

