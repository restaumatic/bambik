module TodoListViewModel (addTodo, clearCompleted, completedClearedLine, emptyTodoList, isCompleted, remainingItems, severalLine, soleLine, todoAddedLine, todoToggledLine, toggleTodo, visibleEntries) where

import Prelude ((<<<), (<>), (==), const, not, show)

import Data.Array (filter, index, length, mapWithIndex, modifyAt, snoc)
import Data.Maybe (fromMaybe, maybe)
import Data.String (trim)
import Data.Variant (match)

emptyTodoList :: { "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ], "What needs to be done?" :: String, todos :: Array { status :: [ active :: {}, completed :: {} ], title :: String } }
emptyTodoList = { "What needs to be done?": "", todos: [], "Visibility": ."All" {} }

addTodo :: { "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ], "What needs to be done?" :: String, todos :: Array { status :: [ active :: {}, completed :: {} ], title :: String } } -> { "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ], "What needs to be done?" :: String, todos :: Array { status :: [ active :: {}, completed :: {} ], title :: String } }
addTodo m@{ "What needs to be done?": entry, todos } =
  if trim entry == "" then m
  else m { todos = snoc todos { title: trim entry, status: .active {} }, "What needs to be done?" = "" }

toggleTodo :: { event :: Int, model :: { "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ], "What needs to be done?" :: String, todos :: Array { status :: [ active :: {}, completed :: {} ], title :: String } } } -> { "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ], "What needs to be done?" :: String, todos :: Array { status :: [ active :: {}, completed :: {} ], title :: String } }
toggleTodo { event: i, model: m@{ todos } } = m { todos = fromMaybe todos (modifyAt i (\t -> t { status = flipped t.status }) todos) }

clearCompleted :: { "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ], "What needs to be done?" :: String, todos :: Array { status :: [ active :: {}, completed :: {} ], title :: String } } -> { "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ], "What needs to be done?" :: String, todos :: Array { status :: [ active :: {}, completed :: {} ], title :: String } }
clearCompleted m@{ todos } = m { todos = filter (isActive <<< _.status) todos }

todoAddedLine :: { "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ], "What needs to be done?" :: String, todos :: Array { status :: [ active :: {}, completed :: {} ], title :: String } } -> String
todoAddedLine { "What needs to be done?": entry } = "Added " <> trim entry

todoToggledLine :: { event :: Int, model :: { "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ], "What needs to be done?" :: String, todos :: Array { status :: [ active :: {}, completed :: {} ], title :: String } } } -> String
todoToggledLine { event: i, model: { todos } } = "Toggled " <> maybe "nothing" _.title (index todos i)

completedClearedLine :: { "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ], "What needs to be done?" :: String, todos :: Array { status :: [ active :: {}, completed :: {} ], title :: String } } -> String
completedClearedLine _ = "Cleared completed todos"

itemsLeft :: forall r1. { todos :: Array { title :: String, status :: [ active :: {}, completed :: {} ] } | r1 } -> Int
itemsLeft { todos } = length (filter (isActive <<< _.status) todos)

remainingItems :: { "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ], "What needs to be done?" :: String, todos :: Array { status :: [ active :: {}, completed :: {} ], title :: String } } -> [ several :: { count :: Int }, sole :: { count :: Int } ]
remainingItems { todos } =
  let count = itemsLeft { todos }
  in if count == 1 then .sole { count } else .several { count }

soleLine :: { count :: Int } -> String
soleLine { count } = show count <> " item left"

severalLine :: { count :: Int } -> String
severalLine { count } = show count <> " items left"

visibleEntries :: { "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ], "What needs to be done?" :: String, todos :: Array { status :: [ active :: {}, completed :: {} ], title :: String } } -> Array { key :: Int, status :: [ active :: {}, completed :: {} ], title :: String }
visibleEntries { todos, "Visibility": visibility } = filter (matches visibility) (mapWithIndex (\i t -> { key: i, title: t.title, status: t.status }) todos)
  where
  matches v t = match { "All": const true, "Active": const (isActive t.status), "Completed": const (completed t.status) } v

completed :: [ active :: {}, completed :: {} ] -> Boolean
completed = match { active: const false, completed: const true }

isActive :: [ active :: {}, completed :: {} ] -> Boolean
isActive = not <<< completed

flipped :: [ active :: {}, completed :: {} ] -> [ active :: {}, completed :: {} ]
flipped = match { active: const (.completed {}), completed: const (.active {}) }

isCompleted :: { key :: Int, status :: [ active :: {}, completed :: {} ], title :: String } -> Boolean
isCompleted { status } = completed status

