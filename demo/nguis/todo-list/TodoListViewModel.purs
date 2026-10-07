module TodoListViewModel (addTodo, clearCompleted, completedClearedLine, emptyTodoList, isCompleted, remainingItems, severalLine, soleLine, todoAddedLine, todoToggledLine, toggleTodo, visibleEntries) where

import Prelude ((<<<), (<>), (==), const, not, show)

import Data.Array (filter, last, length, mapWithIndex, modifyAt, snoc)
import Data.Maybe (fromMaybe, maybe)
import Data.String (trim)
import Data.Variant (match)

emptyTodoList
  :: { "New todo" :: String
     , "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ]
     , todos :: Array { status :: [ active :: {}, completed :: {} ], title :: String }
     }
emptyTodoList = { "New todo": "", todos: [], "Visibility": ."All" {} }

addTodo
  :: { "New todo" :: String
     , "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ]
     , todos :: Array { status :: [ active :: {}, completed :: {} ], title :: String }
     }
  -> { "New todo" :: String
     , "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ]
     , todos :: Array { status :: [ active :: {}, completed :: {} ], title :: String }
     }
addTodo m@{ "New todo": entry, todos } =
  if trim entry == "" then m
  else m { todos = snoc todos { title: trim entry, status: .active {} }, "New todo" = "" }

toggleTodo
  :: { event :: Int
     , model :: { "New todo" :: String
                , "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ]
                , todos :: Array { status :: [ active :: {}, completed :: {} ], title :: String }
                }
     }
  -> { "New todo" :: String
     , "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ]
     , todos :: Array { status :: [ active :: {}, completed :: {} ], title :: String }
     }
toggleTodo { event: i, model: m@{ todos } } = m { todos = fromMaybe todos (modifyAt i (\t -> t { status = flipped t.status }) todos) }

clearCompleted
  :: { "New todo" :: String
     , "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ]
     , todos :: Array { status :: [ active :: {}, completed :: {} ], title :: String }
     }
  -> { "New todo" :: String
     , "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ]
     , todos :: Array { status :: [ active :: {}, completed :: {} ], title :: String }
     }
clearCompleted m@{ todos } = m { todos = filter (isActive <<< _.status) todos }

todoAddedLine
  :: { "New todo" :: String
     , "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ]
     , todos :: Array { status :: [ active :: {}, completed :: {} ], title :: String }
     }
  -> String
todoAddedLine { todos } = maybe "No todos yet" (\t -> "Last added: " <> t.title) (last todos)

todoToggledLine
  :: { "New todo" :: String
     , "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ]
     , todos :: Array { status :: [ active :: {}, completed :: {} ], title :: String }
     }
  -> String
todoToggledLine m = show (itemsLeft m) <> " left to do"

completedClearedLine
  :: { "New todo" :: String
     , "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ]
     , todos :: Array { status :: [ active :: {}, completed :: {} ], title :: String }
     }
  -> String
completedClearedLine { todos } = show (length todos) <> " todos left, none completed"

itemsLeft :: forall r1. { todos :: Array { title :: String, status :: [ active :: {}, completed :: {} ] } | r1 } -> Int
itemsLeft { todos } = length (filter (isActive <<< _.status) todos)

remainingItems
  :: { "New todo" :: String
     , "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ]
     , todos :: Array { status :: [ active :: {}, completed :: {} ], title :: String }
     }
  -> [ several :: { count :: Int }, sole :: { count :: Int } ]
remainingItems { todos } =
  let count = itemsLeft { todos }
  in if count == 1 then .sole { count } else .several { count }

soleLine :: { count :: Int } -> String
soleLine { count } = show count <> " item left"

severalLine :: { count :: Int } -> String
severalLine { count } = show count <> " items left"

visibleEntries
  :: { "New todo" :: String
     , "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ]
     , todos :: Array { status :: [ active :: {}, completed :: {} ], title :: String }
     }
  -> Array { key :: Int, status :: [ active :: {}, completed :: {} ], title :: String }
visibleEntries { todos, "Visibility": visibility } = filter (matches visibility) (mapWithIndex (\i t -> { key: i, title: t.title, status: t.status }) todos)
  where
  matches v t = match { "All": const true, "Active": const (isActive t.status), "Completed": const (completed t.status) } v

completed :: [ active :: {}, completed :: {} ] -> Boolean
completed = match { active: const false, completed: const true }

isActive :: [ active :: {}, completed :: {} ] -> Boolean
isActive = not <<< completed

flipped :: [ active :: {}, completed :: {} ] -> [ active :: {}, completed :: {} ]
flipped = match { active: const (.completed {}), completed: const (.active {}) }

isCompleted
  :: { key :: Int, status :: [ active :: {}, completed :: {} ], title :: String }
  -> Boolean
isCompleted { status } = completed status

