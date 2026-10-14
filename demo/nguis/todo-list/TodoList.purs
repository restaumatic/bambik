module TodoList (addTodo, clearCompleted, emptyTodoList, isCompleted, remainingItems, severalLine, soleLine, toggleTodo, visibleEntries) where

import Prelude ((+), (<), (<<<), (<>), (==), const, map, not, show)

import Data.Array (filter, foldl, length, snoc)
import Data.String (trim)
import Data.Variant (match)

emptyTodoList
  :: { "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ]
     , "What needs to be done?" :: String
     , todos :: Array { id :: Int, status :: [ active :: {}, completed :: {} ], title :: String }
     }
emptyTodoList = { "What needs to be done?": "", todos: [], "Visibility": ."All" {} }

addTodo
  :: { "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ]
     , "What needs to be done?" :: String
     , todos :: Array { id :: Int, status :: [ active :: {}, completed :: {} ], title :: String }
     }
  -> { "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ]
     , "What needs to be done?" :: String
     , todos :: Array { id :: Int, status :: [ active :: {}, completed :: {} ], title :: String }
     }
addTodo m@{ "What needs to be done?": entry, todos } =
  if trim entry == "" then m
  else m { todos = snoc todos { id: nextId todos, title: trim entry, status: .active {} }, "What needs to be done?" = "" }

toggleTodo
  :: { event :: Int
     , model :: { "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ]
                , "What needs to be done?" :: String
                , todos :: Array { id :: Int
                                 , status :: [ active :: {}, completed :: {} ]
                                 , title :: String
                                 }
                }
     }
  -> { "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ]
     , "What needs to be done?" :: String
     , todos :: Array { id :: Int, status :: [ active :: {}, completed :: {} ], title :: String }
     }
toggleTodo { event: id, model: m@{ todos } } = m { todos = map (\t -> if t.id == id then t { status = flipped t.status } else t) todos }

clearCompleted
  :: { "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ]
     , "What needs to be done?" :: String
     , todos :: Array { id :: Int, status :: [ active :: {}, completed :: {} ], title :: String }
     }
  -> { "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ]
     , "What needs to be done?" :: String
     , todos :: Array { id :: Int, status :: [ active :: {}, completed :: {} ], title :: String }
     }
clearCompleted m@{ todos } = m { todos = filter (isActive <<< _.status) todos }

itemsLeft
  :: forall r1.
     { todos :: Array { id :: Int, status :: [ active :: {}, completed :: {} ], title :: String }
     | r1
     }
  -> Int
itemsLeft { todos } = length (filter (isActive <<< _.status) todos)

remainingItems
  :: { "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ]
     , "What needs to be done?" :: String
     , todos :: Array { id :: Int, status :: [ active :: {}, completed :: {} ], title :: String }
     }
  -> [ several :: {}, sole :: {} ]
remainingItems { todos } = if itemsLeft { todos } == 1 then .sole {} else .several {}

soleLine
  :: { "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ]
     , "What needs to be done?" :: String
     , todos :: Array { id :: Int, status :: [ active :: {}, completed :: {} ], title :: String }
     }
  -> String
soleLine { todos } = show (itemsLeft { todos }) <> " item left"

severalLine
  :: { "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ]
     , "What needs to be done?" :: String
     , todos :: Array { id :: Int, status :: [ active :: {}, completed :: {} ], title :: String }
     }
  -> String
severalLine { todos } = show (itemsLeft { todos }) <> " items left"

visibleEntries
  :: { "Visibility" :: [ "Active" :: {}, "All" :: {}, "Completed" :: {} ]
     , "What needs to be done?" :: String
     , todos :: Array { id :: Int, status :: [ active :: {}, completed :: {} ], title :: String }
     }
  -> Array { id :: Int, status :: [ active :: {}, completed :: {} ], title :: String }
visibleEntries { todos, "Visibility": visibility } = filter (matches visibility) todos
  where
  matches v t = match { "All": const true, "Active": const (isActive t.status), "Completed": const (completed t.status) } v

completed :: [ active :: {}, completed :: {} ] -> Boolean
completed = match { active: const false, completed: const true }

isActive :: [ active :: {}, completed :: {} ] -> Boolean
isActive = not <<< completed

flipped :: [ active :: {}, completed :: {} ] -> [ active :: {}, completed :: {} ]
flipped = match { active: const (.completed {}), completed: const (.active {}) }

isCompleted
  :: { id :: Int, status :: [ active :: {}, completed :: {} ], title :: String }
  -> Boolean
isCompleted { status } = completed status

nextId :: Array { id :: Int, status :: [ active :: {}, completed :: {} ], title :: String } -> Int
nextId = foldl (\n t -> if t.id < n then n else t.id + 1) 0

