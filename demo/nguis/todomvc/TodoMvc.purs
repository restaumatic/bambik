module TodoMvc (todoMvc) where

import Prelude ((#), ($), (<<<), (<>), (==), (>>>), class Eq, Unit, const, not, show)

import Data.Array (filter, length, mapWithIndex, modifyAt, snoc)
import Data.Maybe (fromMaybe)
import Data.Profunctor (lcmap, rmap)
import Data.String (trim)
import Data.Variant (match)
import Effect (Effect)
import PUI (asField, completed, forValue, mvu, projection, required, toCase, updates)
import PUI.HTML (attr, body, clWhen, div, span, text)
import PUI.MDC (button, card, caption, elevation20, filledTextField, listOf, segmentedButton)
import QualifiedDo.Semigroupoid as Semigroupoid

todoMvc :: Effect Unit
todoMvc =
  body $
    elevation20 $
      card { caption: "TodoMVC" } $ ( Semigroupoid.do
          div >>> attr "style" "display: flex; gap: 8px; align-items: center;" $ Semigroupoid.do
            filledTextField { floatingLabel: "What needs to be done?" } # asField @"entry" # completed
            button { label: "Add" } # updates (match { clicked: const <<< addTodo })
          attr "style" "border: 1px solid #ccc; min-height: 80px; max-height: 240px; overflow-y: auto; margin: 8px 0;"
            ( listOf { selected: _.done } (span (text # projection _.title # forValue) # clWhen _.done "todo-done")
            ) # rmap _.key # toCase @"todoClicked" # lcmap visibleEntries # updates (match { todoClicked: toggleTodo })
          segmentedButton visibilityChoices # required # asField @"visibility" # completed
          div >>> attr "style" "display: flex; gap: 16px; align-items: center; margin-top: 8px;" $ Semigroupoid.do
            caption (text # projection itemsLeft # forValue) # completed
            button { label: "Clear completed" } # updates (match { clicked: const <<< clearCompleted })
      ) # mvu emptyTodoList

type Todo = { title :: String, done :: Boolean }

data Visibility = All | Active | Completed

derive instance Eq Visibility

type TodoList =
  { entry :: String
  , todos :: Array Todo
  , visibility :: Visibility
  }

emptyTodoList :: TodoList
emptyTodoList = { entry: "", todos: [], visibility: All }

visibilityChoices :: Array { value :: Visibility, label :: String }
visibilityChoices =
  [ { value: All, label: "All" }
  , { value: Active, label: "Active" }
  , { value: Completed, label: "Completed" }
  ]

addTodo :: TodoList -> TodoList
addTodo m =
  if trim m.entry == "" then m
  else m { todos = snoc m.todos { title: trim m.entry, done: false }, entry = "" }

toggleTodo :: Int -> TodoList -> TodoList
toggleTodo i m = m { todos = fromMaybe m.todos (modifyAt i (\t -> t { done = not t.done }) m.todos) }

clearCompleted :: TodoList -> TodoList
clearCompleted m = m { todos = filter (\t -> not t.done) m.todos }

itemsLeft :: TodoList -> String
itemsLeft m = case length (filter (\t -> not t.done) m.todos) of
  1 -> "1 item left"
  n -> show n <> " items left"

visibleEntries :: TodoList -> Array { key :: Int, title :: String, done :: Boolean }
visibleEntries m = filter (matches m.visibility) (mapWithIndex (\i t -> { key: i, title: t.title, done: t.done }) m.todos)
  where
  matches All _ = true
  matches Active t = not t.done
  matches Completed t = t.done
