module TodoMvc (todoMvc) where

import Prelude ((#), ($), (<<<), (<>), (==), Unit, const, not, show, unit)

import Data.Array (filter, length, mapWithIndex, modifyAt, snoc)
import Data.Maybe (fromMaybe)
import Data.Profunctor (lcmap, rmap)
import Data.String (trim)
import Data.Variant (match)
import Effect (Effect)
import PUI (asField, completed, mvu, projection, required, toCase, updates)
import PUI.HTML (body, clWhen, span, text)
import PUI.MDC (button, card, caption, elevation20, filledTextField, listOf, segmentedButton)
import QualifiedDo.Semigroupoid as Semigroupoid

todoMvc :: Effect Unit
todoMvc =
  body $
    elevation20 $
      card { caption: "TodoMVC" } $ ( Semigroupoid.do
          Semigroupoid.do
            filledTextField { floatingLabel: "What needs to be done?" } # asField @"entry" # completed
            button { label: "Add" } # updates (match { clicked: const <<< addTodo })
          listOf { selected: _.done } (span text # projection _.title # clWhen _.done "todo-done") # rmap _.key # toCase @"todoClicked" # lcmap visibleEntries # updates (match { todoClicked: toggleTodo })
          segmentedButton visibilityChoices # required # asField @"visibility" # completed
          Semigroupoid.do
            caption text # projection itemsLeft # completed
            button { label: "Clear completed" } # updates (match { clicked: const <<< clearCompleted })
      ) # mvu emptyTodoList

emptyTodoList :: { entry :: String, todos :: Array { title :: String, done :: Boolean }, visibility :: [ all :: Unit, active :: Unit, completed :: Unit ] }
emptyTodoList = { entry: "", todos: [], visibility: .all unit }

visibilityChoices :: Array { value :: [ all :: Unit, active :: Unit, completed :: Unit ], label :: String }
visibilityChoices =
  [ { value: .all unit, label: "All" }
  , { value: .active unit, label: "Active" }
  , { value: .completed unit, label: "Completed" }
  ]

addTodo :: { entry :: String, todos :: Array { title :: String, done :: Boolean }, visibility :: [ all :: Unit, active :: Unit, completed :: Unit ] } -> { entry :: String, todos :: Array { title :: String, done :: Boolean }, visibility :: [ all :: Unit, active :: Unit, completed :: Unit ] }
addTodo m =
  if trim m.entry == "" then m
  else m { todos = snoc m.todos { title: trim m.entry, done: false }, entry = "" }

toggleTodo :: Int -> { entry :: String, todos :: Array { title :: String, done :: Boolean }, visibility :: [ all :: Unit, active :: Unit, completed :: Unit ] } -> { entry :: String, todos :: Array { title :: String, done :: Boolean }, visibility :: [ all :: Unit, active :: Unit, completed :: Unit ] }
toggleTodo i m = m { todos = fromMaybe m.todos (modifyAt i (\t -> t { done = not t.done }) m.todos) }

clearCompleted :: { entry :: String, todos :: Array { title :: String, done :: Boolean }, visibility :: [ all :: Unit, active :: Unit, completed :: Unit ] } -> { entry :: String, todos :: Array { title :: String, done :: Boolean }, visibility :: [ all :: Unit, active :: Unit, completed :: Unit ] }
clearCompleted m = m { todos = filter (\t -> not t.done) m.todos }

itemsLeft :: { entry :: String, todos :: Array { title :: String, done :: Boolean }, visibility :: [ all :: Unit, active :: Unit, completed :: Unit ] } -> String
itemsLeft m = case length (filter (\t -> not t.done) m.todos) of
  1 -> "1 item left"
  n -> show n <> " items left"

visibleEntries :: { entry :: String, todos :: Array { title :: String, done :: Boolean }, visibility :: [ all :: Unit, active :: Unit, completed :: Unit ] } -> Array { key :: Int, title :: String, done :: Boolean }
visibleEntries m = filter (matches m.visibility) (mapWithIndex (\i t -> { key: i, title: t.title, done: t.done }) m.todos)
  where
  matches v t = match { all: const true, active: \_ -> not t.done, completed: \_ -> t.done } v
