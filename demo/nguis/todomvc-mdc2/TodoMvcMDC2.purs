module TodoMvcMDC2 (todoMvcMDC2) where

import Prelude ((#), ($), (<<<), (==), Unit, const, not, show)

import Data.Array (filter, length, mapWithIndex, modifyAt, snoc)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.String (trim)
import Data.Variant (match)
import Effect (Effect)
import PUI (asField, completed, displayed, forField, mvu, ofField, projection, required, toCase, updates)
import PUI.HTML (body, clWhen, provided, span, staticText, text)
import PUI.MDC2 (button, card, caption, elevation20, filledTextField, listOf, segmentedButton)
import QualifiedDo.Semigroupoid as Semigroupoid

todoMvcMDC2 :: Effect Unit
todoMvcMDC2 =
  body $
    elevation20 $
      card { caption: "TodoMVC" } $ ( Semigroupoid.do
          Semigroupoid.do
            filledTextField { floatingLabel: "What needs to be done?" } # asField @"entry" # completed
            button { label: "Add" } # updates (match { clicked: const <<< addTodo })
          listOf { selected: _.done } visibleEntries (span text # ofField @"title" # clWhen _.done "todo-done") # toCase @"todoClicked" _.key # updates (match { todoClicked: toggleTodo })
          segmentedButton
            [ { value: .all {}, label: "All" }
            , { value: .active {}, label: "Active" }
            , { value: .completed {}, label: "Completed" }
            ] # required # asField @"visibility" # completed
          Semigroupoid.do
            caption ( RecordToRecord.do
                text # projection show # forField @"count"
                staticText " item left" ) # provided soleItemLeft # displayed
            caption ( RecordToRecord.do
                text # projection show # forField @"count"
                staticText " items left" ) # provided severalItemsLeft # displayed
            button { label: "Clear completed" } # updates (match { clicked: const <<< clearCompleted })
      ) # mvu emptyTodoList

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

soleItemLeft :: { todos :: Array { title :: String, done :: Boolean } } -> Maybe { count :: Int }
soleItemLeft { todos } = if itemsLeft { todos } == 1 then Just { count: 1 } else Nothing

severalItemsLeft :: { todos :: Array { title :: String, done :: Boolean } } -> Maybe { count :: Int }
severalItemsLeft { todos } = if itemsLeft { todos } == 1 then Nothing else Just { count: itemsLeft { todos } }

visibleEntries :: { todos :: Array { title :: String, done :: Boolean }, visibility :: [ all :: {}, active :: {}, completed :: {} ] } -> Array { key :: Int, title :: String, done :: Boolean }
visibleEntries { todos, visibility } = filter (matches visibility) (mapWithIndex (\i t -> { key: i, title: t.title, done: t.done }) todos)
  where
  matches v t = match { all: const true, active: \_ -> not t.done, completed: \_ -> t.done } v
