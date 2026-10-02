module TodoListMDC2 (todoListMDC2) where

import Prelude ((#), ($), Unit)

import Data.Variant (match)
import Effect (Effect)
import PUI (applied, mvu, state, updated)
import PUI.Web ((<+>), choice, clWhen, shownWhen, text)
import PUI.Web.HTML (span)
import PUI.Web.MDC2 (body, button, caption, filledTextField, listOf, segmentedButton)
import QualifiedDo.Semigroupoid as Semigroupoid
import TodoListViewModel (addTodo, clearCompleted, emptyTodoList, isCompleted, remainingItems, severalLine, soleLine, toggleTodo, visibleEntries)

todoListMDC2 :: Effect Unit
todoListMDC2 =
  body $
    ( Semigroupoid.do
      state @"todos" @(Array { title :: String, status :: [ active :: {}, completed :: {} ] })
      Semigroupoid.do
        filledTextField @"What needs to be done?" {}
        button @"Add" {} # applied addTodo
      listOf @"toggled" @"key" @Int { selected: isCompleted } visibleEntries ( Semigroupoid.do
        state @"status" @[ active :: {}, completed :: {} ]
        span (text _.title) # clWhen isCompleted "todo-done" ) # updated (match { toggled: toggleTodo })
      segmentedButton @"Visibility"
        (choice @"All" <+> choice @"Active" <+> choice @"Completed")
      Semigroupoid.do
        ( Semigroupoid.do
          state @"count" @Int
          caption (text soleLine) ) # shownWhen @"sole" remainingItems
        ( Semigroupoid.do
          state @"count" @Int
          caption (text severalLine) ) # shownWhen @"several" remainingItems
        button @"Clear completed" {} # applied clearCompleted
    ) # mvu emptyTodoList
