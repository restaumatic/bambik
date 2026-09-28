module TodoListMDC3 (todoListMDC3) where

import Prelude ((#), ($), Unit)

import Data.Variant (match)
import Effect (Effect)
import PUI (applied, mvu, updated)
import PUI.Web (choice, clWhen, shownWhen, text)
import PUI.Web.HTML (span)
import PUI.Web.MDC3 (body, button, bodySmall, filledTextField, listOf, segmentedButton)
import QualifiedDo.Semigroupoid as Semigroupoid
import TodoListLogic (addTodo, clearCompleted, emptyTodoList, isCompleted, remainingItems, severalLine, soleLine, toggleTodo, visibleEntries)

todoListMDC3 :: Effect Unit
todoListMDC3 =
  body $
    ( Semigroupoid.do
      Semigroupoid.do
        filledTextField @"What needs to be done?" {}
        button @"Add" {} # applied addTodo
      listOf @"todoClicked" @"key" { selected: isCompleted } visibleEntries (span (text _.title) # clWhen isCompleted "todo-done") # updated (match { todoClicked: toggleTodo })
      segmentedButton @"Visibility"
        [ choice @"All", choice @"Active", choice @"Completed" ]
      Semigroupoid.do
        bodySmall (text soleLine) # shownWhen @"sole" remainingItems
        bodySmall (text severalLine) # shownWhen @"several" remainingItems
        button @"Clear completed" {} # applied clearCompleted
    ) # mvu emptyTodoList
