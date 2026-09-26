module TodoListMDC2 (todoListMDC2) where

import Prelude ((#), ($), Unit)

import Data.Variant (match)
import Effect (Effect)
import PUI (applied, mvu, updated)
import PUI.Web (choice, clWhen, shownWhen, text)
import PUI.Web.HTML (span)
import PUI.Web.MDC2 (body, button, card, caption, filledTextField, listOf, segmentedButton)
import QualifiedDo.Semigroupoid as Semigroupoid
import TodoListLogic (addTodo, clearCompleted, emptyTodoList, isCompleted, remainingItems, severalLine, soleLine, toggleTodo, visibleEntries)

todoListMDC2 :: Effect Unit
todoListMDC2 =
  body $
    card $ ( Semigroupoid.do
      Semigroupoid.do
        filledTextField @"What needs to be done?" {}
        button @"Add" {} # applied addTodo
      listOf @"todoClicked" _.key { selected: isCompleted } visibleEntries (span (text _.title) # clWhen isCompleted "todo-done") # updated (match { todoClicked: toggleTodo })
      segmentedButton @"Visibility"
        [ choice @"All", choice @"Active", choice @"Completed" ]
      Semigroupoid.do
        caption (text soleLine) # shownWhen @"sole" remainingItems
        caption (text severalLine) # shownWhen @"several" remainingItems
        button @"Clear completed" {} # applied clearCompleted
    ) # mvu emptyTodoList
