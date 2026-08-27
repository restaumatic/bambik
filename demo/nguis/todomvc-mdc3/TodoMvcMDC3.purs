module TodoMvcMDC3 (todoMvcMDC3) where

import Prelude ((#), ($), Unit, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (applied, projection, forProperty, mvu, required, toCase, updated)
import PUI.Web (choice)
import PUI.Web.HTML (shownWhen, body, clWhen, span, staticText, text)
import PUI.Web.MDC3 (button, card, bodySmall, elevation5, filledTextField, listOf, segmentedButton)
import QualifiedDo.Category as Category
import TodoMvcLogic (addTodo, clearCompleted, emptyTodoList, remainingItems, toggleTodo, visibleEntries)

todoMvcMDC3 :: Effect Unit
todoMvcMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          Category.do
            filledTextField @"What needs to be done?" {}
            button @"Add" {} # applied addTodo
          listOf { selected: _.done } visibleEntries (span (text @"title") # forProperty # clWhen _.done "todo-done") # toCase @"todoClicked" _.key # updated (match { todoClicked: toggleTodo })
          segmentedButton @"Visibility"
            [ choice @"All", choice @"Active", choice @"Completed" ] # required
          Category.do
            ( bodySmall $ RecordToRecord.do
                text @"count" # projection show
                staticText " item left" ) # shownWhen @"sole" remainingItems
            ( bodySmall $ RecordToRecord.do
                text @"count" # projection show
                staticText " items left" ) # shownWhen @"several" remainingItems
            button @"Clear completed" {} # applied clearCompleted
      ) # mvu emptyTodoList
