module TodoMvcMDC2 (todoMvcMDC2) where

import Prelude (identity, (#), ($), (<<<), Unit, const, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (projection, forProperty, mvu, required, toCase, updated)
import PUI.Web (choice)
import PUI.Web.HTML (shownCase, body, clWhen, span, staticText, text)
import PUI.Web.MDC2 (button, card, caption, elevation20, filledTextField, listOf, segmentedButton)
import QualifiedDo.Semigroupoid as Semigroupoid
import TodoMvcLogic (addTodo, clearCompleted, emptyTodoList, remainingItems, toggleTodo, visibleEntries)

todoMvcMDC2 :: Effect Unit
todoMvcMDC2 =
  body $
    elevation20 $
      card $ ( Semigroupoid.do
          Semigroupoid.do
            filledTextField @"What needs to be done?" {}
            button @"Add" {} # updated (match { "Add": const <<< addTodo })
          listOf { selected: _.done } visibleEntries (span (text @"title") # forProperty identity # clWhen _.done "todo-done") # toCase @"todoClicked" _.key # updated (match { todoClicked: toggleTodo })
          segmentedButton @"Visibility"
            [ choice @"All", choice @"Active", choice @"Completed" ] # required
          Semigroupoid.do
            shownCase @"sole" remainingItems ( caption $ RecordToRecord.do
                text @"count" # projection show
                staticText " item left" )
            shownCase @"several" remainingItems ( caption $ RecordToRecord.do
                text @"count" # projection show
                staticText " items left" )
            button @"Clear completed" {} # updated (match { "Clear completed": const <<< clearCompleted })
      ) # mvu emptyTodoList
