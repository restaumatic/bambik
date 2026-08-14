module TodoMvcMDC2 (todoMvcMDC2) where

import Prelude (identity, (#), ($), (<<<), Unit, const, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (completed, displayed, projection, forProperty, mvu, required, toCase, updated)
import PUI.Web.HTML (providedCase, body, clWhen, span, staticText, text)
import PUI.Web.MDC2 (button, card, caption, elevation20, filledTextField, listOf, segmentedButton)
import QualifiedDo.Semigroupoid as Semigroupoid
import TodoMvcLogic (addTodo, clearCompleted, emptyTodoList, remainingItems, toggleTodo, visibleEntries)

todoMvcMDC2 :: Effect Unit
todoMvcMDC2 =
  body $
    elevation20 $
      card { caption: "TodoMVC" } $ ( Semigroupoid.do
          Semigroupoid.do
            filledTextField @"What needs to be done?" {} # completed
            button @"Add" {} # updated (match { "Add": const <<< addTodo })
          listOf { selected: _.done } visibleEntries (span (text @"title") # forProperty identity # clWhen _.done "todo-done") # toCase @"todoClicked" _.key # updated (match { todoClicked: toggleTodo })
          segmentedButton @"Visibility"
            [ { value: .all {}, label: "All" }
            , { value: .active {}, label: "Active" }
            , { value: .completed {}, label: "Completed" }
            ] # required # completed
          Semigroupoid.do
            caption ( RecordToRecord.do
                text @"count" # projection show
                staticText " item left" ) # providedCase @"sole" remainingItems # displayed
            caption ( RecordToRecord.do
                text @"count" # projection show
                staticText " items left" ) # providedCase @"several" remainingItems # displayed
            button @"Clear completed" {} # updated (match { "Clear completed": const <<< clearCompleted })
      ) # mvu emptyTodoList
