module TodoMvcMDC3 (todoMvcMDC3) where

import Prelude (identity, (#), ($), (<<<), Unit, const, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (completed, displayed, projection, forProperty, mvu, required, toCase, updated)
import PUI.Web.HTML (providedCase, body, clWhen, span, staticText, text)
import PUI.Web.MDC3 (button, card, bodySmall, elevation5, filledTextField, listOf, segmentedButton)
import QualifiedDo.Semigroupoid as Semigroupoid
import TodoMvcLogic (addTodo, clearCompleted, emptyTodoList, remainingItems, toggleTodo, visibleEntries)

todoMvcMDC3 :: Effect Unit
todoMvcMDC3 =
  body $
    elevation5 $
      card { caption: "TodoMVC" } $ ( Semigroupoid.do
          Semigroupoid.do
            filledTextField @"entry" { floatingLabel: "What needs to be done?" } # completed
            button @"add" { label: "Add" } # updated (match { add: const <<< addTodo })
          listOf { selected: _.done } visibleEntries (span (text @"title") # forProperty identity # clWhen _.done "todo-done") # toCase @"todoClicked" _.key # updated (match { todoClicked: toggleTodo })
          segmentedButton @"visibility"
            [ { value: .all {}, label: "All" }
            , { value: .active {}, label: "Active" }
            , { value: .completed {}, label: "Completed" }
            ] # required # completed
          Semigroupoid.do
            bodySmall ( RecordToRecord.do
                text @"count" # projection show
                staticText " item left" ) # providedCase @"sole" remainingItems # displayed
            bodySmall ( RecordToRecord.do
                text @"count" # projection show
                staticText " items left" ) # providedCase @"several" remainingItems # displayed
            button @"clearCompleted" { label: "Clear completed" } # updated (match { clearCompleted: const <<< clearCompleted })
      ) # mvu emptyTodoList
