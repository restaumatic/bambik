module TodoMvcMDC3 (todoMvcMDC3) where

import Prelude (identity, (#), ($), (<<<), Unit, const, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (asField, completed, displayed, forField, forProperty, mvu, required, toCase, updated)
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
            filledTextField { floatingLabel: "What needs to be done?" } # asField @"value" @"entry" # completed
            button { label: "Add" } # updated (match { clicked: const <<< addTodo })
          listOf { selected: _.done } visibleEntries (span text # forProperty @"value" @"title" identity # clWhen _.done "todo-done") # toCase @"todoClicked" _.key # updated (match { todoClicked: toggleTodo })
          segmentedButton
            [ { value: .all {}, label: "All" }
            , { value: .active {}, label: "Active" }
            , { value: .completed {}, label: "Completed" }
            ] # required @"value" # asField @"value" @"visibility" # completed
          Semigroupoid.do
            bodySmall ( RecordToRecord.do
                text # forField @"value" @"count" show
                staticText " item left" ) # providedCase @"sole" remainingItems # displayed
            bodySmall ( RecordToRecord.do
                text # forField @"value" @"count" show
                staticText " items left" ) # providedCase @"several" remainingItems # displayed
            button { label: "Clear completed" } # updated (match { clicked: const <<< clearCompleted })
      ) # mvu emptyTodoList
