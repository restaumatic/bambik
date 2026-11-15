module TodoList.MDC2 (todoList) where

import Prelude ((#), ($), (>>>), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import PUI (actedWithin, edits)
import PUI.Web (clWhen, text, textAt)
import PUI.Web.HTML (span)
import PUI.Web.MDC2 (body, button, caption, filledTextField', iconToggle, list, listItem, segment, segmentedButton)
import QualifiedDo.Semigroupoid as Semigroupoid
import TodoList (isCompleted, itemsLeftLine, listed, visibleEntries)

todoList :: Effect Unit
todoList =
  body $ RecordToVariant.do
    filledTextField' @"Todo" { hint: "What needs to be done?" } # edits @"Todo typed"
    button @"Add"
    ( Semigroupoid.do
      segmentedButton @"Visibility" $ VariantToVariant.do
        segment @"All"
        segment @"Active"
        segment @"Completed"
      caption $ text itemsLeftLine
      list $
        ( listItem $ Semigroupoid.do
          iconToggle @"Done" { onIcon: "check_circle", offIcon: "radio_button_unchecked" }
          span >>> clWhen isCompleted "todo-done" $ textAt @"title" ) # actedWithin @"id" @"todos" visibleEntries ) # edits @"List edited"
    button @"Clear completed"
  # listed
