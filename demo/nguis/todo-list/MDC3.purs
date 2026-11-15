module TodoList.MDC3 (todoList) where

import Prelude ((#), ($), (>>>), Unit)

import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import PUI (actedWithin, fold, looped, stepped, with)
import PUI.Web (clWhen, text, textAt)
import PUI.Web.HTML (span)
import PUI.Web.MDC3 (body, bodySmall, button, filledTextField', iconToggle, list, listItem, segment, segmentedButton)
import QualifiedDo.Semigroupoid as Semigroupoid
import TodoList (addTodo, clearCompleted, isCompleted, itemsLeftLine, visibleEntries, weekendChores)

todoList :: Effect Unit
todoList =
  body $ Semigroupoid.do
    filledTextField' @"Todo" { hint: "What needs to be done?" }
    ( Semigroupoid.do
      button @"Add"
      fold @"Add" addTodo ) # stepped
    segmentedButton @"Visibility" $ VariantToVariant.do
      segment @"All"
      segment @"Active"
      segment @"Completed"
    bodySmall $ text itemsLeftLine
    list $
      ( listItem $ Semigroupoid.do
        iconToggle @"Done" { onIcon: "check_circle", offIcon: "radio_button_unchecked" }
        span >>> clWhen isCompleted "todo-done" $ textAt @"title" ) # actedWithin @"id" @"todos" visibleEntries
    ( Semigroupoid.do
      button @"Clear completed"
      fold @"Clear completed" clearCompleted ) # stepped
  # looped # with weekendChores
