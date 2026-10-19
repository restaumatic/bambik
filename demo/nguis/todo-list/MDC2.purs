module TodoList.MDC2 (todoList) where

import Prelude ((#), ($), (>>>), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import PUI (fold, joined, looped, with)
import PUI.Web (clWhen, text, textAt)
import PUI.Web.HTML (span)
import PUI.Web.MDC2 (body, button, caption, filledTextField', listOf, segment, segmentedButton)
import QualifiedDo.Semigroupoid as Semigroupoid
import TodoList (addTodo, clearCompleted, emptyTodoList, isCompleted, itemsLeftLine, toggleTodo, visibleEntries)

todoList :: Effect Unit
todoList =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      filledTextField' @"Todo" { hint: "What needs to be done?" }
      segmentedButton @"Visibility" $ VariantToVariant.do
        segment @"All"
        segment @"Active"
        segment @"Completed"
    ) # looped
    caption $ text itemsLeftLine
    RecordToVariant.do
      button @"Add"
      ( listOf @"Todo toggled" @"id" visibleEntries $ span >>> clWhen isCompleted "todo-done" $ textAt @"title" ) # joined @"Todo toggled"
      button @"Clear completed"
    VariantToRecord.do
      fold @"Add" addTodo
      fold @"Todo toggled" toggleTodo
      fold @"Clear completed" clearCompleted
  # looped # with emptyTodoList
