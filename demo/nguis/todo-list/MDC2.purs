module TodoList.MDC2 (todoList) where

import Prelude ((#), ($), (>>>), Unit)

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (fold, joined, looped, with)
import PUI.Web ((<+>), choice, clWhen, inCase, text, textAt)
import PUI.Web.HTML (span)
import PUI.Web.MDC2 (body, button, caption, filledTextField, listOf, segmentedButton)
import QualifiedDo.Semigroupoid as Semigroupoid
import TodoList (addTodo, clearCompleted, emptyTodoList, isCompleted, remainingItems, severalLine, soleLine, toggleTodo, visibleEntries)

todoList :: Effect Unit
todoList =
  body $ Semigroupoid.do
    filledTextField @"What needs to be done?"
    segmentedButton @"Visibility"
      (choice @"All" <+> choice @"Active" <+> choice @"Completed")
    ( caption $ text soleLine ) # inCase @"sole" @( sole :: {}, several :: {} ) remainingItems
    ( caption $ text severalLine ) # inCase @"several" remainingItems
    RecordToVariant.do
      button @"Add"
      ( listOf @"Todo toggled" @"id"
        @( id :: Int
         , title :: String
         , status :: [ active :: {}, completed :: {} ]
         ) visibleEntries $ span >>> clWhen isCompleted "todo-done" $ textAt @"title" ) # joined @"Todo toggled"
      button @"Clear completed"
    VariantToRecord.do
      fold @"Add" addTodo
      fold @"Todo toggled" toggleTodo
      fold @"Clear completed" clearCompleted
  # looped
    @( "What needs to be done?" :: String
     , todos :: Array { id :: Int, title :: String, status :: [ active :: {}, completed :: {} ] }
     , "Visibility" :: [ "All" :: {}, "Active" :: {}, "Completed" :: {} ]
     ) # with emptyTodoList
