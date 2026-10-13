module TodoList.MDC3 (todoList) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (fold, joined, looped, unshown, with)
import PUI.Web ((<+>), choice, clWhen, inCase, text, textAt)
import PUI.Web.HTML (span)
import PUI.Web.MDC3 (body, button, bodySmall, filledTextField, listOf, segmentedButton)
import QualifiedDo.Semigroupoid as Semigroupoid
import TodoList (addTodo, clearCompleted, emptyTodoList, isCompleted, remainingItems, severalLine, soleLine, toggleTodo, visibleEntries)

todoList :: Effect Unit
todoList =
  body $ Semigroupoid.do
    filledTextField @"What needs to be done?"
    segmentedButton @"Visibility"
      (choice @"All" <+> choice @"Active" <+> choice @"Completed")
    bodySmall (text soleLine) # inCase @"sole" @( sole :: {}, several :: {} ) remainingItems
    bodySmall (text severalLine) # inCase @"several" remainingItems
    RecordToVariant.do
      button @"Add"
      listOf @"Todo toggled" @"key"
        @( key :: Int
         , title :: String
         , status :: [ active :: {}, completed :: {} ]
         ) visibleEntries (span (textAt @"title") # clWhen isCompleted "todo-done") # joined @"Todo toggled"
      button @"Clear completed"
    VariantToRecord.do
      unshown @"Add" # fold addTodo
      unshown @"Todo toggled" # fold toggleTodo
      unshown @"Clear completed" # fold clearCompleted
  # looped
    @( "What needs to be done?" :: String
     , todos :: Array { title :: String, status :: [ active :: {}, completed :: {} ] }
     , "Visibility" :: [ "All" :: {}, "Active" :: {}, "Completed" :: {} ]
     ) # with emptyTodoList
