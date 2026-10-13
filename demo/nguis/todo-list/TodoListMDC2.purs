module TodoListMDC2 (todoListMDC2) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (fold, joined, looped, unshown, with)
import PUI.Web ((<+>), choice, clWhen, inCase, text, textAt)
import PUI.Web.HTML (span)
import PUI.Web.MDC2 (body, button, caption, filledTextField, listOf, segmentedButton)
import QualifiedDo.Semigroupoid as Semigroupoid
import TodoListViewModel (addTodo, clearCompleted, emptyTodoList, isCompleted, remainingItems, severalLine, soleLine, toggleTodo, visibleEntries)

todoListMDC2 :: Effect Unit
todoListMDC2 =
  body $ Semigroupoid.do
    filledTextField @"What needs to be done?"
    segmentedButton @"Visibility"
      (choice @"All" <+> choice @"Active" <+> choice @"Completed")
    caption (text soleLine) # inCase @"sole" @( sole :: {}, several :: {} ) remainingItems
    caption (text severalLine) # inCase @"several" remainingItems
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
