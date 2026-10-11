module TodoListMDC3 (todoListMDC3) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (blankStatus, fold, joined, looped, prefixed, with)
import PUI.Web ((<+>), choice, clWhen, pane, text, textAt)
import PUI.Web.HTML (span)
import PUI.Web.MDC3 (body, button, bodySmall, filledTextField, listOf, segmentedButton)
import QualifiedDo.Semigroupoid as Semigroupoid
import TodoListViewModel (addTodo, clearCompleted, emptyTodoList, isCompleted, remainingItems, severalLine, soleLine, toggleTodo, visibleEntries)

todoListMDC3 :: Effect Unit
todoListMDC3 =
  body $ Semigroupoid.do
    filledTextField @"What needs to be done?" {}
    segmentedButton @"Visibility"
      (choice @"All" <+> choice @"Active" <+> choice @"Completed")
    prefixed ( RecordToRecord.do
      bodySmall (text soleLine) # pane @"sole" @( sole :: { count :: Int }, several :: { count :: Int } ) remainingItems
      bodySmall (text severalLine) # pane @"several" remainingItems ) $ RecordToVariant.do
      button @"Add" {}
      listOf @"Todo toggled" @"key"
        @( key :: Int
         , title :: String
         , status :: [ active :: {}, completed :: {} ]
         ) {} visibleEntries (span (textAt @"title") # clWhen isCompleted "todo-done") # joined @"Todo toggled"
      button @"Clear completed" {}
    VariantToRecord.do
      blankStatus @"Add" # fold addTodo
      blankStatus @"Todo toggled" # fold toggleTodo
      blankStatus @"Clear completed" # fold clearCompleted
  # looped
    @( "What needs to be done?" :: String
     , todos :: Array { title :: String, status :: [ active :: {}, completed :: {} ] }
     , "Visibility" :: [ "All" :: {}, "Active" :: {}, "Completed" :: {} ]
     ) # with emptyTodoList
