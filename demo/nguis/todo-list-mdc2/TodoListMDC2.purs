module TodoListMDC2 (todoListMDC2) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import PUI (fold, joined, looped, with)
import PUI.Web ((<+>), choice, clWhen, shownWhen, text)
import PUI.Web.HTML (span)
import PUI.Web.MDC2 (body, button, caption, filledTextField, listOf, segmentedButton)
import QualifiedDo.Semigroupoid as Semigroupoid
import TodoListViewModel (addTodo, clearCompleted, emptyTodoList, isCompleted, remainingItems, severalLine, soleLine, toggleTodo, visibleEntries)

todoListMDC2 :: Effect Unit
todoListMDC2 =
  body $
    ( Semigroupoid.do
      filledTextField @"What needs to be done?" {}
      segmentedButton @"Visibility"
        (choice @"All" <+> choice @"Active" <+> choice @"Completed")
      caption (text soleLine) # shownWhen @"sole" @( sole :: { count :: Int }, several :: { count :: Int } ) remainingItems
      caption (text severalLine) # shownWhen @"several" remainingItems
      RecordToVariant.do
        button @"Add" {}
        listOf @"toggled" @"key" @( key :: Int, title :: String, status :: [ active :: {}, completed :: {} ] ) { selected: isCompleted } visibleEntries (span (text _.title) # clWhen isCompleted "todo-done") # joined @"toggled"
        button @"Clear completed" {}
      VariantToRecord.do
        fold @"Add" addTodo
        fold @"toggled" toggleTodo
        fold @"Clear completed" clearCompleted
    ) # looped
      @( "What needs to be done?" :: String
       , todos :: Array { title :: String, status :: [ active :: {}, completed :: {} ] }
       , "Visibility" :: [ "All" :: {}, "Active" :: {}, "Completed" :: {} ]
       ) # with emptyTodoList
