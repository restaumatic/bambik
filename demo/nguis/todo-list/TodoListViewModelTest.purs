module TodoListViewModelTest (todoListClaims) where

import Prelude ((&&), (==), map)

import TodoListViewModel (addTodo, clearCompleted, emptyTodoList, remainingItems, severalLine, soleLine, toggleTodo, visibleEntries)

todoListClaims :: Array { claim :: String, holds :: Boolean }
todoListClaims =
  [ { claim: "adding stores the trimmed title and clears the field", holds: withMilk.todos == [ { title: "Buy milk", status: .active {} } ] && withMilk."New todo" == "" }
  , { claim: "a blank entry adds nothing", holds: addTodo (emptyTodoList { "New todo" = "   " }) == emptyTodoList { "New todo" = "   " } }
  , { claim: "toggling completes the todo", holds: map _.status milkBought.todos == [ .completed {} ] }
  , { claim: "one active todo is the sole case", holds: remainingItems withMilk == .sole { count: 1 } }
  , { claim: "the counts read naturally", holds: soleLine { count: 1 } == "1 item left" && severalLine { count: 3 } == "3 items left" }
  , { claim: "the completed filter shows only completed todos", holds: map _.title (visibleEntries (milkBought { "Visibility" = ."Completed" {} })) == [ "Buy milk" ] }
  , { claim: "clearing completed removes them", holds: (clearCompleted milkBought).todos == [] }
  ]
  where
  withMilk = addTodo (emptyTodoList { "New todo" = "  Buy milk  " })
  milkBought = toggleTodo { event: 0, model: withMilk }
