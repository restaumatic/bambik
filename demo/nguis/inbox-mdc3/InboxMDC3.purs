module InboxMDC3 (inboxMDC3) where

import Prelude ((#), ($), (<<<), Unit, const)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import InboxViewModel (composeMessage, deleteOpened, bodyText, fromLine, highlighted, inboxZeroLine, keepMessages, messageLine, messageView, mondayMail, openMessage, requestDelete, sortBySender, sortBySubject, sortUnreadFirst, subjectLine, unreadLine)
import PUI (applied, mvu, observed, updated, with)
import PUI.Web (provided, shown, text)
import PUI.Web.HTML (span)
import PUI.Web.MDC3 (body, bodyLarge, bodyMedium, bodySmall, button, dialog, fab, headlineSmall, iconButton, listOf, menu, menuItem, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

inboxMDC3 :: Effect Unit
inboxMDC3 =
  body $
    ( Semigroupoid.do
      ( bodySmall $ text unreadLine ) # shown
      listOf @"opened" @"id" { selected: highlighted } _.messages ( span $ text messageLine # shown ) # updated (match { opened: openMessage })
      ( Semigroupoid.do
        headlineSmall (text subjectLine) # shown
        bodyMedium (text fromLine) # shown
        bodyLarge (text bodyText) # shown
        iconButton @"Delete message" {} "delete" ) # provided @"reading" @( reading :: { sender :: String, subject :: String, body :: String }, browsing :: {} ) messageView # updated (match { "Delete message": const requestDelete })
      ( Semigroupoid.do
        ( dialog @"Delete the last message?" $ RecordToVariant.do
          button @"Delete" {} # with {}
          button @"Keep" {} # with {} ) # provided @"confirming" _.deletion
        snackbar @"Delete" inboxZeroLine # observed ) # updated (match { "Delete": const deleteOpened, "Keep": const keepMessages })
      fab @"Compose" {} "edit" # applied composeMessage
      ( menu @"Sort" $ RecordToVariant.do
        menuItem @"By sender" {}
        menuItem @"By subject" {}
        menuItem @"Unread first" {} ) # updated (match { "By sender": const <<< sortBySender, "By subject": const <<< sortBySubject, "Unread first": const <<< sortUnreadFirst })
    ) # mvu
      @( messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {}, open :: {} ] }
       , deletion :: [ silent :: {}, confirming :: {} ]
       )
      mondayMail
