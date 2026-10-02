module InboxMDC2 (inboxMDC2) where

import Prelude ((#), ($), (<<<), Unit, const)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import InboxViewModel (composeMessage, deleteOpened, bodyText, fromLine, highlighted, inboxZeroLine, keepMessages, messageLine, messageView, mondayMail, openMessage, requestDelete, sortBySender, sortBySubject, sortUnreadFirst, subjectLine, unreadLine)
import PUI (applied, mvu, observed, state, updated, with)
import PUI.Web (provided, shown, text)
import PUI.Web.HTML (span)
import PUI.Web.MDC2 (banner, body, body1, body2, button, caption, dialog, fab, headline6, iconButton, listOf, menu, menuItem)
import QualifiedDo.Semigroupoid as Semigroupoid

inboxMDC2 :: Effect Unit
inboxMDC2 =
  body $
    ( Semigroupoid.do
      state @"deletion" @[ silent :: {}, confirming :: {} ]
      ( caption $ text unreadLine ) # shown
      listOf @"opened" @"id" @Int { selected: highlighted } _.messages ( Semigroupoid.do
        state @"sender" @String
        state @"subject" @String
        state @"body" @String
        state @"status" @[ unread :: {}, read :: {}, open :: {} ]
        span $ text messageLine # shown ) # updated (match { opened: openMessage })
      ( Semigroupoid.do
        state @"sender" @String
        state @"subject" @String
        state @"body" @String
        headline6 (text subjectLine) # shown
        body2 (text fromLine) # shown
        body1 (text bodyText) # shown
        iconButton @"Delete message" {} "delete" ) # provided @"reading" messageView # updated (match { "Delete message": const requestDelete })
      ( Semigroupoid.do
        ( dialog @"Delete the last message?" $ RecordToVariant.do
          button @"Delete" {} # with {}
          button @"Keep" {} # with {} ) # provided @"confirming" _.deletion
        banner @"Delete" inboxZeroLine # observed ) # updated (match { "Delete": const deleteOpened, "Keep": const keepMessages })
      fab @"Compose" {} "edit" # applied composeMessage
      ( menu @"Sort" $ RecordToVariant.do
        menuItem @"By sender" {}
        menuItem @"By subject" {}
        menuItem @"Unread first" {} ) # updated (match { "By sender": const <<< sortBySender, "By subject": const <<< sortBySubject, "Unread first": const <<< sortUnreadFirst })
    ) # mvu mondayMail
