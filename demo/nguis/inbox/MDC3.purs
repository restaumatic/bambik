module Inbox.MDC3 (inbox) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Inbox (composeMessage, deleteOpened, bodyText, fromLine, highlighted, inboxZeroLine, keepMessages, messageLine, messageView, mondayMail, openMessage, requestDelete, sortBySender, sortBySubject, sortUnreadFirst, subjectLine, unreadLine)
import PUI (fold, joined, looped, observed, unshown, with)
import PUI.Web (provided, providedAt, text)
import PUI.Web.HTML (span)
import PUI.Web.MDC3 (snackbar, body, bodySmall, headlineSmall, bodyMedium, bodyLarge, button, dialog, fab, iconButton, listOfAt', menu, menuItem)
import QualifiedDo.Semigroupoid as Semigroupoid

inbox :: Effect Unit
inbox =
  body $ Semigroupoid.do
    bodySmall $ text unreadLine
    RecordToVariant.do
      listOfAt' @"Message opened" @"id" @"messages" { selected: highlighted } ( span $ text messageLine ) # joined @"Message opened"
      ( Semigroupoid.do
        headlineSmall (text subjectLine)
        bodyMedium (text fromLine)
        bodyLarge (text bodyText)
        iconButton @"Delete message" "delete" ) # provided @"reading" @( reading :: {}, browsing :: {} ) messageView
      ( Semigroupoid.do
        ( dialog "Delete the last message?" $ RecordToVariant.do
          button @"Delete"
          button @"Keep" ) # providedAt @"confirming" @"deletion"
        snackbar @"Delete" inboxZeroLine # observed )
      fab @"Compose" "edit"
      ( menu "Sort" $ RecordToVariant.do
        menuItem @"By sender"
        menuItem @"By subject"
        menuItem @"Unread first" )
    VariantToRecord.do
      unshown @"Message opened" # fold openMessage
      unshown @"Delete message" # fold requestDelete
      unshown @"Delete" # fold deleteOpened
      unshown @"Keep" # fold keepMessages
      unshown @"Compose" # fold composeMessage
      unshown @"By sender" # fold sortBySender
      unshown @"By subject" # fold sortBySubject
      unshown @"Unread first" # fold sortUnreadFirst
  # looped
    @( messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {}, open :: {} ] }
     , deletion :: [ silent :: {}, confirming :: {} ]
     ) # with mondayMail
