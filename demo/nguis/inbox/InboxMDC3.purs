module InboxMDC3 (inboxMDC3) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import InboxViewModel (deletionPane, composeMessage, deleteOpened, bodyText, fromLine, highlighted, inboxZeroLine, keepMessages, messageLine, messageView, mondayMail, openMessage, requestDelete, sortBySender, sortBySubject, sortUnreadFirst, subjectLine, unreadLine)
import PUI (fold, joined, looped, observed, prefixed, unshown, with)
import PUI.Web (provided, text)
import PUI.Web.HTML (span)
import PUI.Web.MDC3 (body, bodyLarge, bodyMedium, bodySmall, button, dialog, fab, headlineSmall, iconButton, listOfAt', menu, menuItem, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

inboxMDC3 :: Effect Unit
inboxMDC3 =
  body $ Semigroupoid.do
    prefixed (bodySmall $ text unreadLine) $ RecordToVariant.do
      listOfAt' @"Message opened" @"id" @"messages" { selected: highlighted } ( span $ text messageLine ) # joined @"Message opened"
      ( prefixed ( RecordToRecord.do
        headlineSmall (text subjectLine)
        bodyMedium (text fromLine)
        bodyLarge (text bodyText) ) $ iconButton @"Delete message" "delete" ) # provided @"reading"
        @( reading :: { sender :: String, subject :: String, body :: String }
         , browsing :: {}
         ) messageView # joined @"Delete message"
      ( Semigroupoid.do
        ( dialog "Delete the last message?" $ RecordToVariant.do
          button @"Delete"
          button @"Keep" ) # provided @"confirming"
            @( confirming :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {}, open :: {} ] }, deletion :: [ silent :: {}, confirming :: {} ] }
             , silent :: {}
             ) deletionPane
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
