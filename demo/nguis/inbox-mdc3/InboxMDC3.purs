module InboxMDC3 (inboxMDC3) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import InboxViewModel (deletionPane, composeMessage, deleteOpened, bodyText, fromLine, highlighted, inboxZeroLine, keepMessages, messageComposedLine, messageDeletedLine, messageLine, messageView, mondayMail, openMessage, requestDelete, sortBySender, sortBySubject, sortUnreadFirst, subjectLine, unreadLine)
import PUI (blankStatus, fold, joined, looped, observed, with)
import PUI.Pipeline as Pipeline
import PUI.Web (provided, text)
import PUI.Web.HTML (span)
import PUI.Web.MDC3 (body, bodyLarge, bodyMedium, bodySmall, button, dialog, fab, headlineSmall, iconButton, listOfAt, menu, menuItem, snackbar)

inboxMDC3 :: Effect Unit
inboxMDC3 =
  body $ Pipeline.do
    bodySmall $ text unreadLine
    RecordToVariant.do
      listOfAt @"Message opened" @"id" @"messages" { selected: highlighted } ( span $ text messageLine ) # joined @"Message opened"
      ( Pipeline.do
        headlineSmall (text subjectLine)
        bodyMedium (text fromLine)
        bodyLarge (text bodyText)
        iconButton @"Delete message" {} "delete" ) # provided @"reading"
          @( reading :: { sender :: String, subject :: String, body :: String }
           , browsing :: {}
           ) messageView # joined @"Delete message"
      ( Pipeline.do
        ( dialog "Delete the last message?" $ RecordToVariant.do
          button @"Delete" {}
          button @"Keep" {} ) # provided @"confirming"
            @( confirming :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {}, open :: {} ] }, deletion :: [ silent :: {}, confirming :: {} ] }
             , silent :: {}
             ) deletionPane
        snackbar @"Delete" inboxZeroLine # observed )
      fab @"Compose" {} "edit"
      ( menu "Sort" $ RecordToVariant.do
        menuItem @"By sender" {}
        menuItem @"By subject" {}
        menuItem @"Unread first" {} )
    VariantToRecord.do
      blankStatus @"Message opened" # fold openMessage
      blankStatus @"Delete message" # fold requestDelete
      snackbar @"Delete" messageDeletedLine # fold deleteOpened
      blankStatus @"Keep" # fold keepMessages
      snackbar @"Compose" messageComposedLine # fold composeMessage
      blankStatus @"By sender" # fold sortBySender
      blankStatus @"By subject" # fold sortBySubject
      blankStatus @"Unread first" # fold sortUnreadFirst
  # looped
    @( messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {}, open :: {} ] }
     , deletion :: [ silent :: {}, confirming :: {} ]
     ) # with mondayMail
