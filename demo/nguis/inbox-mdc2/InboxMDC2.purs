module InboxMDC2 (inboxMDC2) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import InboxViewModel (deletionPane, composeMessage, deleteOpened, deleteRequestedLine, bodyText, fromLine, highlighted, inboxZeroLine, keepMessages, messageComposedLine, messageDeletedLine, messageKeptLine, messageLine, messageOpenedLine, messageView, mondayMail, openMessage, requestDelete, sortBySender, sortBySubject, sortUnreadFirst, sortedBySenderLine, sortedBySubjectLine, sortedUnreadFirstLine, subjectLine, unreadLine)
import PUI (fold, joined, looped, observed, with)
import PUI.Web (provided, shown, text)
import PUI.Web.HTML (span)
import PUI.Web.MDC2 (banner, body, body1, body2, button, caption, dialog, fab, headline6, iconButton, listOf, menu, menuItem, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

inboxMDC2 :: Effect Unit
inboxMDC2 =
  body $
    Semigroupoid.do
      ( caption $ text unreadLine ) # shown
      RecordToVariant.do
        listOf @"Message opened" @"id" { selected: highlighted } _.messages ( span $ text messageLine # shown ) # joined @"Message opened"
        ( Semigroupoid.do
          headline6 (text subjectLine) # shown
          body2 (text fromLine) # shown
          body1 (text bodyText) # shown
          iconButton @"Delete message" {} "delete" ) # provided @"reading" @( reading :: { sender :: String, subject :: String, body :: String }, browsing :: {} ) messageView # joined @"Delete message"
        ( Semigroupoid.do
          ( dialog @"Delete the last message?" $ RecordToVariant.do
            button @"Delete" {}
            button @"Keep" {} ) # provided @"confirming" @( confirming :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {}, open :: {} ] } , deletion :: [ silent :: {}, confirming :: {} ] }, silent :: {} ) deletionPane
          banner @"Delete" inboxZeroLine # observed )
        fab @"Compose" {} "edit"
        ( menu @"Sort" $ RecordToVariant.do
          menuItem @"By sender" {}
          menuItem @"By subject" {}
          menuItem @"Unread first" {} )
      VariantToRecord.do
        snackbar @"Message opened" messageOpenedLine # fold openMessage
        snackbar @"Delete message" deleteRequestedLine # fold requestDelete
        snackbar @"Delete" messageDeletedLine # fold deleteOpened
        snackbar @"Keep" messageKeptLine # fold keepMessages
        snackbar @"Compose" messageComposedLine # fold composeMessage
        snackbar @"By sender" sortedBySenderLine # fold sortBySender
        snackbar @"By subject" sortedBySubjectLine # fold sortBySubject
        snackbar @"Unread first" sortedUnreadFirstLine # fold sortUnreadFirst
    # looped
      @( messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {}, open :: {} ] }
       , deletion :: [ silent :: {}, confirming :: {} ]
       ) # with mondayMail
