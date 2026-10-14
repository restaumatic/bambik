module Inbox.MDC3 (inbox) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Inbox (composeMessage, deleteOpened, bodyText, fromLine, highlighted, inboxState, keepMessages, messageLine, messageView, mondayMail, openMessage, requestDelete, sortBySender, sortBySubject, sortUnreadFirst, subjectLine, unreadLine)
import PUI (fold, joined, looped, with)
import PUI.Web (inCase, provided, providedAt, staticText, text)
import PUI.Web.HTML (span)
import PUI.Web.MDC3 (body, bodySmall, headlineSmall, bodyMedium, bodyLarge, button, dialog, fab, iconButton, listOfAt', menu, menuItem)
import QualifiedDo.Semigroupoid as Semigroupoid

inbox :: Effect Unit
inbox =
  body $ Semigroupoid.do
    bodySmall $ text unreadLine
    ( bodyLarge $ staticText "Inbox zero!" ) # inCase @"empty" @( empty :: {}, filled :: {} ) inboxState
    RecordToVariant.do
      ( listOfAt' @"Message opened" @"id" @"messages" { selected: highlighted } $ span $ text messageLine ) # joined
      ( Semigroupoid.do
        headlineSmall $ text subjectLine
        bodyMedium $ text fromLine
        bodyLarge $ text bodyText
        iconButton @"Delete message" "delete" ) # provided @"reading" @( reading :: {}, browsing :: {} ) messageView
      ( dialog "Delete the last message?" $ RecordToVariant.do
        button @"Delete"
        button @"Keep" ) # providedAt @"confirming" @"deletion"
      fab @"Compose" "edit"
      menu "Sort" $ RecordToVariant.do
        menuItem @"By sender"
        menuItem @"By subject"
        menuItem @"Unread first"
    VariantToRecord.do
      fold @"Message opened" openMessage
      fold @"Delete message" requestDelete
      fold @"Delete" deleteOpened
      fold @"Keep" keepMessages
      fold @"Compose" composeMessage
      fold @"By sender" sortBySender
      fold @"By subject" sortBySubject
      fold @"Unread first" sortUnreadFirst
  # looped
    @( messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {}, open :: {} ] }
     , deletion :: [ silent :: {}, confirming :: {} ]
     ) # with mondayMail
