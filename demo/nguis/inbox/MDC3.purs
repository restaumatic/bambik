module Inbox.MDC3 (inbox) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Inbox (composeMessage, deleteOpened, bodyText, deletionLine, fromLine, highlighted, inboxState, messageLine, messageView, mondayMail, openMessage, sortBySender, sortBySubject, sortUnreadFirst, subjectLine, unreadLine)
import PUI (fold, joined, looped, with)
import PUI.Web (inCase, provided, staticText, text)
import PUI.Web.HTML (span)
import PUI.Web.MDC3 (body, bodySmall, headlineSmall, bodyMedium, bodyLarge, confirmed, fab, iconButton, listOfAt', menu, menuItem)
import QualifiedDo.Semigroupoid as Semigroupoid

inbox :: Effect Unit
inbox =
  body $ Semigroupoid.do
    bodySmall $ text unreadLine
    ( bodyLarge $ staticText "Inbox zero!" ) # inCase @"empty" inboxState
    RecordToVariant.do
      ( listOfAt' @"Message opened" @"id" @"messages" { selected: highlighted } $ span $ text messageLine ) # joined @"Message opened"
      ( Semigroupoid.do
        headlineSmall $ text subjectLine
        bodyMedium $ text fromLine
        bodyLarge $ text bodyText
        iconButton @"Delete message" "delete" ) # provided @"reading" messageView
      fab @"Compose" "edit"
      menu "Sort" $ RecordToVariant.do
        menuItem @"By sender"
        menuItem @"By subject"
        menuItem @"Unread first"
    confirmed @"Delete message" $ bodyMedium $ text deletionLine
    VariantToRecord.do
      fold @"Message opened" openMessage
      fold @"Delete message" deleteOpened
      fold @"Compose" composeMessage
      fold @"By sender" sortBySender
      fold @"By subject" sortBySubject
      fold @"Unread first" sortUnreadFirst
  # looped # with mondayMail
