module Inbox.MDC3 (inbox) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Inbox (composeMessage, deleteMessage, deletionLine, fromLine, highlighted, inboxState, messageLine, mondayMail, openMessage, openMessages, sortBySender, sortBySubject, sortUnreadFirst, unreadLine)
import PUI (fold, foldInto, gatheredOf, looped, stepped, with)
import PUI.Web (inCase, staticText, text, textAt)
import PUI.Web.HTML (div, span)
import PUI.Web.MDC3 (body, bodyLarge, bodyMedium, bodySmall, confirmed, fab, headlineSmall, iconButton, listOfAt', menu, menuItem)
import QualifiedDo.Semigroupoid as Semigroupoid

inbox :: Effect Unit
inbox =
  body $ Semigroupoid.do
    bodySmall $ text unreadLine
    ( bodyLarge $ staticText "Inbox zero!" ) # inCase @"empty" inboxState
    ( Semigroupoid.do
      RecordToVariant.do
        listOfAt' @"Message opened" @"id" @"messages" { selected: highlighted } $ span $ text messageLine
        div $
          ( Semigroupoid.do
            headlineSmall $ textAt @"subject"
            bodyMedium $ text fromLine
            bodyLarge $ textAt @"body"
            iconButton @"Delete message" "delete" ) # gatheredOf @"id" openMessages
        fab @"Compose" "edit"
        menu "Sort" $ RecordToVariant.do
          menuItem @"By sender"
          menuItem @"By subject"
          menuItem @"Unread first"
      confirmed @"Delete message" $ bodyMedium $ text deletionLine
      VariantToRecord.do
        foldInto @"Message opened" openMessage
        foldInto @"Delete message" deleteMessage
        fold @"Compose" composeMessage
        fold @"By sender" sortBySender
        fold @"By subject" sortBySubject
        fold @"Unread first" sortUnreadFirst ) # stepped
  # looped # with mondayMail
