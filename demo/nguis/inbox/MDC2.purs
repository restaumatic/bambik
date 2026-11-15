module Inbox.MDC2 (inbox) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import Inbox (composeMessage, deleteMessage, deletionLine, fromLine, highlighted, inboxState, messageLine, mondayMail, openMessage, openMessages, sortBySender, sortBySubject, sortUnreadFirst, unreadLine)
import PUI (fold, foldInto, gatheredOf, looped, stepped, with)
import PUI.Web (inCase, staticText, text, textAt)
import PUI.Web.HTML (div, span)
import PUI.Web.MDC2 (banner, body, body1, body2, caption, confirmed, fab, headline6, iconButton, listOfAt', menu, menuItem)
import QualifiedDo.Semigroupoid as Semigroupoid

inbox :: Effect Unit
inbox =
  body $ Semigroupoid.do
    caption $ text unreadLine
    ( banner $ staticText "Inbox zero!" ) # inCase @"empty" inboxState
    ( Semigroupoid.do
      RecordToVariant.do
        listOfAt' @"Message opened" @"id" @"messages" { selected: highlighted } $ span $ text messageLine
        div $
          ( Semigroupoid.do
            headline6 $ textAt @"subject"
            body2 $ text fromLine
            body1 $ textAt @"body"
            iconButton @"Delete message" "delete" ) # gatheredOf @"id" openMessages
        fab @"Compose" "edit"
        menu "Sort" $ RecordToVariant.do
          menuItem @"By sender"
          menuItem @"By subject"
          menuItem @"Unread first"
      confirmed @"Delete message" $ body2 $ text deletionLine
      VariantToRecord.do
        foldInto @"Message opened" openMessage
        foldInto @"Delete message" deleteMessage
        fold @"Compose" composeMessage
        fold @"By sender" sortBySender
        fold @"By subject" sortBySubject
        fold @"Unread first" sortUnreadFirst ) # stepped
  # looped # with mondayMail
