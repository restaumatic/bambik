module InboxMDC2 (inboxMDC2) where

import Prelude ((#), ($), (<<<), Unit, const)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import InboxLogic (composeMessage, deleteOpened, deletionOf, bodyText, fromLine, highlighted, inboxZeroLine, keepMessages, mailboxRows, messageLine, messageView, mondayMail, openMessage, requestDelete, sortBySender, sortBySubject, sortUnreadFirst, subjectLine, unreadLine)
import PUI (applied, forCase, mvu, observed, toCase, updated, with)
import PUI.Web.HTML (shown, provided, span, text)
import PUI.Web.MDC2 (banner, body, body1, body2, button, caption, card, dialog, elevation20, fab, headline6, iconButton, listOf, menu, menuItem)
import QualifiedDo.Category as Category

inboxMDC2 :: Effect Unit
inboxMDC2 =
  body $
    elevation20 $
      card $ ( Category.do
          ( caption $ text unreadLine ) # shown
          listOf { selected: highlighted } mailboxRows ( span $ text messageLine # shown ) # toCase @"opened" _.id # updated (match { opened: openMessage })
          ( Category.do
              headline6 (text subjectLine) # shown
              body2 (text fromLine) # shown
              body1 (text bodyText) # shown
              iconButton @"Delete message" { icon: "delete" } ) # provided @"reading" messageView # updated (match { "Delete message": const requestDelete })
          ( Category.do
              ( dialog { title: "Delete the last message?" } $ RecordToVariant.do
                  button @"Delete" {} # with {}
                  button @"Keep" {} # with {} ) # provided @"confirming" deletionOf
              banner # forCase @"Delete" (const inboxZeroLine) # observed ) # updated (match { "Delete": const deleteOpened, "Keep": const keepMessages })
          fab @"Compose" { icon: "edit" } # applied composeMessage
          ( menu { label: "Sort" } $ RecordToVariant.do
              menuItem @"By sender" {}
              menuItem @"By subject" {}
              menuItem @"Unread first" {} ) # updated (match { "By sender": const <<< sortBySender, "By subject": const <<< sortBySubject, "Unread first": const <<< sortUnreadFirst })
      ) # mvu mondayMail
