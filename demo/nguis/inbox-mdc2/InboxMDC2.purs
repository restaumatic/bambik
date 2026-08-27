module InboxMDC2 (inboxMDC2) where

import Prelude (identity, (#), ($), (<<<), Unit, const)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import InboxLogic (composeMessage, deleteOpened, deletionOf, highlighted, inboxZeroLine, keepMessages, mailboxRows, messageCountText, messageView, mondayMail, openMessage, readState, requestDelete, sortBySender, sortBySubject, sortUnreadFirst, unreadCountText)
import PUI (applied, forCase, mvu, observed, atCase, projected, toCase, updated, with)
import PUI.Web.HTML (shownWhen, shown, body, provided, span, staticText, text)
import PUI.Web.MDC2 (banner, body1, body2, button, caption, card, dialog, elevation20, fab, headline6, iconButton, listOf, menu, menuItem)
import QualifiedDo.Category as Category

inboxMDC2 :: Effect Unit
inboxMDC2 =
  body $
    elevation20 $
      card $ ( Category.do
          ( caption $ RecordToRecord.do
              text @"unreadCount" # projected unreadCountText
              staticText " unread of "
              text @"messageCount" # projected messageCountText
              staticText " messages" ) # shown
          listOf { selected: highlighted } mailboxRows
            ( span $ Category.do
                (staticText "● ") # shownWhen @"unread" readState
                ( RecordToRecord.do
                    text @"sender"
                    staticText " — "
                    text @"subject" ) # shown ) # toCase @"opened" _.id # updated (match { opened: openMessage })
          ( Category.do
              ( RecordToRecord.do
                  headline6 (text @"subject")
                  body2 RecordToRecord.do
                    staticText "From: "
                    text @"sender"
                  body1 (text @"body")) # shown
              iconButton @"Delete message" { icon: "delete" } ) # provided @"reading" messageView # updated (match { "Delete message": const requestDelete })
          ( Category.do
              ( dialog { title: "Delete the last message?" } $ RecordToVariant.do
                  button @"Delete" {} # with {}
                  button @"Keep" {} # with {} ) # provided @"confirming" deletionOf
              VariantToVariant.do
                banner # forCase @"Delete" (const inboxZeroLine) # observed
                identity # atCase @"Keep" # toCase @"Keep" identity ) # updated (match { "Delete": const deleteOpened, "Keep": const keepMessages })
          fab @"Compose" { icon: "edit" } # applied composeMessage
          ( menu { label: "Sort" } $ RecordToVariant.do
              menuItem @"By sender" {}
              menuItem @"By subject" {}
              menuItem @"Unread first" {} ) # updated (match { "By sender": const <<< sortBySender, "By subject": const <<< sortBySubject, "Unread first": const <<< sortUnreadFirst })
      ) # mvu mondayMail
