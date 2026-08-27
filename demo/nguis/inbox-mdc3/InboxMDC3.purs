module InboxMDC3 (inboxMDC3) where

import Prelude (identity, (#), ($), (<<<), Unit, const)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import InboxLogic (composeMessage, confirmingDelete, deleteOpened, inboxZeroLine, keepMessages, mailboxRows, messageCountText, mondayMail, openMessage, openedMessage, requestDelete, sortBySender, sortBySubject, sortUnreadFirst, unreadCountText, unreadMark)
import PUI (applied, forCase, mvu, observed, atCase, projected, toCase, updated)
import PUI.Web.HTML (shownWhen, shown, body, provided, span, staticText, text)
import PUI.Web.MDC3 (snackbar, bodyLarge, bodyMedium, button, bodySmall, card, dialog, elevation5, fab, headlineSmall, iconButton, listOf, menu, menuItem)
import QualifiedDo.Category as Category

inboxMDC3 :: Effect Unit
inboxMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          ( bodySmall $ RecordToRecord.do
              text @"unreadCount" # projected unreadCountText
              staticText " unread of "
              text @"messageCount" # projected messageCountText
              staticText " messages" ) # shown
          listOf { selected: _.attention } mailboxRows
            ( span $ Category.do
                (staticText "● ") # shownWhen unreadMark
                ( RecordToRecord.do
                    text @"sender"
                    staticText " — "
                    text @"subject" ) # shown ) # toCase @"opened" _.id # updated (match { opened: openMessage })
          ( Category.do
              ( RecordToRecord.do
                  headlineSmall (text @"subject")
                  bodyMedium RecordToRecord.do
                    staticText "From: "
                    text @"sender"
                  bodyLarge (text @"body")) # shown
              iconButton @"Delete message" { icon: "delete" } ) # provided openedMessage # updated (match { "Delete message": const requestDelete })
          ( Category.do
              ( dialog { title: "Delete the last message?" } $ RecordToVariant.do
                  button @"Delete" {}
                  button @"Keep" {} ) # provided confirmingDelete
              VariantToVariant.do
                snackbar # forCase @"Delete" (const inboxZeroLine) # observed
                identity # atCase @"Keep" # toCase @"Keep" identity ) # updated (match { "Delete": const <<< deleteOpened, "Keep": const <<< keepMessages })
          fab @"Compose" { icon: "edit" } # applied composeMessage
          ( menu { label: "Sort" } $ RecordToVariant.do
              menuItem @"By sender" {}
              menuItem @"By subject" {}
              menuItem @"Unread first" {} ) # updated (match { "By sender": const <<< sortBySender, "By subject": const <<< sortBySubject, "Unread first": const <<< sortUnreadFirst })
      ) # mvu mondayMail
