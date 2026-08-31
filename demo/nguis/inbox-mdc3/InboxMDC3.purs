module InboxMDC3 (inboxMDC3) where

import Prelude (identity, (#), ($), (<<<), Unit, const)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import InboxLogic (composeMessage, deleteOpened, deletionOf, highlighted, inboxZeroLine, keepMessages, mailboxRows, messageView, mondayMail, openMessage, presentInbox, readState, requestDelete, sortBySender, sortBySubject, sortUnreadFirst)
import PUI (applied, atCase, forCases, mvu, observed, settled, toCase, updated, with)
import PUI.Web.HTML (shownWhen, shown, body, provided, span, staticText, text)
import PUI.Web.MDC3 (snackbar, bodyLarge, bodyMedium, button, bodySmall, card, dialog, elevation5, fab, headlineSmall, iconButton, listOf, menu, menuItem)
import QualifiedDo.Category as Category

inboxMDC3 :: Effect Unit
inboxMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          ( bodySmall $ RecordToRecord.do
              text @"unreadCountText"
              staticText " unread of "
              text @"messageCountText"
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
                  headlineSmall (text @"subject")
                  bodyMedium RecordToRecord.do
                    staticText "From: "
                    text @"sender"
                  bodyLarge (text @"body")) # shown
              iconButton @"Delete message" { icon: "delete" } ) # provided @"reading" messageView # updated (match { "Delete message": const requestDelete })
          ( Category.do
              ( dialog { title: "Delete the last message?" } $ RecordToVariant.do
                  button @"Delete" {} # with {}
                  button @"Keep" {} # with {} ) # provided @"confirming" deletionOf
              VariantToVariant.do
                snackbar # forCases (match { "Delete": const inboxZeroLine }) # observed
                identity # atCase @"Keep" # toCase @"Keep" identity ) # updated (match { "Delete": const deleteOpened, "Keep": const keepMessages })
          fab @"Compose" { icon: "edit" } # applied composeMessage
          ( menu { label: "Sort" } $ RecordToVariant.do
              menuItem @"By sender" {}
              menuItem @"By subject" {}
              menuItem @"Unread first" {} ) # updated (match { "By sender": const <<< sortBySender, "By subject": const <<< sortBySubject, "Unread first": const <<< sortUnreadFirst })
      ) # settled presentInbox # mvu mondayMail
