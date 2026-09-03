module InboxMDC3 (inboxMDC3) where

import Prelude (identity, (#), ($), (<<<), Unit, const)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import InboxLogic (composeMessage, deleteOpened, deletionOf, bodyText, fromLine, highlighted, inboxZeroLine, keepMessages, mailboxRows, messageLine, messageView, mondayMail, openMessage, readState, requestDelete, sortBySender, sortBySubject, sortUnreadFirst, subjectLine, unreadLine)
import PUI (applied, atCase, forCase, mvu, observed, toCase, updated, with)
import PUI.Web.HTML (shownWhen, shown, body, provided, span, staticText, text)
import PUI.Web.MDC3 (snackbar, bodyLarge, bodyMedium, button, bodySmall, card, dialog, elevation5, fab, headlineSmall, iconButton, listOf, menu, menuItem)
import QualifiedDo.Category as Category

inboxMDC3 :: Effect Unit
inboxMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          ( bodySmall $ text unreadLine ) # shown
          listOf { selected: highlighted } mailboxRows
            ( span $ Category.do
                (staticText "● ") # shownWhen @"unread" readState
                text messageLine # shown ) # toCase @"opened" _.id # updated (match { opened: openMessage })
          ( Category.do
              headlineSmall (text subjectLine) # shown
              bodyMedium (text fromLine) # shown
              bodyLarge (text bodyText) # shown
              iconButton @"Delete message" { icon: "delete" } ) # provided @"reading" messageView # updated (match { "Delete message": const requestDelete })
          ( Category.do
              ( dialog { title: "Delete the last message?" } $ RecordToVariant.do
                  button @"Delete" {} # with {}
                  button @"Keep" {} # with {} ) # provided @"confirming" deletionOf
              VariantToVariant.do
                snackbar # forCase @"Delete" (const inboxZeroLine) # observed
                identity # atCase @"Keep" # toCase @"Keep" identity ) # updated (match { "Delete": const deleteOpened, "Keep": const keepMessages })
          fab @"Compose" { icon: "edit" } # applied composeMessage
          ( menu { label: "Sort" } $ RecordToVariant.do
              menuItem @"By sender" {}
              menuItem @"By subject" {}
              menuItem @"Unread first" {} ) # updated (match { "By sender": const <<< sortBySender, "By subject": const <<< sortBySubject, "Unread first": const <<< sortUnreadFirst })
      ) # mvu mondayMail
