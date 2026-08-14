module InboxMDC3 (inboxMDC3) where

import Prelude (identity, (#), ($), (<<<), Unit, const)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import InboxLogic (composeMessage, confirmingDelete, deleteOpened, inboxZeroLine, keepMessages, mailboxRows, messageCountText, mondayMail, openMessage, openedMessage, requestDelete, sortBySender, sortBySubject, sortUnreadFirst, unreadCountText, unreadMark)
import PUI (asCase, completed, displayed, forCase, mvu, observed, atCase, projected, tapped, toCase, updated)
import PUI.Web.HTML (body, provided, span, staticText, text)
import PUI.Web.MDC3 (snackbar, bodyLarge, bodyMedium, button, bodySmall, card, dialog, elevation5, fab, headlineSmall, iconButton, listOf, menu, menuItem)
import QualifiedDo.Semigroupoid as Semigroupoid

inboxMDC3 :: Effect Unit
inboxMDC3 =
  body $
    elevation5 $
      card { caption: "Inbox" } $ ( Semigroupoid.do
          bodySmall ( RecordToRecord.do
              text @"value" # projected @"value" unreadCountText
              staticText " unread of "
              text @"value" # projected @"value" messageCountText
              staticText " messages" ) # completed
          listOf { selected: _.attention } mailboxRows
            ( span $ Semigroupoid.do
                staticText "● " # provided unreadMark # displayed
                ( RecordToRecord.do
                    text @"sender"
                    staticText " — "
                    text @"subject" ) # displayed ) # toCase @"opened" _.id # updated (match { opened: openMessage })
          ( Semigroupoid.do
              ( RecordToRecord.do
                  headlineSmall (text @"subject")
                  bodyMedium RecordToRecord.do
                    staticText "From: "
                    text @"sender"
                  bodyLarge (text @"body")) # tapped
              iconButton { icon: "delete", label: "Delete message" } # asCase @"clicked" @"deleteRequested") # provided openedMessage # updated (match { deleteRequested: const requestDelete })
          ( Semigroupoid.do
              ( dialog { title: "Delete the last message?" } $ RecordToVariant.do
                  button { label: "Delete" } # asCase @"clicked" @"emptied"
                  button { label: "Keep" } # asCase @"clicked" @"kept") # provided confirmingDelete
              VariantToVariant.do
                snackbar # forCase @"event" @"emptied" (const inboxZeroLine) # observed
                identity # atCase @"kept" # toCase @"kept" identity) # updated (match { emptied: const <<< deleteOpened, kept: const <<< keepMessages })
          fab { icon: "edit", label: "Compose" } # asCase @"clicked" @"compose" # updated (match { compose: const <<< composeMessage })
          ( menu { label: "Sort" } $ RecordToVariant.do
              menuItem { label: "By sender" } # asCase @"clicked" @"bySender"
              menuItem { label: "By subject" } # asCase @"clicked" @"bySubject"
              menuItem { label: "Unread first" } # asCase @"clicked" @"unreadFirst") # updated (match { bySender: const <<< sortBySender, bySubject: const <<< sortBySubject, unreadFirst: const <<< sortUnreadFirst })
      ) # mvu mondayMail
