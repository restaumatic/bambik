module InboxMDC2 (inboxMDC2) where

import Prelude (identity, (#), ($), (<<<), Unit, const)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import InboxLogic (composeMessage, confirmingDelete, deleteOpened, inboxZeroLine, keepMessages, mailboxRows, messageCountText, mondayMail, openMessage, openedMessage, requestDelete, sortBySender, sortBySubject, sortUnreadFirst, unreadCountText, unreadMark)
import PUI (asCase, completed, displayed, forCase, mvu, observed, atCase, projected, tapped, toCase, updated)
import PUI.Web.HTML (body, provided, span, staticText, text)
import PUI.Web.MDC2 (banner, body1, body2, button, caption, card, dialog, elevation20, fab, headline6, iconButton, listOf, menu, menuItem)
import QualifiedDo.Semigroupoid as Semigroupoid

inboxMDC2 :: Effect Unit
inboxMDC2 =
  body $
    elevation20 $
      card { caption: "Inbox" } $ ( Semigroupoid.do
          caption ( RecordToRecord.do
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
                  headline6 (text @"subject")
                  body2 RecordToRecord.do
                    staticText "From: "
                    text @"sender"
                  body1 (text @"body")) # tapped
              iconButton { icon: "delete", label: "Delete message" } # asCase @"clicked" @"deleteRequested") # provided openedMessage # updated (match { deleteRequested: const requestDelete })
          ( Semigroupoid.do
              ( dialog { title: "Delete the last message?" } $ RecordToVariant.do
                  button { label: "Delete" } # asCase @"clicked" @"emptied"
                  button { label: "Keep" } # asCase @"clicked" @"kept") # provided confirmingDelete
              VariantToVariant.do
                banner # forCase @"event" @"emptied" (const inboxZeroLine) # observed
                identity # atCase @"kept" # toCase @"kept" identity) # updated (match { emptied: const <<< deleteOpened, kept: const <<< keepMessages })
          fab { icon: "edit", label: "Compose" } # asCase @"clicked" @"compose" # updated (match { compose: const <<< composeMessage })
          ( menu { label: "Sort" } $ RecordToVariant.do
              menuItem { label: "By sender" } # asCase @"clicked" @"bySender"
              menuItem { label: "By subject" } # asCase @"clicked" @"bySubject"
              menuItem { label: "Unread first" } # asCase @"clicked" @"unreadFirst") # updated (match { bySender: const <<< sortBySender, bySubject: const <<< sortBySubject, unreadFirst: const <<< sortUnreadFirst })
      ) # mvu mondayMail
