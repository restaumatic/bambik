module InboxMDC2 (inboxMDC2) where

import Prelude (identity, (#), ($), (<<<), Unit, const)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import InboxLogic (composeMessage, confirmingDelete, deleteOpened, inboxZeroLine, keepMessages, mailboxRows, messageCountText, mondayMail, openMessage, openedMessage, requestDelete, sortBySender, sortBySubject, sortUnreadFirst, unreadCountText, unreadMark)
import PUI (asCase, completed, displayed, forCase, forField, mvu, observed, onCase, projected, tapped, toCase, updated)
import PUI.Web.HTML (body, provided, span, staticText, text)
import PUI.Web.MDC2 (banner, body1, body2, button, caption, card, dialog, elevation20, fab, headline6, iconButton, listOf, menu, menuItem)
import QualifiedDo.Semigroupoid as Semigroupoid

inboxMDC2 :: Effect Unit
inboxMDC2 =
  body $
    elevation20 $
      card { caption: "Inbox" } $ ( Semigroupoid.do
          caption ( RecordToRecord.do
              text # projected unreadCountText
              staticText " unread of "
              text # projected messageCountText
              staticText " messages" ) # completed
          listOf { selected: _.attention } mailboxRows
            ( span $ Semigroupoid.do
                staticText "● " # provided unreadMark # displayed
                ( RecordToRecord.do
                    text # forField @"sender" identity
                    staticText " — "
                    text # forField @"subject" identity ) # displayed ) # toCase @"opened" _.id # updated (match { opened: openMessage })
          ( Semigroupoid.do
              ( RecordToRecord.do
                  headline6 text # forField @"subject" identity
                  body2 RecordToRecord.do
                    staticText "From: "
                    text # forField @"sender" identity
                  body1 text # forField @"body" identity) # tapped
              iconButton { icon: "delete", label: "Delete message" } # asCase @"deleteRequested") # provided openedMessage # updated (match { deleteRequested: const requestDelete })
          ( Semigroupoid.do
              ( dialog { title: "Delete the last message?" } $ RecordToVariant.do
                  button { label: "Delete" } # asCase @"emptied"
                  button { label: "Keep" } # asCase @"kept") # provided confirmingDelete
              VariantToVariant.do
                banner # forCase @"emptied" (const inboxZeroLine) # observed
                identity # onCase @"kept" # toCase @"kept" identity) # updated (match { emptied: const <<< deleteOpened, kept: const <<< keepMessages })
          fab { icon: "edit", label: "Compose" } # asCase @"compose" # updated (match { compose: const <<< composeMessage })
          ( menu { label: "Sort" } $ RecordToVariant.do
              menuItem { label: "By sender" } # asCase @"bySender"
              menuItem { label: "By subject" } # asCase @"bySubject"
              menuItem { label: "Unread first" } # asCase @"unreadFirst") # updated (match { bySender: const <<< sortBySender, bySubject: const <<< sortBySubject, unreadFirst: const <<< sortUnreadFirst })
      ) # mvu mondayMail
