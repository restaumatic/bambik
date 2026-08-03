module InboxMDC3 (inboxMDC3) where

import Prelude (identity, (#), ($), (<<<), Unit, const)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import InboxLogic (composeMessage, confirmingDelete, deleteOpened, keepMessages, mailboxRows, messageCountText, mondayMail, openMessage, openedMessage, requestDelete, sortBySender, sortBySubject, sortUnreadFirst, unreadCountText, unreadMark)
import PUI (PUI, asCase, completed, constantly, displayed, forCase, forField, mvu, onCase, projected, tapped, toCase, updated)
import PUI.Web.HTML (body, provided, span, staticText, text)
import PUI.Web (Web)
import PUI.Web.MDC3 (snackbar, bodyLarge, bodyMedium, button, bodySmall, card, dialog, elevation5, fab, headlineSmall, iconButton, listOf, menu, menuItem)
import QualifiedDo.Semigroupoid as Semigroupoid

inboxMDC3 :: Effect Unit
inboxMDC3 =
  body $
    elevation5 $
      card { caption: "Inbox" } $ ( Semigroupoid.do
          bodySmall ( RecordToRecord.do
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
                  headlineSmall text # forField @"subject" identity
                  bodyMedium RecordToRecord.do
                    staticText "From: "
                    text # forField @"sender" identity
                  bodyLarge text # forField @"body" identity) # tapped
              iconButton { icon: "delete", label: "Delete message" } # asCase @"deleteRequested") # provided openedMessage # updated (match { deleteRequested: const requestDelete })
          ( Semigroupoid.do
              ( dialog { title: "Delete the last message?" } $ RecordToVariant.do
                  button { label: "Delete" } # asCase @"emptied"
                  button { label: "Keep" } # asCase @"kept") # provided confirmingDelete
              VariantToVariant.do
                inboxZeroBanner # tapped # onCase @"emptied" # toCase @"emptied" identity
                identity # onCase @"kept" # toCase @"kept" identity) # updated (match { emptied: const <<< deleteOpened, kept: const <<< keepMessages })
          fab { icon: "edit", label: "Compose" } # asCase @"compose" # updated (match { compose: const <<< composeMessage })
          ( menu { label: "Sort" } $ RecordToVariant.do
              menuItem { label: "By sender" } # asCase @"bySender"
              menuItem { label: "By subject" } # asCase @"bySubject"
              menuItem { label: "Unread first" } # asCase @"unreadFirst") # updated (match { bySender: const <<< sortBySender, bySubject: const <<< sortBySubject, unreadFirst: const <<< sortUnreadFirst })
      ) # mvu mondayMail

inboxZeroBanner :: PUI Web {} {}
inboxZeroBanner = snackbar # forCase @"emptied" identity # constantly (.emptied "Inbox zero!")
