module Inbox (inbox) where

import Prelude ((#), ($), (+), (<>), (==), (/=), (||), (>>>), Unit, comparing, identity, map, not, show)

import Data.Array (filter, find, length, snoc, sortBy)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Profunctor (lcmap, rmap)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (PUI, asCase, completed, forCase, forValue, mvu, onCase, projection, tapped, updates)
import PUI.HTML (attr, body, clWhen, div, shownWhen, span, text)
import PUI.MDC (banner, body1, body2, button, caption, card, dialog, elevation20, fab, headline6, iconButton, listOf, menu, menuItem)
import PUI.Web (Web)
import QualifiedDo.Semigroupoid as Semigroupoid

inbox :: Effect Unit
inbox =
  body $
    elevation20 $
      card { caption: "Inbox" } $ ( Semigroupoid.do
          caption (text # projection unreadLine # forValue) # completed
          listOf { selected: _.attention } (span (text # projection _.line # forValue))
            # lcmap mailboxRows # rmap (\e -> .opened e.id :: [ opened :: Int ]) # updates (match { opened: openMessage })
          shownWhen messageOpen
            ( RecordToRecord.do
                headline6 (text # projection subjectLine # forValue)
                body2 (text # projection senderLine # forValue)
                body1 (text # projection bodyLine # forValue)
            ) # tapped
          shownWhen messageOpen (iconButton { icon: "delete", label: "Delete message" })
            # updates (match { clicked: \m _ -> requestDelete m })
          ( Semigroupoid.do
              ( dialog { title: "Delete the last message?" } $ div >>> attr "style" "display: flex; gap: 16px;" $ RecordToVariant.do
                  button { label: "Delete" } # asCase @"emptied"
                  button { label: "Keep" } # asCase @"kept"
              ) # clWhen _.confirming "mdc-dialog--open"
              VariantToVariant.do
                banner # forCase @"emptied" # lcmap (match { emptied: \m -> .emptied (emptiedNote m) :: [ emptied :: String ] }) # tapped
                (identity :: PUI Web Mailbox Mailbox) # onCase @"kept" # rmap (\m -> .kept m :: [ kept :: Mailbox ])
          ) # updates (match { emptied: \m _ -> deleteOpened m, kept: \m _ -> keepMessages m })
          div >>> attr "style" "display: flex; gap: 16px; align-items: center; margin-top: 8px;" $ ( RecordToVariant.do
              fab { icon: "edit", label: "Compose" } # asCase @"compose"
              menu { label: "Sort" } RecordToVariant.do
                menuItem { label: "By sender" } # asCase @"bySender"
                menuItem { label: "By subject" } # asCase @"bySubject"
                menuItem { label: "Unread first" } # asCase @"unreadFirst"
          ) # updates (match { compose: \m _ -> composeMessage m, bySender: \m _ -> sortBySender m, bySubject: \m _ -> sortBySubject m, unreadFirst: \m _ -> sortUnreadFirst m })
      ) # mvu mondayMail

type Message = { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }

type Mailbox = { messages :: Array Message, opened :: Maybe Int, confirming :: Boolean, nextId :: Int }

mondayMail :: Mailbox
mondayMail =
  { messages:
      [ { id: 1, sender: "Alice Kowalska", subject: "Quarterly report ready", body: "The Q2 numbers are in - revenue up 12%, see the attached sheet before Friday's review.", read: false }
      , { id: 2, sender: "Bob Nowak", subject: "Lunch on Thursday?", body: "The new ramen place near the office finally opened. Noon works for me.", read: true }
      , { id: 3, sender: "Carol Wu", subject: "Code review request", body: "Could you take a look at the profunctor refactor branch? Two files, mostly renames.", read: false }
      ]
  , opened: Nothing
  , confirming: false
  , nextId: 4
  }

unreadLine :: Mailbox -> String
unreadLine m = show (length (filter (\g -> not g.read) m.messages)) <> " unread of " <> show (length m.messages) <> " messages"

mailboxRows :: Mailbox -> Array { id :: Int, line :: String, attention :: Boolean }
mailboxRows m = m.messages # map \g ->
  { id: g.id
  , line: (if g.read then "" else "● ") <> g.sender <> " — " <> g.subject
  , attention: not g.read || m.opened == Just g.id
  }

openMessage :: Int -> Mailbox -> Mailbox
openMessage id m = m { messages = map (\g -> if g.id == id then g { read = true } else g) m.messages, opened = Just id }

messageOpen :: Mailbox -> Boolean
messageOpen m = isJust m.opened

openedMessage :: Mailbox -> Maybe Message
openedMessage m = find (\g -> Just g.id == m.opened) m.messages

senderLine :: Mailbox -> String
senderLine m = maybe "" (\g -> "From: " <> g.sender) (openedMessage m)

subjectLine :: Mailbox -> String
subjectLine m = maybe "" _.subject (openedMessage m)

bodyLine :: Mailbox -> String
bodyLine m = maybe "" _.body (openedMessage m)

lastMessage :: Mailbox -> Boolean
lastMessage m = length m.messages == 1

requestDelete :: Mailbox -> Mailbox
requestDelete m = if lastMessage m then m { confirming = true } else deleteOpened m

deleteOpened :: Mailbox -> Mailbox
deleteOpened m = m { messages = filter (\g -> Just g.id /= m.opened) m.messages, opened = Nothing, confirming = false }

keepMessages :: Mailbox -> Mailbox
keepMessages m = m { confirming = false }

emptiedNote :: Mailbox -> String
emptiedNote _ = "Inbox zero!"

composeMessage :: Mailbox -> Mailbox
composeMessage m = m
  { messages = snoc m.messages { id: m.nextId, sender: "Me", subject: "Draft " <> show m.nextId, body: "A freshly composed note, still looking for its recipient.", read: false }
  , nextId = m.nextId + 1
  }

sortBySender :: Mailbox -> Mailbox
sortBySender m = m { messages = sortBy (comparing _.sender) m.messages }

sortBySubject :: Mailbox -> Mailbox
sortBySubject m = m { messages = sortBy (comparing _.subject) m.messages }

sortUnreadFirst :: Mailbox -> Mailbox
sortUnreadFirst m = m { messages = sortBy (comparing _.read) m.messages }
