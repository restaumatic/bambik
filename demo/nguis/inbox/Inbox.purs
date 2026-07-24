module Inbox (inbox) where

import Prelude ((#), ($), (+), (<<<), (<>), (==), (/=), (||), Unit, comparing, const, identity, map, not, show)

import Data.Array (filter, find, length, snoc, sortBy)
import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap, rmap)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (asCase, completed, forCase, mvu, onCase, projection, PUI, tapped, toCase, updates)
import PUI.HTML (body, provided, span, text)
import PUI.MDC (banner, body1, body2, button, caption, card, dialog, elevation20, fab, headline6, iconButton, listOf, menu, menuItem)
import PUI.Web (Web)
import QualifiedDo.Semigroupoid as Semigroupoid

inbox :: Effect Unit
inbox =
  body $
    elevation20 $
      card { caption: "Inbox" } $ ( Semigroupoid.do
          caption text # projection unreadLine # completed
          listOf { selected: _.attention } (span text # projection _.line) # lcmap mailboxRows # rmap _.id # toCase @"opened" # updates (match { opened: openMessage })
          ( Semigroupoid.do
              ( RecordToRecord.do
                  headline6 text # projection subjectLine
                  body2 text # projection senderLine
                  body1 text # projection bodyLine) # tapped
              iconButton { icon: "delete", label: "Delete message" } # asCase @"deleteRequested") # provided # lcmap openedMessage # updates (match { deleteRequested: const requestDelete })
          ( Semigroupoid.do
              ( dialog { title: "Delete the last message?" } $ RecordToVariant.do
                  button { label: "Delete" } # asCase @"emptied"
                  button { label: "Keep" } # asCase @"kept") # provided # lcmap confirmingDelete
              VariantToVariant.do
                banner # forCase @"emptied" # lcmap (match { emptied: .emptied <<< emptiedNote }) # tapped
                (identity :: PUI Web { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, confirming :: Boolean, nextId :: Int } { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, confirming :: Boolean, nextId :: Int }) # onCase @"kept" # toCase @"kept") # updates (match { emptied: const <<< deleteOpened, kept: const <<< keepMessages })
          ( RecordToVariant.do
              fab { icon: "edit", label: "Compose" } # asCase @"compose"
              menu { label: "Sort" } RecordToVariant.do
                menuItem { label: "By sender" } # asCase @"bySender"
                menuItem { label: "By subject" } # asCase @"bySubject"
                menuItem { label: "Unread first" } # asCase @"unreadFirst") # updates (match { compose: const <<< composeMessage, bySender: const <<< sortBySender, bySubject: const <<< sortBySubject, unreadFirst: const <<< sortUnreadFirst })
      ) # mvu mondayMail

mondayMail :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, confirming :: Boolean, nextId :: Int }
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

unreadLine :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, confirming :: Boolean, nextId :: Int } -> String
unreadLine m = show (length (filter (\g -> not g.read) m.messages)) <> " unread of " <> show (length m.messages) <> " messages"

mailboxRows :: forall r s. { messages :: Array { id :: Int, sender :: String, subject :: String, read :: Boolean | s }, opened :: Maybe Int | r } -> Array { id :: Int, line :: String, attention :: Boolean }
mailboxRows m = m.messages # map \g ->
  { id: g.id
  , line: (if g.read then "" else "● ") <> g.sender <> " — " <> g.subject
  , attention: not g.read || m.opened == Just g.id
  }

openMessage :: forall r s. Int -> { messages :: Array { id :: Int, read :: Boolean | s }, opened :: Maybe Int | r } -> { messages :: Array { id :: Int, read :: Boolean | s }, opened :: Maybe Int | r }
openMessage id m = m { messages = map (\g -> if g.id == id then g { read = true } else g) m.messages, opened = Just id }

openedMessage :: forall r s. { messages :: Array { id :: Int | s }, opened :: Maybe Int | r } -> Maybe { id :: Int | s }
openedMessage m = find (\g -> Just g.id == m.opened) m.messages

senderLine :: { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean } -> String
senderLine g = "From: " <> g.sender

subjectLine :: { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean } -> String
subjectLine g = g.subject

bodyLine :: { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean } -> String
bodyLine g = g.body

lastMessage :: forall a r. { messages :: Array a | r } -> Boolean
lastMessage m = length m.messages == 1

requestDelete :: forall r s. { messages :: Array { id :: Int | s }, opened :: Maybe Int, confirming :: Boolean | r } -> { messages :: Array { id :: Int | s }, opened :: Maybe Int, confirming :: Boolean | r }
requestDelete m = if lastMessage m then m { confirming = true } else deleteOpened m

confirmingDelete :: forall r. { confirming :: Boolean | r } -> Maybe { confirming :: Boolean | r }
confirmingDelete m = if m.confirming then Just m else Nothing

deleteOpened :: forall r s. { messages :: Array { id :: Int | s }, opened :: Maybe Int, confirming :: Boolean | r } -> { messages :: Array { id :: Int | s }, opened :: Maybe Int, confirming :: Boolean | r }
deleteOpened m = m { messages = filter (\g -> Just g.id /= m.opened) m.messages, opened = Nothing, confirming = false }

keepMessages :: forall r. { confirming :: Boolean | r } -> { confirming :: Boolean | r }
keepMessages m = m { confirming = false }

emptiedNote :: forall a. a -> String
emptiedNote _ = "Inbox zero!"

composeMessage :: forall r. { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, nextId :: Int | r } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, nextId :: Int | r }
composeMessage m = m
  { messages = snoc m.messages { id: m.nextId, sender: "Me", subject: "Draft " <> show m.nextId, body: "A freshly composed note, still looking for its recipient.", read: false }
  , nextId = m.nextId + 1
  }

sortBySender :: forall r s. { messages :: Array { sender :: String | s } | r } -> { messages :: Array { sender :: String | s } | r }
sortBySender m = m { messages = sortBy (comparing _.sender) m.messages }

sortBySubject :: forall r s. { messages :: Array { subject :: String | s } | r } -> { messages :: Array { subject :: String | s } | r }
sortBySubject m = m { messages = sortBy (comparing _.subject) m.messages }

sortUnreadFirst :: forall r s. { messages :: Array { read :: Boolean | s } | r } -> { messages :: Array { read :: Boolean | s } | r }
sortUnreadFirst m = m { messages = sortBy (comparing _.read) m.messages }
