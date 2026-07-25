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
import PUI (asCase, completed, displayed, forCase, forField, forValue, mvu, onCase, projection, tapped, toCase, updates)
import PUI.HTML (body, provided, span, staticText, text)
import PUI.MDC (banner, body1, body2, button, caption, card, dialog, elevation20, fab, headline6, iconButton, listOf, menu, menuItem)
import QualifiedDo.Semigroupoid as Semigroupoid

inbox :: Effect Unit
inbox =
  body $
    elevation20 $
      card { caption: "Inbox" } $ ( Semigroupoid.do
          caption ( RecordToRecord.do
              text # projection unreadCountText
              staticText " unread of "
              text # projection messageCountText
              staticText " messages" ) # completed
          listOf { selected: _.attention }
            ( span $ Semigroupoid.do
                staticText "● " # provided # lcmap unreadMark # displayed
                ( RecordToRecord.do
                    text # forValue # forField @"sender"
                    staticText " — "
                    text # forValue # forField @"subject" ) # displayed
            ) # lcmap mailboxRows # rmap _.id # toCase @"opened" # updates (match { opened: openMessage })
          ( Semigroupoid.do
              ( RecordToRecord.do
                  headline6 text # projection subjectLine
                  body2 RecordToRecord.do
                    staticText "From: "
                    text # forValue # forField @"sender"
                  body1 text # projection bodyLine) # tapped
              iconButton { icon: "delete", label: "Delete message" } # asCase @"deleteRequested") # provided # lcmap openedMessage # updates (match { deleteRequested: const requestDelete })
          ( Semigroupoid.do
              ( dialog { title: "Delete the last message?" } $ RecordToVariant.do
                  button { label: "Delete" } # asCase @"emptied"
                  button { label: "Keep" } # asCase @"kept") # provided # lcmap confirmingDelete
              VariantToVariant.do
                banner # forCase @"emptied" # lcmap emptiedNote # tapped # onCase @"emptied" # toCase @"emptied"
                identity # onCase @"kept" # toCase @"kept") # updates (match { emptied: const <<< deleteOpened, kept: const <<< keepMessages })
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

unreadCountText :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean } } -> String
unreadCountText m = show (length (filter (\g -> not g.read) m.messages))

messageCountText :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean } } -> String
messageCountText m = show (length m.messages)

mailboxRows :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, confirming :: Boolean, nextId :: Int } -> Array { id :: Int, sender :: String, subject :: String, read :: Boolean, attention :: Boolean }
mailboxRows m = m.messages # map \g ->
  { id: g.id
  , sender: g.sender
  , subject: g.subject
  , read: g.read
  , attention: not g.read || m.opened == Just g.id
  }

unreadMark :: { read :: Boolean } -> Maybe {}
unreadMark g = if g.read then Nothing else Just {}

openMessage :: Int -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int }
openMessage id m = m { messages = map (\g -> if g.id == id then g { read = true } else g) m.messages, opened = Just id }

openedMessage :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, confirming :: Boolean, nextId :: Int } -> Maybe { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }
openedMessage m = find (\g -> Just g.id == m.opened) m.messages

subjectLine :: { subject :: String } -> String
subjectLine g = g.subject

bodyLine :: { body :: String } -> String
bodyLine g = g.body

lastMessage :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, confirming :: Boolean, nextId :: Int } -> Boolean
lastMessage m = length m.messages == 1

requestDelete :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, confirming :: Boolean, nextId :: Int } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, confirming :: Boolean, nextId :: Int }
requestDelete m = if lastMessage m then m { confirming = true } else deleteOpened m

confirmingDelete :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, confirming :: Boolean, nextId :: Int } -> Maybe { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, confirming :: Boolean, nextId :: Int }
confirmingDelete m = if m.confirming then Just m else Nothing

deleteOpened :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, confirming :: Boolean, nextId :: Int } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, confirming :: Boolean, nextId :: Int }
deleteOpened m = m { messages = filter (\g -> Just g.id /= m.opened) m.messages, opened = Nothing, confirming = false }

keepMessages :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, confirming :: Boolean, nextId :: Int } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, confirming :: Boolean, nextId :: Int }
keepMessages m = m { confirming = false }

emptiedNote :: {} -> [ emptied :: String ]
emptiedNote _ = .emptied "Inbox zero!"

composeMessage :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, confirming :: Boolean, nextId :: Int } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, confirming :: Boolean, nextId :: Int }
composeMessage m = m
  { messages = snoc m.messages { id: m.nextId, sender: "Me", subject: "Draft " <> show m.nextId, body: "A freshly composed note, still looking for its recipient.", read: false }
  , nextId = m.nextId + 1
  }

sortBySender :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, confirming :: Boolean, nextId :: Int } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, confirming :: Boolean, nextId :: Int }
sortBySender m = m { messages = sortBy (comparing _.sender) m.messages }

sortBySubject :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, confirming :: Boolean, nextId :: Int } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, confirming :: Boolean, nextId :: Int }
sortBySubject m = m { messages = sortBy (comparing _.subject) m.messages }

sortUnreadFirst :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, confirming :: Boolean, nextId :: Int } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, confirming :: Boolean, nextId :: Int }
sortUnreadFirst m = m { messages = sortBy (comparing _.read) m.messages }
