module InboxMDC2 (inboxMDC2) where

import Prelude (identity, (#), ($), (+), (<<<), (<>), (==), (/=), (||), Unit, comparing, const, identity, map, not, show)

import Data.Array (filter, find, length, snoc, sortBy)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (PUI, asCase, completed, constantly, displayed, forCase, forField, forValue, mvu, onCase, projection, tapped, toCase, updates)
import PUI.HTML (body, provided, span, staticText, text)
import PUI.Web (Web)
import PUI.MDC2 (banner, body1, body2, button, caption, card, dialog, elevation20, fab, headline6, iconButton, listOf, menu, menuItem)
import QualifiedDo.Semigroupoid as Semigroupoid

inboxMDC2 :: Effect Unit
inboxMDC2 =
  body $
    elevation20 $
      card { caption: "Inbox" } $ ( Semigroupoid.do
          caption ( RecordToRecord.do
              text # projection unreadCountText
              staticText " unread of "
              text # projection messageCountText
              staticText " messages" ) # completed
          listOf { selected: _.attention } mailboxRows
            ( span $ Semigroupoid.do
                staticText "● " # provided unreadMark # displayed
                ( RecordToRecord.do
                    text # forValue # forField @"sender"
                    staticText " — "
                    text # forValue # forField @"subject" ) # displayed
            ) # toCase @"opened" _.id # updates (match { opened: openMessage })
          ( Semigroupoid.do
              ( RecordToRecord.do
                  headline6 text # forValue # forField @"subject"
                  body2 RecordToRecord.do
                    staticText "From: "
                    text # forValue # forField @"sender"
                  body1 text # forValue # forField @"body") # tapped
              iconButton { icon: "delete", label: "Delete message" } # asCase @"deleteRequested") # provided openedMessage # updates (match { deleteRequested: const requestDelete })
          ( Semigroupoid.do
              ( dialog { title: "Delete the last message?" } $ RecordToVariant.do
                  button { label: "Delete" } # asCase @"emptied"
                  button { label: "Keep" } # asCase @"kept") # provided confirmingDelete
              VariantToVariant.do
                inboxZeroBanner # tapped # onCase @"emptied" # toCase @"emptied" identity
                identity # onCase @"kept" # toCase @"kept" identity) # updates (match { emptied: const <<< deleteOpened, kept: const <<< keepMessages })
          fab { icon: "edit", label: "Compose" } # asCase @"compose" # updates (match { compose: const <<< composeMessage })
          ( menu { label: "Sort" } RecordToVariant.do
              menuItem { label: "By sender" } # asCase @"bySender"
              menuItem { label: "By subject" } # asCase @"bySubject"
              menuItem { label: "Unread first" } # asCase @"unreadFirst") # updates (match { bySender: const <<< sortBySender, bySubject: const <<< sortBySubject, unreadFirst: const <<< sortUnreadFirst })
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
unreadCountText { messages } = show (length (filter (\g -> not g.read) messages))

messageCountText :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean } } -> String
messageCountText { messages } = show (length messages)

mailboxRows :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, confirming :: Boolean, nextId :: Int } -> Array { id :: Int, sender :: String, subject :: String, read :: Boolean, attention :: Boolean }
mailboxRows { messages, opened } = messages # map \g ->
  { id: g.id
  , sender: g.sender
  , subject: g.subject
  , read: g.read
  , attention: not g.read || opened == Just g.id
  }

unreadMark :: { read :: Boolean } -> Maybe {}
unreadMark { read } = if read then Nothing else Just {}

openMessage :: Int -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int }
openMessage id m@{ messages } = m { messages = map (\g -> if g.id == id then g { read = true } else g) messages, opened = Just id }

openedMessage :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, confirming :: Boolean, nextId :: Int } -> Maybe { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }
openedMessage { messages, opened } = find (\g -> Just g.id == opened) messages

lastMessage :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean } } -> Boolean
lastMessage { messages } = length messages == 1

requestDelete :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, confirming :: Boolean } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, confirming :: Boolean }
requestDelete m@{ messages } = if lastMessage { messages } then m { confirming = true } else deleteOpened m

confirmingDelete :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, confirming :: Boolean } -> Maybe { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, confirming :: Boolean }
confirmingDelete m@{ confirming } = if confirming then Just m else Nothing

deleteOpened :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, confirming :: Boolean } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, confirming :: Boolean }
deleteOpened m@{ messages, opened } = m { messages = filter (\g -> Just g.id /= opened) messages, opened = Nothing, confirming = false }

keepMessages :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, confirming :: Boolean } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, confirming :: Boolean }
keepMessages m = m { confirming = false }

inboxZeroBanner :: PUI Web {} {}
inboxZeroBanner = banner # forCase @"emptied" identity # constantly (.emptied "Inbox zero!")

composeMessage :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, nextId :: Int } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, nextId :: Int }
composeMessage m@{ messages, nextId } = m
  { messages = snoc messages { id: nextId, sender: "Me", subject: "Draft " <> show nextId, body: "A freshly composed note, still looking for its recipient.", read: false }
  , nextId = nextId + 1
  }

sortBySender :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean } } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean } }
sortBySender m@{ messages } = m { messages = sortBy (comparing _.sender) messages }

sortBySubject :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean } } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean } }
sortBySubject m@{ messages } = m { messages = sortBy (comparing _.subject) messages }

sortUnreadFirst :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean } } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean } }
sortUnreadFirst m@{ messages } = m { messages = sortBy (comparing _.read) messages }
