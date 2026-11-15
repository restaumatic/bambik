module Inbox (Inbox, Message, Reading, composeMessage, deleteMessage, deletionLine, fromLine, highlighted, inboxState, messageLine, mondayMail, openMessage, openMessages, sortBySender, sortBySubject, sortUnreadFirst, unreadLine) where

import Prelude ((<<<), (<>), (+), (/=), (==), comparing, const, map, max, otherwise, show)

import Data.Array (filter, foldl, length, null, snoc, sortBy)
import Data.Variant (match)

type Reading = [ open :: {}, read :: {}, unread :: {} ]

type Message = { body :: String, id :: Int, sender :: String, status :: Reading, subject :: String }

type Inbox = { messages :: Array Message }

mondayMail :: Inbox
mondayMail =
  { messages:
    [ { id: 1, sender: "Alice Kowalska", subject: "Quarterly report ready", body: "The Q2 numbers are in - revenue up 12%, see the attached sheet before Friday's review.", status: .unread {} }
    , { id: 2, sender: "Bob Nowak", subject: "Lunch on Thursday?", body: "The new ramen place near the office finally opened. Noon works for me.", status: .read {} }
    , { id: 3, sender: "Carol Wu", subject: "Code review request", body: "Could you take a look at the profunctor refactor branch? Two files, mostly renames.", status: .unread {} }
    ]
  }

unreadLine :: Inbox -> String
unreadLine { messages } = show (length (filter (isUnread <<< _.status) messages)) <> " unread of " <> received
  where
  received = case length messages of
    1 -> "1 message"
    n -> show n <> " messages"

highlighted :: Message -> Boolean
highlighted { status } = match { unread: const true, read: const false, open: const true } status

messageLine :: Message -> String
messageLine { sender, subject, status } = match { unread: const "● ", read: const "", open: const "" } status <> sender <> " — " <> subject

openMessages :: Inbox -> Array Message
openMessages { messages } = filter (isOpen <<< _.status) messages

fromLine :: Message -> String
fromLine { sender } = "From: " <> sender

deletionLine :: { event :: Message, model :: Inbox } -> String
deletionLine { event: { sender, subject } } = "“" <> subject <> "” from " <> sender <> " will be gone for good."

inboxState :: Inbox -> [ empty :: {}, filled :: {} ]
inboxState { messages } = if null messages then .empty {} else .filled {}

openMessage :: { event :: Int, model :: Inbox } -> Inbox
openMessage { event: id, model: m@{ messages } } = m { messages = map opened messages }
  where
  opened g
    | g.id == id = g { status = .open {} }
    | isOpen g.status = g { status = .read {} }
    | otherwise = g

deleteMessage :: { event :: Message, model :: Inbox } -> Inbox
deleteMessage { event: { id }, model: m@{ messages } } = m { messages = filter (\g -> g.id /= id) messages }

composeMessage :: Inbox -> Inbox
composeMessage m@{ messages } = m { messages = snoc messages { id, sender: "Me", subject: "Draft " <> show id, body: "A freshly composed note, still looking for its recipient.", status: .unread {} } }
  where
  id = foldl max 0 (map _.id messages) + 1

sortBySender :: Inbox -> Inbox
sortBySender m@{ messages } = m { messages = sortBy (comparing _.sender) messages }

sortBySubject :: Inbox -> Inbox
sortBySubject m@{ messages } = m { messages = sortBy (comparing _.subject) messages }

sortUnreadFirst :: Inbox -> Inbox
sortUnreadFirst m@{ messages } = m { messages = sortBy (comparing (readRank <<< _.status)) messages }

isUnread :: Reading -> Boolean
isUnread = match { unread: const true, read: const false, open: const false }

isOpen :: Reading -> Boolean
isOpen = match { unread: const false, read: const false, open: const true }

readRank :: Reading -> Int
readRank = match { unread: const 0, read: const 1, open: const 1 }
