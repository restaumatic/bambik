module Inbox (Inbox, Message, Reading, mondayMail, unreadLine, highlighted, messageLine, messageView, subjectLine, fromLine, bodyText, deletionLine, inboxState, openMessage, deleteOpened, composeMessage, sortBySender, sortBySubject, sortUnreadFirst) where

import Prelude ((<<<), (<>), (+), (==), comparing, const, map, max, not, show)

import Data.Array (filter, find, foldl, length, snoc, sortBy)
import Data.Maybe (Maybe(..), maybe)
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
unreadLine { messages } = show (length (filter (isUnread <<< _.status) messages)) <> " unread of " <> show (length messages) <> " messages"

highlighted :: Message -> Boolean
highlighted { status } = match { unread: const true, read: const false, open: const true } status

messageLine :: Message -> String
messageLine { sender, subject, status } = match { unread: const "● ", read: const "", open: const "" } status <> sender <> " — " <> subject

messageView :: Inbox -> [ browsing :: {}, reading :: {} ]
messageView { messages } = case find (isOpen <<< _.status) messages of
  Just _ -> .reading {}
  Nothing -> .browsing {}

subjectLine :: Inbox -> String
subjectLine = openMessageLine _.subject

fromLine :: Inbox -> String
fromLine = openMessageLine \{ sender } -> "From: " <> sender

bodyText :: Inbox -> String
bodyText = openMessageLine _.body

deletionLine :: Inbox -> String
deletionLine = openMessageLine \{ sender, subject } -> "“" <> subject <> "” from " <> sender <> " will be gone for good."

openMessageLine :: (Message -> String) -> Inbox -> String
openMessageLine line { messages } = maybe "" line (find (isOpen <<< _.status) messages)

inboxState :: Inbox -> [ empty :: {}, filled :: {} ]
inboxState { messages } = if length messages == 0 then .empty {} else .filled {}

openMessage :: { event :: Int, model :: Inbox } -> Inbox
openMessage { event: id, model: m@{ messages } } = m { messages = map (\g -> if g.id == id then g { status = .open {} } else if isOpen g.status then g { status = .read {} } else g) messages }

deleteOpened :: Inbox -> Inbox
deleteOpened m@{ messages } = m { messages = filter (not <<< isOpen <<< _.status) messages }

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
