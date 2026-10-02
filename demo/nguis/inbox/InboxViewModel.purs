module InboxViewModel (mondayMail, unreadLine, highlighted, messageLine, messageView, subjectLine, fromLine, bodyText, inboxZeroLine, openMessage, requestDelete, deleteOpened, keepMessages, composeMessage, sortBySender, sortBySubject, sortUnreadFirst) where

import Prelude ((<<<), (<>), (+), (==), comparing, const, map, max, not, show)

import Data.Array (filter, find, foldl, length, snoc, sortBy)
import Data.Maybe (Maybe(..))
import Data.Variant (match)

mondayMail :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {}, open :: {} ] }, deletion :: [ silent :: {}, confirming :: {} ] }
mondayMail =
  { messages:
    [ { id: 1, sender: "Alice Kowalska", subject: "Quarterly report ready", body: "The Q2 numbers are in - revenue up 12%, see the attached sheet before Friday's review.", status: .unread {} }
    , { id: 2, sender: "Bob Nowak", subject: "Lunch on Thursday?", body: "The new ramen place near the office finally opened. Noon works for me.", status: .read {} }
    , { id: 3, sender: "Carol Wu", subject: "Code review request", body: "Could you take a look at the profunctor refactor branch? Two files, mostly renames.", status: .unread {} }
    ]
  , deletion: .silent {}
  }

unreadLine :: forall r1. { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {}, open :: {} ] } | r1 } -> String
unreadLine { messages } = show (length (filter (isUnread <<< _.status) messages)) <> " unread of " <> show (length messages) <> " messages"

highlighted :: forall r1. { status :: [ unread :: {}, read :: {}, open :: {} ] | r1 } -> Boolean
highlighted { status } = match { unread: const true, read: const false, open: const true } status

messageLine :: forall r1. { sender :: String, subject :: String, status :: [ unread :: {}, read :: {}, open :: {} ] | r1 } -> String
messageLine { sender, subject, status } = match { unread: const "● ", read: const "", open: const "" } status <> sender <> " — " <> subject

messageView :: forall r1. { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {}, open :: {} ] } | r1 } -> [ reading :: { sender :: String, subject :: String, body :: String }, browsing :: {} ]
messageView { messages } = case find (isOpen <<< _.status) messages of
  Just message -> .reading { sender: message.sender, subject: message.subject, body: message.body }
  Nothing -> .browsing {}

subjectLine :: forall r1. { sender :: String, subject :: String, body :: String | r1 } -> String
subjectLine { subject } = subject

fromLine :: forall r1. { sender :: String, subject :: String, body :: String | r1 } -> String
fromLine { sender } = "From: " <> sender

bodyText :: forall r1. { sender :: String, subject :: String, body :: String | r1 } -> String
bodyText { body } = body

inboxZeroLine :: forall r1. { | r1 } -> String
inboxZeroLine _ = "Inbox zero!"

openMessage :: forall r1. Int -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {}, open :: {} ] } | r1 } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {}, open :: {} ] } | r1 }
openMessage id m@{ messages } = m { messages = map (\g -> if g.id == id then g { status = .open {} } else if isOpen g.status then g { status = .read {} } else g) messages }

requestDelete :: forall r1. { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {}, open :: {} ] }, deletion :: [ silent :: {}, confirming :: {} ] | r1 } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {}, open :: {} ] }, deletion :: [ silent :: {}, confirming :: {} ] | r1 }
requestDelete m@{ messages } = if length messages == 1 then m { deletion = .confirming {} } else deleteOpened m

deleteOpened :: forall r1. { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {}, open :: {} ] }, deletion :: [ silent :: {}, confirming :: {} ] | r1 } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {}, open :: {} ] }, deletion :: [ silent :: {}, confirming :: {} ] | r1 }
deleteOpened m@{ messages } = m { messages = filter (not <<< isOpen <<< _.status) messages, deletion = .silent {} }

keepMessages :: forall r1. { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {}, open :: {} ] }, deletion :: [ silent :: {}, confirming :: {} ] | r1 } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {}, open :: {} ] }, deletion :: [ silent :: {}, confirming :: {} ] | r1 }
keepMessages m = m { deletion = .silent {} }

composeMessage :: forall r1. { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {}, open :: {} ] } | r1 } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {}, open :: {} ] } | r1 }
composeMessage m@{ messages } = m { messages = snoc messages { id, sender: "Me", subject: "Draft " <> show id, body: "A freshly composed note, still looking for its recipient.", status: .unread {} } }
  where
  id = foldl max 0 (map _.id messages) + 1

sortBySender :: forall r1. { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {}, open :: {} ] } | r1 } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {}, open :: {} ] } | r1 }
sortBySender m@{ messages } = m { messages = sortBy (comparing _.sender) messages }

sortBySubject :: forall r1. { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {}, open :: {} ] } | r1 } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {}, open :: {} ] } | r1 }
sortBySubject m@{ messages } = m { messages = sortBy (comparing _.subject) messages }

sortUnreadFirst :: forall r1. { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {}, open :: {} ] } | r1 } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {}, open :: {} ] } | r1 }
sortUnreadFirst m@{ messages } = m { messages = sortBy (comparing (readRank <<< _.status)) messages }

isUnread :: [ unread :: {}, read :: {}, open :: {} ] -> Boolean
isUnread = match { unread: const true, read: const false, open: const false }

isOpen :: [ unread :: {}, read :: {}, open :: {} ] -> Boolean
isOpen = match { unread: const false, read: const false, open: const true }

readRank :: [ unread :: {}, read :: {}, open :: {} ] -> Int
readRank = match { unread: const 0, read: const 1, open: const 1 }
