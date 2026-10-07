module InboxViewModel (deletionPane, mondayMail, unreadLine, highlighted, messageLine, messageView, subjectLine, fromLine, bodyText, inboxZeroLine, openMessage, requestDelete, deleteOpened, keepMessages, composeMessage, sortBySender, sortBySubject, sortUnreadFirst, messageOpenedLine, deleteRequestedLine, messageDeletedLine, messageKeptLine, messageComposedLine, sortedBySenderLine, sortedBySubjectLine, sortedUnreadFirstLine) where

import Prelude ((<<<), (<>), (+), (==), comparing, const, map, max, not, show)

import Data.Array (filter, find, foldl, length, snoc, sortBy)
import Data.Maybe (Maybe(..))
import Data.Variant (match)

mondayMail :: { deletion :: [ confirming :: {}, silent :: {} ], messages :: Array { body :: String, id :: Int, sender :: String, status :: [ open :: {}, read :: {}, unread :: {} ], subject :: String } }
mondayMail =
  { messages:
    [ { id: 1, sender: "Alice Kowalska", subject: "Quarterly report ready", body: "The Q2 numbers are in - revenue up 12%, see the attached sheet before Friday's review.", status: .unread {} }
    , { id: 2, sender: "Bob Nowak", subject: "Lunch on Thursday?", body: "The new ramen place near the office finally opened. Noon works for me.", status: .read {} }
    , { id: 3, sender: "Carol Wu", subject: "Code review request", body: "Could you take a look at the profunctor refactor branch? Two files, mostly renames.", status: .unread {} }
    ]
  , deletion: .silent {}
  }

unreadLine :: { deletion :: [ confirming :: {}, silent :: {} ], messages :: Array { body :: String, id :: Int, sender :: String, status :: [ open :: {}, read :: {}, unread :: {} ], subject :: String } } -> String
unreadLine { messages } = show (length (filter (isUnread <<< _.status) messages)) <> " unread of " <> show (length messages) <> " messages"

highlighted :: { body :: String, id :: Int, sender :: String, status :: [ open :: {}, read :: {}, unread :: {} ], subject :: String } -> Boolean
highlighted { status } = match { unread: const true, read: const false, open: const true } status

messageLine :: { body :: String, id :: Int, sender :: String, status :: [ open :: {}, read :: {}, unread :: {} ], subject :: String } -> String
messageLine { sender, subject, status } = match { unread: const "● ", read: const "", open: const "" } status <> sender <> " — " <> subject

messageView :: { deletion :: [ confirming :: {}, silent :: {} ], messages :: Array { body :: String, id :: Int, sender :: String, status :: [ open :: {}, read :: {}, unread :: {} ], subject :: String } } -> [ browsing :: {}, reading :: { body :: String, sender :: String, subject :: String } ]
messageView { messages } = case find (isOpen <<< _.status) messages of
  Just message -> .reading { sender: message.sender, subject: message.subject, body: message.body }
  Nothing -> .browsing {}

subjectLine :: { body :: String, sender :: String, subject :: String } -> String
subjectLine { subject } = subject

fromLine :: { body :: String, sender :: String, subject :: String } -> String
fromLine { sender } = "From: " <> sender

bodyText :: { body :: String, sender :: String, subject :: String } -> String
bodyText { body } = body

inboxZeroLine :: { deletion :: [ confirming :: {}, silent :: {} ], messages :: Array { body :: String, id :: Int, sender :: String, status :: [ open :: {}, read :: {}, unread :: {} ], subject :: String } } -> String
inboxZeroLine _ = "Inbox zero!"

openMessage :: { event :: Int, model :: { deletion :: [ confirming :: {}, silent :: {} ], messages :: Array { body :: String, id :: Int, sender :: String, status :: [ open :: {}, read :: {}, unread :: {} ], subject :: String } } } -> { deletion :: [ confirming :: {}, silent :: {} ], messages :: Array { body :: String, id :: Int, sender :: String, status :: [ open :: {}, read :: {}, unread :: {} ], subject :: String } }
openMessage { event: id, model: m@{ messages } } = m { messages = map (\g -> if g.id == id then g { status = .open {} } else if isOpen g.status then g { status = .read {} } else g) messages }

requestDelete :: { event :: { body :: String, sender :: String, subject :: String }, model :: { deletion :: [ confirming :: {}, silent :: {} ], messages :: Array { body :: String, id :: Int, sender :: String, status :: [ open :: {}, read :: {}, unread :: {} ], subject :: String } } } -> { deletion :: [ confirming :: {}, silent :: {} ], messages :: Array { body :: String, id :: Int, sender :: String, status :: [ open :: {}, read :: {}, unread :: {} ], subject :: String } }
requestDelete { model: m@{ messages } } = if length messages == 1 then m { deletion = .confirming {} } else deleteOpened m

deleteOpened :: { deletion :: [ confirming :: {}, silent :: {} ], messages :: Array { body :: String, id :: Int, sender :: String, status :: [ open :: {}, read :: {}, unread :: {} ], subject :: String } } -> { deletion :: [ confirming :: {}, silent :: {} ], messages :: Array { body :: String, id :: Int, sender :: String, status :: [ open :: {}, read :: {}, unread :: {} ], subject :: String } }
deleteOpened m@{ messages } = m { messages = filter (not <<< isOpen <<< _.status) messages, deletion = .silent {} }

keepMessages :: { deletion :: [ confirming :: {}, silent :: {} ], messages :: Array { body :: String, id :: Int, sender :: String, status :: [ open :: {}, read :: {}, unread :: {} ], subject :: String } } -> { deletion :: [ confirming :: {}, silent :: {} ], messages :: Array { body :: String, id :: Int, sender :: String, status :: [ open :: {}, read :: {}, unread :: {} ], subject :: String } }
keepMessages m = m { deletion = .silent {} }

composeMessage :: { deletion :: [ confirming :: {}, silent :: {} ], messages :: Array { body :: String, id :: Int, sender :: String, status :: [ open :: {}, read :: {}, unread :: {} ], subject :: String } } -> { deletion :: [ confirming :: {}, silent :: {} ], messages :: Array { body :: String, id :: Int, sender :: String, status :: [ open :: {}, read :: {}, unread :: {} ], subject :: String } }
composeMessage m@{ messages } = m { messages = snoc messages { id, sender: "Me", subject: "Draft " <> show id, body: "A freshly composed note, still looking for its recipient.", status: .unread {} } }
  where
  id = foldl max 0 (map _.id messages) + 1

sortBySender :: { deletion :: [ confirming :: {}, silent :: {} ], messages :: Array { body :: String, id :: Int, sender :: String, status :: [ open :: {}, read :: {}, unread :: {} ], subject :: String } } -> { deletion :: [ confirming :: {}, silent :: {} ], messages :: Array { body :: String, id :: Int, sender :: String, status :: [ open :: {}, read :: {}, unread :: {} ], subject :: String } }
sortBySender m@{ messages } = m { messages = sortBy (comparing _.sender) messages }

sortBySubject :: { deletion :: [ confirming :: {}, silent :: {} ], messages :: Array { body :: String, id :: Int, sender :: String, status :: [ open :: {}, read :: {}, unread :: {} ], subject :: String } } -> { deletion :: [ confirming :: {}, silent :: {} ], messages :: Array { body :: String, id :: Int, sender :: String, status :: [ open :: {}, read :: {}, unread :: {} ], subject :: String } }
sortBySubject m@{ messages } = m { messages = sortBy (comparing _.subject) messages }

sortUnreadFirst :: { deletion :: [ confirming :: {}, silent :: {} ], messages :: Array { body :: String, id :: Int, sender :: String, status :: [ open :: {}, read :: {}, unread :: {} ], subject :: String } } -> { deletion :: [ confirming :: {}, silent :: {} ], messages :: Array { body :: String, id :: Int, sender :: String, status :: [ open :: {}, read :: {}, unread :: {} ], subject :: String } }
sortUnreadFirst m@{ messages } = m { messages = sortBy (comparing (readRank <<< _.status)) messages }

isUnread :: [ unread :: {}, read :: {}, open :: {} ] -> Boolean
isUnread = match { unread: const true, read: const false, open: const false }

isOpen :: [ unread :: {}, read :: {}, open :: {} ] -> Boolean
isOpen = match { unread: const false, read: const false, open: const true }

readRank :: [ unread :: {}, read :: {}, open :: {} ] -> Int
readRank = match { unread: const 0, read: const 1, open: const 1 }

deletionPane :: { deletion :: [ confirming :: {}, silent :: {} ], messages :: Array { body :: String, id :: Int, sender :: String, status :: [ open :: {}, read :: {}, unread :: {} ], subject :: String } } -> [ confirming :: { deletion :: [ confirming :: {}, silent :: {} ], messages :: Array { body :: String, id :: Int, sender :: String, status :: [ open :: {}, read :: {}, unread :: {} ], subject :: String } }, silent :: {} ]
deletionPane m = match { confirming: \_ -> .confirming m, silent: \_ -> .silent {} } m.deletion

messageOpenedLine :: { event :: Int, model :: { deletion :: [ confirming :: {}, silent :: {} ], messages :: Array { body :: String, id :: Int, sender :: String, status :: [ open :: {}, read :: {}, unread :: {} ], subject :: String } } } -> String
messageOpenedLine { event: id, model: { messages } } = case find (\g -> g.id == id) messages of
  Just message -> "Opened “" <> message.subject <> "” from " <> message.sender
  Nothing -> "Opened a message"

deleteRequestedLine :: { event :: { body :: String, sender :: String, subject :: String }, model :: { deletion :: [ confirming :: {}, silent :: {} ], messages :: Array { body :: String, id :: Int, sender :: String, status :: [ open :: {}, read :: {}, unread :: {} ], subject :: String } } } -> String
deleteRequestedLine { event: { subject }, model: { messages } } = if length messages == 1 then "Deleting the last message, “" <> subject <> "”" else "Deleted “" <> subject <> "”"

messageDeletedLine :: { deletion :: [ confirming :: {}, silent :: {} ], messages :: Array { body :: String, id :: Int, sender :: String, status :: [ open :: {}, read :: {}, unread :: {} ], subject :: String } } -> String
messageDeletedLine { messages } = case find (isOpen <<< _.status) messages of
  Just message -> "Deleted “" <> message.subject <> "”"
  Nothing -> "Deleted the last message"

messageKeptLine :: { deletion :: [ confirming :: {}, silent :: {} ], messages :: Array { body :: String, id :: Int, sender :: String, status :: [ open :: {}, read :: {}, unread :: {} ], subject :: String } } -> String
messageKeptLine { messages } = case find (isOpen <<< _.status) messages of
  Just message -> "Kept “" <> message.subject <> "”"
  Nothing -> "Kept the last message"

messageComposedLine :: { deletion :: [ confirming :: {}, silent :: {} ], messages :: Array { body :: String, id :: Int, sender :: String, status :: [ open :: {}, read :: {}, unread :: {} ], subject :: String } } -> String
messageComposedLine { messages } = "Composed Draft " <> show (foldl max 0 (map _.id messages) + 1)

sortedBySenderLine :: { deletion :: [ confirming :: {}, silent :: {} ], messages :: Array { body :: String, id :: Int, sender :: String, status :: [ open :: {}, read :: {}, unread :: {} ], subject :: String } } -> String
sortedBySenderLine { messages } = "Sorted " <> show (length messages) <> " messages by sender"

sortedBySubjectLine :: { deletion :: [ confirming :: {}, silent :: {} ], messages :: Array { body :: String, id :: Int, sender :: String, status :: [ open :: {}, read :: {}, unread :: {} ], subject :: String } } -> String
sortedBySubjectLine { messages } = "Sorted " <> show (length messages) <> " messages by subject"

sortedUnreadFirstLine :: { deletion :: [ confirming :: {}, silent :: {} ], messages :: Array { body :: String, id :: Int, sender :: String, status :: [ open :: {}, read :: {}, unread :: {} ], subject :: String } } -> String
sortedUnreadFirstLine { messages } = "Sorted " <> show (length messages) <> " messages, unread first"
