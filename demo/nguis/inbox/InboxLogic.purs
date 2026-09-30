module InboxLogic (composeMessage, deleteOpened, deletionOf, bodyText, fromLine, highlighted, inboxZeroLine, keepMessages, mailboxRows, messageLine, mondayMail, messageView, openMessage, requestDelete, sortBySender, sortBySubject, sortUnreadFirst, subjectLine, unreadLine) where

import Prelude ((<<<), (<>), (#), (+), (==), (||), comparing, const, map, not, show)

import Data.Array (filter, find, length, snoc, sortBy)
import Data.Maybe (Maybe(..))
import Data.Variant (match)

mondayMail :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] }, opened :: [ message :: { id :: Int }, none :: {} ], deletion :: [ silent :: {}, confirming :: {} ], nextId :: Int }
mondayMail =
  { messages:
    [ { id: 1, sender: "Alice Kowalska", subject: "Quarterly report ready", body: "The Q2 numbers are in - revenue up 12%, see the attached sheet before Friday's review.", status: .unread {} }
    , { id: 2, sender: "Bob Nowak", subject: "Lunch on Thursday?", body: "The new ramen place near the office finally opened. Noon works for me.", status: .read {} }
    , { id: 3, sender: "Carol Wu", subject: "Code review request", body: "Could you take a look at the profunctor refactor branch? Two files, mostly renames.", status: .unread {} }
    ]
  , opened: .none {}
  , deletion: .silent {}
  , nextId: 4
  }

unreadLine :: forall r1. { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] } | r1 } -> String
unreadLine { messages } = show (length (filter (isUnread <<< _.status) messages)) <> " unread of " <> show (length messages) <> " messages"

mailboxRows :: forall r1. { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] }, opened :: [ message :: { id :: Int }, none :: {} ] | r1 } -> Array { id :: Int, sender :: String, subject :: String, status :: [ unread :: {}, read :: {} ], emphasis :: [ highlighted :: {}, plain :: {} ] }
mailboxRows { messages, opened } = messages # map \g ->
  { id: g.id
  , sender: g.sender
  , subject: g.subject
  , status: g.status
  , emphasis: if isUnread g.status || isOpened g.id opened then .highlighted {} else .plain {}
  }

messageLine :: forall r1. { sender :: String, subject :: String, status :: [ unread :: {}, read :: {} ] | r1 } -> String
messageLine { sender, subject, status } = match { unread: const "● ", read: const "" } status <> sender <> " — " <> subject

fromLine :: forall r1. { sender :: String, subject :: String, body :: String | r1 } -> String
fromLine { sender } = "From: " <> sender

subjectLine :: forall r1. { sender :: String, subject :: String, body :: String | r1 } -> String
subjectLine { subject } = subject

bodyText :: forall r1. { sender :: String, subject :: String, body :: String | r1 } -> String
bodyText { body } = body

isUnread :: [ unread :: {}, read :: {} ] -> Boolean
isUnread = match { unread: const true, read: const false }

isOpened :: Int -> [ message :: { id :: Int }, none :: {} ] -> Boolean
isOpened id = match { message: \m -> m.id == id, none: const false }

highlighted :: forall r1. { id :: Int, sender :: String, subject :: String, status :: [ unread :: {}, read :: {} ], emphasis :: [ highlighted :: {}, plain :: {} ] | r1 } -> Boolean
highlighted { emphasis } = match { highlighted: const true, plain: const false } emphasis

openMessage :: forall r1. Int -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] }, opened :: [ message :: { id :: Int }, none :: {} ] | r1 } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] }, opened :: [ message :: { id :: Int }, none :: {} ] | r1 }
openMessage id m@{ messages } = m { messages = map (\g -> if g.id == id then g { status = .read {} } else g) messages, opened = .message { id } }

messageView :: forall r1. { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] }, opened :: [ message :: { id :: Int }, none :: {} ] | r1 } -> [ reading :: { sender :: String, subject :: String, body :: String }, browsing :: {} ]
messageView { messages, opened } = match
  { message: \m -> case find (\g -> g.id == m.id) messages of
    Just message -> .reading { sender: message.sender, subject: message.subject, body: message.body }
    Nothing -> .browsing {}
  , none: const (.browsing {})
  } opened

deletionOf :: forall r1. { deletion :: [ silent :: {}, confirming :: {} ] | r1 } -> [ silent :: {}, confirming :: {} ]
deletionOf { deletion } = deletion

requestDelete :: forall r1. { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] }, opened :: [ message :: { id :: Int }, none :: {} ], deletion :: [ silent :: {}, confirming :: {} ] | r1 } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] }, opened :: [ message :: { id :: Int }, none :: {} ], deletion :: [ silent :: {}, confirming :: {} ] | r1 }
requestDelete m@{ messages } = if length messages == 1 then m { deletion = .confirming {} } else deleteOpened m

deleteOpened :: forall r1. { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] }, opened :: [ message :: { id :: Int }, none :: {} ], deletion :: [ silent :: {}, confirming :: {} ] | r1 } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] }, opened :: [ message :: { id :: Int }, none :: {} ], deletion :: [ silent :: {}, confirming :: {} ] | r1 }
deleteOpened m@{ messages, opened } = m { messages = filter (\g -> not (isOpened g.id opened)) messages, opened = .none {}, deletion = .silent {} }

keepMessages :: forall r1. { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] }, opened :: [ message :: { id :: Int }, none :: {} ], deletion :: [ silent :: {}, confirming :: {} ] | r1 } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] }, opened :: [ message :: { id :: Int }, none :: {} ], deletion :: [ silent :: {}, confirming :: {} ] | r1 }
keepMessages m = m { deletion = .silent {} }

inboxZeroLine :: forall r1. { | r1 } -> String
inboxZeroLine _ = "Inbox zero!"

composeMessage :: forall r1. { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] }, nextId :: Int | r1 } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] }, nextId :: Int | r1 }
composeMessage m@{ messages, nextId } = m
  { messages = snoc messages { id: nextId, sender: "Me", subject: "Draft " <> show nextId, body: "A freshly composed note, still looking for its recipient.", status: .unread {} }
  , nextId = nextId + 1
  }

sortBySender :: forall r1. { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] } | r1 } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] } | r1 }
sortBySender m@{ messages } = m { messages = sortBy (comparing _.sender) messages }

sortBySubject :: forall r1. { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] } | r1 } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] } | r1 }
sortBySubject m@{ messages } = m { messages = sortBy (comparing _.subject) messages }

sortUnreadFirst :: forall r1. { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] } | r1 } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] } | r1 }
sortUnreadFirst m@{ messages } = m { messages = sortBy (comparing (readRank <<< _.status)) messages }

readRank :: [ unread :: {}, read :: {} ] -> Int
readRank = match { unread: const 0, read: const 1 }
