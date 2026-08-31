module InboxLogic (composeMessage, deleteOpened, deletionOf, highlighted, inboxZeroLine, keepMessages, mailboxRows, mondayMail, messageView, openMessage, presentInbox, readState, requestDelete, sortBySender, sortBySubject, sortUnreadFirst) where

import Prelude ((<>), (#), (+), (==), (||), comparing, map, not, show)

import Data.Array (filter, find, length, snoc, sortBy)
import Data.Maybe (Maybe(..))
import Data.Variant (match)

mondayMail :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] }, opened :: [ message :: { id :: Int }, none :: {} ], deletion :: [ silent :: {}, confirming :: {} ], nextId :: Int, unreadCountText :: String, messageCountText :: String }
mondayMail = presentInbox
  { messages:
      [ { id: 1, sender: "Alice Kowalska", subject: "Quarterly report ready", body: "The Q2 numbers are in - revenue up 12%, see the attached sheet before Friday's review.", status: .unread {} }
      , { id: 2, sender: "Bob Nowak", subject: "Lunch on Thursday?", body: "The new ramen place near the office finally opened. Noon works for me.", status: .read {} }
      , { id: 3, sender: "Carol Wu", subject: "Code review request", body: "Could you take a look at the profunctor refactor branch? Two files, mostly renames.", status: .unread {} }
      ]
  , opened: .none {}
  , deletion: .silent {}
  , nextId: 4
  , unreadCountText: ""
  , messageCountText: ""
  }

presentInbox :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] }, opened :: [ message :: { id :: Int }, none :: {} ], deletion :: [ silent :: {}, confirming :: {} ], nextId :: Int, unreadCountText :: String, messageCountText :: String } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] }, opened :: [ message :: { id :: Int }, none :: {} ], deletion :: [ silent :: {}, confirming :: {} ], nextId :: Int, unreadCountText :: String, messageCountText :: String }
presentInbox r = r
  { unreadCountText = show (length (filter isUnread r.messages))
  , messageCountText = show (length r.messages)
  }

mailboxRows :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] }, opened :: [ message :: { id :: Int }, none :: {} ] } -> Array { id :: Int, sender :: String, subject :: String, status :: [ unread :: {}, read :: {} ], emphasis :: [ highlighted :: {}, plain :: {} ] }
mailboxRows { messages, opened } = messages # map \g ->
  { id: g.id
  , sender: g.sender
  , subject: g.subject
  , status: g.status
  , emphasis: if isUnread g || isOpened g.id opened then .highlighted {} else .plain {}
  }

isUnread :: { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] } -> Boolean
isUnread { status } = match { unread: \_ -> true, read: \_ -> false } status

isOpened :: Int -> [ message :: { id :: Int }, none :: {} ] -> Boolean
isOpened id = match { message: \m -> m.id == id, none: \_ -> false }

highlighted :: { id :: Int, sender :: String, subject :: String, status :: [ unread :: {}, read :: {} ], emphasis :: [ highlighted :: {}, plain :: {} ] } -> Boolean
highlighted { emphasis } = match { highlighted: \_ -> true, plain: \_ -> false } emphasis

readState :: { status :: [ unread :: {}, read :: {} ] } -> [ unread :: {}, read :: {} ]
readState { status } = status

openMessage :: Int -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] }, opened :: [ message :: { id :: Int }, none :: {} ] } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] }, opened :: [ message :: { id :: Int }, none :: {} ] }
openMessage id m@{ messages } = m { messages = map (\g -> if g.id == id then g { status = .read {} } else g) messages, opened = .message { id } }

messageView :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] }, opened :: [ message :: { id :: Int }, none :: {} ] } -> [ reading :: { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] }, browsing :: {} ]
messageView { messages, opened } = match
  { message: \m -> case find (\g -> g.id == m.id) messages of
      Just message -> .reading message
      Nothing -> .browsing {}
  , none: \_ -> .browsing {}
  } opened

lastMessage :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] } } -> Boolean
lastMessage { messages } = length messages == 1

deletionOf :: { deletion :: [ silent :: {}, confirming :: {} ] } -> [ silent :: {}, confirming :: {} ]
deletionOf { deletion } = deletion

requestDelete :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] }, opened :: [ message :: { id :: Int }, none :: {} ], deletion :: [ silent :: {}, confirming :: {} ] } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] }, opened :: [ message :: { id :: Int }, none :: {} ], deletion :: [ silent :: {}, confirming :: {} ] }
requestDelete m@{ messages } = if lastMessage { messages } then m { deletion = .confirming {} } else deleteOpened m

deleteOpened :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] }, opened :: [ message :: { id :: Int }, none :: {} ], deletion :: [ silent :: {}, confirming :: {} ] } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] }, opened :: [ message :: { id :: Int }, none :: {} ], deletion :: [ silent :: {}, confirming :: {} ] }
deleteOpened m@{ messages, opened } = m { messages = filter (\g -> not (isOpened g.id opened)) messages, opened = .none {}, deletion = .silent {} }

keepMessages :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] }, opened :: [ message :: { id :: Int }, none :: {} ], deletion :: [ silent :: {}, confirming :: {} ] } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] }, opened :: [ message :: { id :: Int }, none :: {} ], deletion :: [ silent :: {}, confirming :: {} ] }
keepMessages m = m { deletion = .silent {} }

inboxZeroLine :: String
inboxZeroLine = "Inbox zero!"

composeMessage :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] }, nextId :: Int } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] }, nextId :: Int }
composeMessage m@{ messages, nextId } = m
  { messages = snoc messages { id: nextId, sender: "Me", subject: "Draft " <> show nextId, body: "A freshly composed note, still looking for its recipient.", status: .unread {} }
  , nextId = nextId + 1
  }

sortBySender :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] } } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] } }
sortBySender m@{ messages } = m { messages = sortBy (comparing _.sender) messages }

sortBySubject :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] } } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] } }
sortBySubject m@{ messages } = m { messages = sortBy (comparing _.subject) messages }

sortUnreadFirst :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] } } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] } }
sortUnreadFirst m@{ messages } = m { messages = sortBy (comparing readRank) messages }

readRank :: { id :: Int, sender :: String, subject :: String, body :: String, status :: [ unread :: {}, read :: {} ] } -> Int
readRank { status } = match { unread: \_ -> 0, read: \_ -> 1 } status
