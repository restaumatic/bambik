module InboxLogic (composeMessage, confirmingDelete, deleteOpened, inboxZeroLine, keepMessages, mailboxRows, messageCountText, mondayMail, openedMessage, openMessage, requestDelete, rowLine, sortBySender, sortBySubject, sortUnreadFirst, unreadCountText, unreadMark) where

import Prelude ((<>), (#), (+), (==), (/=), (||), comparing, map, not, show)

import Data.Array (filter, find, length, snoc, sortBy)
import Data.Maybe (Maybe(..))
import Data.Variant (match)

mondayMail :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, deletion :: [ silent :: {}, confirming :: {} ], nextId :: Int }
mondayMail =
  { messages:
      [ { id: 1, sender: "Alice Kowalska", subject: "Quarterly report ready", body: "The Q2 numbers are in - revenue up 12%, see the attached sheet before Friday's review.", read: false }
      , { id: 2, sender: "Bob Nowak", subject: "Lunch on Thursday?", body: "The new ramen place near the office finally opened. Noon works for me.", read: true }
      , { id: 3, sender: "Carol Wu", subject: "Code review request", body: "Could you take a look at the profunctor refactor branch? Two files, mostly renames.", read: false }
      ]
  , opened: Nothing
  , deletion: .silent {}
  , nextId: 4
  }

unreadCountText :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean } } -> String
unreadCountText { messages } = show (length (filter (\g -> not g.read) messages))

messageCountText :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean } } -> String
messageCountText { messages } = show (length messages)

mailboxRows :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int } -> Array { id :: Int, sender :: String, subject :: String, read :: Boolean, attention :: Boolean }
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

openedMessage :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int } -> Maybe { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }
openedMessage { messages, opened } = find (\g -> Just g.id == opened) messages

lastMessage :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean } } -> Boolean
lastMessage { messages } = length messages == 1

confirmingDelete :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, deletion :: [ silent :: {}, confirming :: {} ] } -> Maybe { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, deletion :: [ silent :: {}, confirming :: {} ] }
confirmingDelete m = match { confirming: \_ -> Just m, silent: \_ -> Nothing } m.deletion

requestDelete :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, deletion :: [ silent :: {}, confirming :: {} ] } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, deletion :: [ silent :: {}, confirming :: {} ] }
requestDelete m@{ messages } = if lastMessage { messages } then m { deletion = .confirming {} } else deleteOpened m

deleteOpened :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, deletion :: [ silent :: {}, confirming :: {} ] } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, deletion :: [ silent :: {}, confirming :: {} ] }
deleteOpened m@{ messages, opened } = m { messages = filter (\g -> Just g.id /= opened) messages, opened = Nothing, deletion = .silent {} }

keepMessages :: { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, deletion :: [ silent :: {}, confirming :: {} ] } -> { messages :: Array { id :: Int, sender :: String, subject :: String, body :: String, read :: Boolean }, opened :: Maybe Int, deletion :: [ silent :: {}, confirming :: {} ] }
keepMessages m = m { deletion = .silent {} }

inboxZeroLine :: String
inboxZeroLine = "Inbox zero!"

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

rowLine :: { id :: Int, sender :: String, subject :: String, read :: Boolean, attention :: Boolean } -> String
rowLine { sender, subject } = sender <> " \x2014 " <> subject
