module Reorder (reorder) where

import Prelude ((#), ($), (<<<), (>>>), Unit, const)

import Data.Array (snoc, uncons)
import Data.Maybe (maybe)
import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToRecord (pempty)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (PUI, displayed, mvu, updates)
import PUI.HTML (body, el, foreach, li, span, text, ul, (:=))
import PUI.MDC (button, card, elevation20)
import PUI.Web (Web)
import QualifiedDo.Semigroupoid as Semigroupoid

type Track = { id :: String, title :: String }
type Playlist = { order :: Array Track }

reorder :: Effect Unit
reorder =
  body $
    elevation20 $
      card { caption: "Reorder" } $ ( Semigroupoid.do
          button { label: "Rotate", icon: "sync" } # updates (match { clicked: const <<< rotate })
          ul ( trackWidget # foreach _.id ) # lcmap _.order # displayed
      ) # mvu initialPlaylist

-- one row per track, keyed by track id. The row's title is model-fed; its
-- checkbox is a bare `<input>` with no channel binding, so its `checked` state
-- is purely DOM-local. Rotating the list moves each row's DOM node with its
-- track — so the ticked checkbox follows its track to the new position. A
-- positional reconciler would keep the node put and re-feed it another track's
-- title, leaving the tick behind on the wrong track.
trackWidget :: PUI Web Track {}
trackWidget = li >>> "style" := "display: flex; gap: 10px; align-items: center; list-style: none; margin: 6px 0;" $ RecordToRecord.do
  el "input" >>> "type" := "checkbox" $ pempty
  span (text # lcmap (\(t :: Track) -> { value: t.title }))

rotate :: Playlist -> Playlist
rotate pl = pl { order = maybe pl.order (\{ head, tail } -> snoc tail head) (uncons pl.order) }

initialPlaylist :: Playlist
initialPlaylist =
  { order:
      [ { id: "t1", title: "Track 1" }
      , { id: "t2", title: "Track 2" }
      , { id: "t3", title: "Track 3" }
      , { id: "t4", title: "Track 4" }
      ]
  }
