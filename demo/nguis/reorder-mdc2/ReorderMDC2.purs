module ReorderMDC2 (reorderMDC2) where

import Prelude ((#), ($), (<$>), (>>>), Unit, bind, compare, map, pure)

import Data.Array (snoc, sortBy, uncons)
import Data.Maybe (maybe)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Variant (match)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import PUI (action, asCase, asField, edited, field, mvu, onCase, pempty, silence, updated)
import PUI.Web.HTML (body, el, (:=))
import PUI.Web.MDC2 (button, card, cardActions, elevation20, filledTextField, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid

reorderMDC2 :: Effect Unit
reorderMDC2 =
  body $
    elevation20 $
      card { caption: "Reorder" } $ ( Semigroupoid.do
          ( Semigroupoid.do
              cardActions $ RecordToVariant.do
                button { label: "Rotate", icon: "sync" } # asCase @"rotate"
                button { label: "Shuffle", icon: "shuffle" } # asCase @"shuffle"
              VariantToVariant.do
                silence # action rotateAction # onCase @"rotate"
                silence # action shuffleAction # onCase @"shuffle") # updated (match { reordered: setOrder })
          list
            ( ( listItem $ ( RecordToRecord.do
                  el "input" >>> "type" := "checkbox" $ pempty
                  filledTextField { floatingLabel: "Title" } # asField @"title")) # edited @"id") # field @"order"
      ) # mvu openingSetlist

rotateAction
  :: { order :: Array { id :: String, title :: String } }
  -> Aff [ reordered :: Array { id :: String, title :: String } ]
rotateAction { order } = pure (.reordered (rotate { order }))

shuffleAction
  :: { order :: Array { id :: String, title :: String } }
  -> Aff [ reordered :: Array { id :: String, title :: String } ]
shuffleAction { order } = liftEffect (.reordered <$> shuffleOrder order)

rotate :: { order :: Array { id :: String, title :: String } } -> Array { id :: String, title :: String }
rotate { order } = maybe order (\{ head, tail } -> snoc tail head) (uncons order)

setOrder
  :: Array { id :: String, title :: String }
  -> { order :: Array { id :: String, title :: String } }
  -> { order :: Array { id :: String, title :: String } }
setOrder order pl = pl { order = order }

shuffleOrder :: Array { id :: String, title :: String } -> Effect (Array { id :: String, title :: String })
shuffleOrder tracks = do
  keyed <- traverse withKey tracks
  pure (map snd (sortBy (\a b -> compare (fst a) (fst b)) keyed))
  where
  withKey t = do
    k <- randomInt 0 1000000
    pure (Tuple k t)

openingSetlist :: { order :: Array { id :: String, title :: String } }
openingSetlist =
  { order:
      [ { id: "t1", title: "Track 1" }
      , { id: "t2", title: "Track 2" }
      , { id: "t3", title: "Track 3" }
      , { id: "t4", title: "Track 4" }
      ]
  }
