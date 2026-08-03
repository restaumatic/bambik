module ReorderLogic (openingSetlist, rotateAction, setOrder, shuffleAction) where

import Prelude ((<$>), bind, compare, map, pure)

import Data.Array (snoc, sortBy, uncons)
import Data.Maybe (maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)

openingSetlist :: { order :: Array { id :: String, title :: String } }
openingSetlist =
  { order:
      [ { id: "t1", title: "Track 1" }
      , { id: "t2", title: "Track 2" }
      , { id: "t3", title: "Track 3" }
      , { id: "t4", title: "Track 4" }
      ]
  }

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
