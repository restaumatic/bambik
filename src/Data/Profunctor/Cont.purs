module Data.Profunctor.Cont where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..))

-- Add to profunctors package?
newtype Cont r a b = Cont ((b -> r) -> (a -> r))

derive instance Newtype (Cont r a b) _

instance Profunctor (Cont r) where
  dimap f g r = wrap \br -> (unwrap r) (br <<< g) <<< f

instance Strong (Cont r) where
  first r = wrap \bd2r (Tuple a d) -> unwrap r (\b -> bd2r (Tuple b d)) a
  second r = wrap \db2r (Tuple d a) -> unwrap r (\b -> db2r (Tuple d b)) a

instance Choice (Cont r) where
  left r = wrap \bd2r -> case _ of
    Left a -> unwrap r (\b -> bd2r (Left b)) a
    Right d -> bd2r (Right d)
  right r = wrap \db2r -> case _ of
    Right a -> unwrap r (\b -> db2r (Right b)) a
    Left d -> db2r (Left d)

edit :: forall r a b. (a -> b) -> Cont r a b
edit a2b = wrap (\b2r -> a2b >>> b2r)

introduce :: forall r b. b -> Cont r Unit b
introduce b = edit (const b)

-- a.k.a. "handle"?
eliminate :: forall a r. (a -> r) -> Cont r a Void
eliminate a2r = wrap (\_ a -> a2r a)

