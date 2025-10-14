module Data.Profunctor.Cont where

import Prelude

import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (class Profunctor)

-- Add to profunctors package?
newtype Cont r a b = Cont ((b -> r) -> (a -> r))

derive instance Newtype (Cont r a b) _

instance Profunctor (Cont r) where
  dimap f g r = wrap \br -> (unwrap r) (br <<< g) <<< f

foo :: forall r a b. (a -> b) -> Cont r a b
foo a2b = wrap (\b2r -> a2b >>> b2r)

bar :: forall r b. b -> Cont r Unit b
bar b = foo (const b)

baz :: forall a. Cont a a Void
baz = wrap (const identity)
