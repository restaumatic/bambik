module Data.Invariant.Star
  ( InvStar(..)
  , Pure
  )
  where

import Prelude hiding (zero)

import Data.Distributive (class Distributive, distribute)
import Data.Either (Either(..), either)
import Data.Identity (Identity)
import Data.Invariant (class Cartesian, class Closed, class CoCartesian, class Invariant)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Plus (class Plus, class Plusoid)
import Data.Tuple (Tuple(..))

-- InvStar turns functors into invariants.
-- Basically, invariant version of `Star`.
newtype InvStar :: (Type -> Type) -> Type -> Type
newtype InvStar f a = InvStar (a -> f a)

derive instance Newtype (InvStar f a) _

instance Functor f => Invariant (InvStar f) where
  invmap post pre p = wrap $ pre >>> unwrap p >>> map post

instance Functor f => Cartesian (InvStar f) where
  invfirst p = wrap \(Tuple a b) -> (_ `Tuple` b) <$> unwrap p a
  invsecond p = wrap \(Tuple a b) -> (a `Tuple` _) <$> unwrap p b

instance Applicative f => CoCartesian (InvStar f) where
  invleft p = wrap $ either (unwrap p >>> map Left) (pure <<< Right)
  invright p = wrap $ either (pure <<< Left) (unwrap p >>> map Right)

instance Monad f => Plusoid (InvStar f) where
  plus p1 p2 = wrap $ unwrap p1 >=> unwrap p2

instance Monad f => Plus (InvStar f) where
  pzero = wrap pure

instance Distributive f => Closed (InvStar f) where
  closed i = wrap \a -> distribute (unwrap i <<< a)

-- In particular, Pure turn any endomorphism
type Pure = InvStar Identity
