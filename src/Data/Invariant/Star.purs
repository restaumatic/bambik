module Data.Invariant.Star
  ( InvStar(..)
  , Pure
  )
  where

import Prelude hiding (zero)

import Data.Either (Either(..), either)
import Data.Identity (Identity)
import Data.Invariant (class Cartesian, class CoCartesian, class Invariant)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Plus (class Plus)
import Data.Tuple (Tuple(..))

-- basically, invariant version of Star
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

instance Monad f => Plus (InvStar f) where
  plus p1 p2 = wrap $ unwrap p1 >=> unwrap p2
  zero = wrap pure

type Pure = InvStar Identity
