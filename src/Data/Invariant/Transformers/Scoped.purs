module Data.Invariant.Transformers.Scoped
  ( Scope
  , Scoped(..)
  , invConstructor
  , invField
  , invField'
  )
  where

import Prelude

import Data.Array (cons, uncons)
import Data.Either (Either(..), either)
import Data.Foldable (intercalate)
import Data.Invariant (class Cartesian, class CoCartesian, class Filtered, invfirst, invleft, invmap)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Zero (class Zero)
import Prim.Row as Row
import Record (get, set)
import Type.Proxy (Proxy)

data Scoped a = Scoped Scope a

data Scope = Scope (Array Hop) | AnyScope -- this is actually a lattice (bottom, singleton vales, top), TODO: find already existing data type for it

type Hop = String

instance Show Scope where
  show AnyScope = "*"
  show (Scope hops) = "'" <> intercalate "/" hops <> "'"

-- finds least common scope
instance Semigroup Scope where
  append AnyScope s = s
  append s AnyScope = s
  append s1 s2 = s1 -- TODO this is hack!

instance Monoid Scope where
  mempty = AnyScope

instance Zero Scope where
  zero = Scope []

zoomOut :: Hop -> Scope -> Scope
zoomOut hop AnyScope = Scope [hop]
zoomOut hop (Scope hops) = Scope (hop `cons` hops)  -- zooming out zero is not zero

-- TODO CHECK!
zoomIn :: Hop -> Scope -> Scope
zoomIn _ AnyScope = AnyScope
zoomIn hop s@(Scope hops) = case uncons hops of
  Nothing -> s -- zooming in zero is zero
  Just { head, tail }
    | head == hop -> Scope tail
    | otherwise -> AnyScope -- cannot zoom in

invField :: forall i a s. Cartesian i => Hop -> (s -> a -> s) -> (s -> a) -> i (Scoped a) -> i (Scoped s)
invField name setter getter = invfirst >>> invmap
  (\(Tuple (Scoped c a) s) -> Scoped (zoomOut name c) (setter s a))
  (\(Scoped c s) -> Tuple (Scoped (zoomIn name c) (getter s)) s)

invConstructor :: forall i a s. CoCartesian i => Hop -> (a -> s) -> (s -> Maybe a) -> i (Scoped a) -> i (Scoped s)
invConstructor name construct deconstruct = invleft >>> invmap
  (\saors -> either (\(Scoped c a) -> Scoped (zoomOut name c) (construct a)) identity saors)
  (\(Scoped c s) -> maybe (Right (Scoped c s)) (\a -> Left (Scoped (zoomIn name c) a)) (deconstruct s))

invField' :: forall i l r1 r a . Filtered i => Cartesian i => IsSymbol l => Row.Cons l a r r1 => Proxy l -> i (Scoped a) -> i (Scoped (Record r1))
invField' l = invField (reflectSymbol l) (flip (set l)) (get l)
