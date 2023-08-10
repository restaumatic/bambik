module Data.Invariant.Transformers.Scoped
  ( Scope(..)
  , Hop
  , Scoped(..)
  , invConstructor
  , invField
  , invField'
  )
  where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, cons, cons', uncons) as NonEmptyArray
import Data.Either (Either(..), either)
import Data.Foldable (intercalate)
import Data.Invariant (class Cartesian, class CoCartesian, invfirst, invleft, invmap)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Zero (class Zero)
import Prim.Row as Row
import Record (get, set)
import Type.Proxy (Proxy)

data Scoped a = Scoped Scope a

instance Functor Scoped where
  map f (Scoped c a) = Scoped c (f a)

data Scope = All | Part (NonEmptyArray.NonEmptyArray Hop) | None -- this is actually a lattice (bottom, singleton vales, top), TODO: find already existing data type for it

type Hop = String

instance Show Scope where
  show All = "*"
  show None = "-"
  show (Part hops) = "'" <> intercalate "/" hops <> "'"

-- finds least common scope
instance Semigroup Scope where
  append None s = s
  append s None = s
  append All _ = All
  append _ All = All
  append (Part hops1) (Part hops2)
    | hops1 == hops2 = (Part hops1) -- TODO optimization: get common hops prefix
    | otherwise = All

instance Monoid Scope where
  mempty = None

instance Zero Scope where
  zero = All

zoomOut :: Hop -> Scope -> Scope
zoomOut hop All = Part (hop `NonEmptyArray.cons'` [])
zoomOut hop (Part hops) = Part (hop `NonEmptyArray.cons` hops)
zoomOut _ None = None

-- TODO CHECK!
zoomIn :: Hop -> Scope -> Scope
zoomIn _ All = All
zoomIn hop (Part hops) = case NonEmptyArray.uncons hops of
  { head, tail } | head == hop -> case Array.uncons tail of
    Just { head, tail } -> Part $ NonEmptyArray.cons' head tail
    Nothing -> All
  _ -> None
zoomIn _ None = None

invField :: forall i a s. Cartesian i => Hop -> (s -> a -> s) -> (s -> a) -> i (Scoped a) -> i (Scoped s)
invField name setter getter = invfirst >>> invmap
  (\(Tuple (Scoped c a) s) -> Scoped (zoomOut name c) (setter s a))
  (\(Scoped c s) -> Tuple (Scoped (zoomIn name c) (getter s)) s)

invConstructor :: forall i a s. CoCartesian i => Hop -> (a -> s) -> (s -> Maybe a) -> i (Scoped a) -> i (Scoped s)
invConstructor name construct deconstruct = invleft >>> invmap
  (\saors -> either (\(Scoped c a) -> Scoped (zoomOut name c) (construct a)) identity saors)
  (\(Scoped c s) -> maybe (Right (Scoped c s)) (\a -> Left (Scoped (zoomIn name c) a)) (deconstruct s))

invField' :: forall i l r1 r a . Cartesian i => IsSymbol l => Row.Cons l a r r1 => Proxy l -> i (Scoped a) -> i (Scoped (Record r1))
invField' l = invField (reflectSymbol l) (flip (set l)) (get l)
