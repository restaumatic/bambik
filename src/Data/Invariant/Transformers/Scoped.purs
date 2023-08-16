module Data.Invariant.Transformers.Scoped
  ( PartName
  , Part(..)
  , Scoped(..)
  , invAdapter
  , invField
  , invConstructor
  , invProjection
  )
  where

import Prelude

import Data.Array (uncons, (:))
import Data.Array.NonEmpty (NonEmptyArray, cons, cons', fromArray, uncons) as NonEmptyArray
import Data.Either (Either(..), either)
import Data.Foldable (intercalate)
import Data.Invariant (class Cartesian, class CoCartesian, class Invariant, invfirst, invleft, invmap)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row as Row
import Record (get, set)
import Type.Proxy (Proxy(..))

data Scoped a = Scoped Part a

data Part = MoreThanOnePart | OnePart (NonEmptyArray.NonEmptyArray PartName) | NoPart -- this is actually a lattice (bottom, singleton vales, top), TODO: find already existing data type for it

type PartName = String

instance Show Part where
  show MoreThanOnePart = "+"
  show NoPart = "-"
  show (OnePart hops) = intercalate "." hops

-- finds least common scope
instance Semigroup Part where
  append NoPart s = s
  append s NoPart = s
  append MoreThanOnePart _ = MoreThanOnePart
  append _ MoreThanOnePart = MoreThanOnePart
  append (OnePart hops1) (OnePart hops2) = case NonEmptyArray.fromArray $ commonPrefix hops1 hops2 of
    Nothing -> MoreThanOnePart -- no common prefix
    Just prefix -> OnePart prefix -- common prefix
    where
      commonPrefix :: forall a. Eq a => NonEmptyArray.NonEmptyArray a -> NonEmptyArray.NonEmptyArray a -> Array a
      commonPrefix a1 a2 = let
        {head: h1, tail: t1} = NonEmptyArray.uncons a1
        {head: h2, tail: t2} = NonEmptyArray.uncons a2
        in if h1 == h2 then h1:(fromMaybe [] $ commonPrefix <$> NonEmptyArray.fromArray t1 <*> NonEmptyArray.fromArray t2)
        else []

instance Monoid Part where
  mempty = NoPart

zoomOut :: PartName -> Part -> Part
zoomOut hop MoreThanOnePart = OnePart (hop `NonEmptyArray.cons'` [])
zoomOut hop (OnePart hops) = OnePart (hop `NonEmptyArray.cons` hops)
zoomOut _ NoPart = NoPart

-- TODO CHECK!
zoomIn :: PartName -> Part -> Part
zoomIn _ MoreThanOnePart = MoreThanOnePart
zoomIn hop (OnePart hops) = case NonEmptyArray.uncons hops of
  { head, tail } | head == hop -> case uncons tail of -- matching head
    Just { head, tail } -> OnePart $ NonEmptyArray.cons' head tail -- non empty tail
    Nothing -> MoreThanOnePart -- empty tail
  _ -> NoPart -- not matching head
zoomIn _ NoPart = NoPart

invField :: forall @l i r1 r a . Cartesian i => IsSymbol l => Row.Cons l a r r1 => i (Scoped a) -> i (Scoped (Record r1))
invField = invField' (reflectSymbol (Proxy @l)) (flip (set (Proxy @l))) (get (Proxy @l))
  where
    invField' :: forall i a s. Cartesian i => PartName -> (s -> a -> s) -> (s -> a) -> i (Scoped a) -> i (Scoped s)
    invField' name setter getter = invfirst >>> invmap
      (\(Tuple (Scoped c a) s) -> Scoped (zoomOut name c) (setter s a))
      (\(Scoped c s) -> Tuple (Scoped (zoomIn name c) (getter s)) s)

invConstructor :: forall i a s. CoCartesian i => PartName -> (a -> s) -> (s -> Maybe a) -> i (Scoped a) -> i (Scoped s)
invConstructor name construct deconstruct = invleft >>> invmap
  (\saors -> either (\(Scoped c a) -> Scoped (zoomOut name c) (construct a)) identity saors)
  (\(Scoped c s) -> maybe (Right (Scoped c s)) (\a -> Left (Scoped (zoomIn name c) a)) (deconstruct s))

invProjection :: forall i a s . Cartesian i => (s -> a) -> i (Scoped a) -> i (Scoped s)
invProjection f = invfirst >>> invmap
  (\(Tuple (Scoped c a) s) -> Scoped c s)
  (\(Scoped c s) -> Tuple (Scoped MoreThanOnePart (f s)) s)

invAdapter :: forall i a b. Invariant i => (a -> b) -> (b -> a) -> i (Scoped a) -> i (Scoped b)
invAdapter f g = invmap
  (\(Scoped _ a) -> Scoped MoreThanOnePart (f a))
  (\(Scoped _ b) -> Scoped MoreThanOnePart (g b))



