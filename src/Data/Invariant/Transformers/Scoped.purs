module Data.Invariant.Transformers.Scoped
  ( Part(..)
  , PartName
  , Scoped(..)
  , invAdapter
  , invConstructor
  , invField
  , invProjection
  , field
  , adapter
  , proConstructor
  , projection
  )
  where

import Prelude

import Data.Array (uncons, (:))
import Data.Array.NonEmpty (NonEmptyArray, cons, cons', fromArray, uncons) as NonEmptyArray
import Data.Either (Either(..), either)
import Data.Foldable (intercalate)
import Data.Invariant (class InvCartesian, class InvCocartesian, class Invariant, invfirst, invleft, invmap)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Optics (class ProCartesian, class ProCocartesian, profirst, proleft, promap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row as Row
import Record (get, set)
import Type.Proxy (Proxy(..))

data Scoped a = Scoped Part a

data Part = MoreThanOnePart | OnePart (NonEmptyArray.NonEmptyArray PartName) | NoPart -- this is actually a lattice (bottom, singleton vales, top), TODO: find already existing data type for it

data PartName = PartName String | TwistName String

instance Show PartName where
  show (PartName s) = "." <> s
  show (TwistName s) = "@" <> s

derive instance Eq PartName

instance Show Part where
  show MoreThanOnePart = "+"
  show NoPart = "-"
  show (OnePart hops) = intercalate "" (show <$> hops)

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
zoomOut partName MoreThanOnePart = OnePart (partName `NonEmptyArray.cons'` [])
zoomOut partName (OnePart hops) = OnePart (partName `NonEmptyArray.cons` hops)
zoomOut _ NoPart = NoPart

zoomIn :: PartName -> Part -> Part
zoomIn _ MoreThanOnePart = MoreThanOnePart
zoomIn partName (OnePart hops) = case NonEmptyArray.uncons hops of
  { head, tail } | head == partName -> case uncons tail of -- matching head
    Just { head, tail } -> OnePart $ NonEmptyArray.cons' head tail -- non empty tail
    Nothing -> MoreThanOnePart -- empty tail
  { head: TwistName twistName, tail } -> MoreThanOnePart -- not matching head but head is twist
  _ -> case partName of
    TwistName _ -> MoreThanOnePart -- not matching head but partName is twist
    _ -> NoPart -- otherwise
zoomIn _ NoPart = NoPart

invField :: forall @l i r1 r a . InvCartesian i => IsSymbol l => Row.Cons l a r r1 => i (Scoped a) -> i (Scoped (Record r1))
invField = invField' (reflectSymbol (Proxy @l)) (flip (set (Proxy @l))) (get (Proxy @l))
  where
    invField' :: forall i a s. InvCartesian i => String -> (s -> a -> s) -> (s -> a) -> i (Scoped a) -> i (Scoped s)
    invField' partName setter getter = invfirst >>> invmap
      (\(Tuple (Scoped c a) s) -> Scoped (zoomOut (PartName partName) c) (setter s a))
      (\(Scoped c s) -> Tuple (Scoped (zoomIn (PartName partName) c) (getter s)) s)

invConstructor :: forall i a s. InvCocartesian i => String -> (a -> s) -> (s -> Maybe a) -> i (Scoped a) -> i (Scoped s)
invConstructor name construct deconstruct = invleft >>> invmap
  (\saors -> either (\(Scoped c a) -> Scoped (zoomOut (PartName name) c) (construct a)) identity saors)
  (\(Scoped c s) -> maybe (Right (Scoped c s)) (\a -> Left (Scoped (zoomIn (PartName name) c) a)) (deconstruct s))

invProjection :: forall i a s . InvCartesian i => String -> (s -> a) -> i (Scoped a) -> i (Scoped s)
invProjection name f = invfirst >>> invmap
  (\(Tuple (Scoped c _) s) -> Scoped (zoomOut (PartName name) c) s)
  (\(Scoped c s) -> Tuple (Scoped (zoomIn (PartName name) c) (f s)) s)

invAdapter :: forall i a b. Invariant i => String -> (a -> b) -> (b -> a) -> i (Scoped a) -> i (Scoped b)
invAdapter name f g = invmap
  (\(Scoped c a) -> Scoped (zoomOut (TwistName name) c) (f a))
  (\(Scoped c b) -> Scoped (zoomIn (TwistName name) c) (g b))



---

field :: forall @l i r1 r a . ProCartesian i => IsSymbol l => Row.Cons l a r r1 => i (Scoped a) (Scoped a)-> i (Scoped (Record r1)) (Scoped (Record r1))
field = field' (reflectSymbol (Proxy @l)) (flip (set (Proxy @l))) (get (Proxy @l))
  where
    field' :: forall i a s. ProCartesian i => String -> (s -> a -> s) -> (s -> a) -> i (Scoped a) (Scoped a)-> i (Scoped s) (Scoped s)
    field' partName setter getter = profirst >>> promap
      (\(Scoped c s) -> Tuple (Scoped (zoomIn (PartName partName) c) (getter s)) s)
      (\(Tuple (Scoped c a) s) -> Scoped (zoomOut (PartName partName) c) (setter s a))

proConstructor :: forall i a s. ProCocartesian i => String -> (a -> s) -> (s -> Maybe a) -> i (Scoped a) (Scoped a) -> i (Scoped s) (Scoped s)
proConstructor name construct deconstruct = proleft >>> promap
  (\(Scoped c s) -> maybe (Right (Scoped c s)) (\a -> Left (Scoped (zoomIn (PartName name) c) a)) (deconstruct s))
  (\saors -> either (\(Scoped c a) -> Scoped (zoomOut (PartName name) c) (construct a)) identity saors)

projection :: forall i a s . ProCartesian i => String -> (s -> a) -> i (Scoped a) (Scoped a) -> i (Scoped s) (Scoped s)
projection name f = profirst >>> promap
  (\(Scoped c s) -> Tuple (Scoped (zoomIn (PartName name) c) (f s)) s)
  (\(Tuple (Scoped c _) s) -> Scoped (zoomOut (PartName name) c) s)

adapter :: forall i a b s t. Profunctor i => String -> (b -> t) -> (s -> a) -> i (Scoped a) (Scoped b) -> i (Scoped s) (Scoped t)
adapter name outside inside = promap
  (\(Scoped c b) -> Scoped (zoomIn (TwistName name) c) (inside b))
  (\(Scoped c a) -> Scoped (zoomOut (TwistName name) c) (outside a))

