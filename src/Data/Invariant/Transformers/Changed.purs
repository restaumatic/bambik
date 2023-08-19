module Data.Invariant.Transformers.Changed
  ( Change(..)
  , Scope(..)
  , Changed(..)
  , invAdapter
  , invConstructor
  , invField
  , invProjection
  , zoomIn
  , zoomOut
  )
  where

import Prelude

import Data.Array (uncons, (:))
import Data.Array.NonEmpty (NonEmptyArray, cons, cons', fromArray, uncons) as NonEmptyArray
import Data.Either (Either(..), either)
import Data.Foldable (intercalate)
import Data.Invariant (class InvCartesian, class InvCocartesian, class Invariant, invfirst, invleft, invmap)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Debug (spy)
import Prim.Row as Row
import Record (get, set)
import Type.Proxy (Proxy(..))

data Changed a = Changed Change a

data Change = Some | Scoped (NonEmptyArray.NonEmptyArray Scope) | None -- TODO: find already existing data type for it

data Scope = Part String | Variant String

instance Show Scope where
  show (Part s) = "." <> s
  show (Variant s) = "@" <> s

derive instance Eq Scope

instance Show Change where
  show Some = "+"
  show None = "-"
  show (Scoped hops) = intercalate "" (show <$> hops)

-- finds least common scope
instance Semigroup Change where
  append None s = s
  append s None = s
  append Some _ = Some
  append _ Some = Some
  append (Scoped hops1) (Scoped hops2) = case NonEmptyArray.fromArray $ commonPrefix hops1 hops2 of
    Nothing -> Some -- no common prefix
    Just prefix -> Scoped prefix -- common prefix
    where
      commonPrefix :: forall a. Eq a => NonEmptyArray.NonEmptyArray a -> NonEmptyArray.NonEmptyArray a -> Array a
      commonPrefix a1 a2 = let
        {head: h1, tail: t1} = NonEmptyArray.uncons a1
        {head: h2, tail: t2} = NonEmptyArray.uncons a2
        in if h1 == h2 then h1:(fromMaybe [] $ commonPrefix <$> NonEmptyArray.fromArray t1 <*> NonEmptyArray.fromArray t2)
        else []

instance Monoid Change where
  mempty = None

zoomOut :: Scope -> Change -> Change
zoomOut partName Some = Scoped (partName `NonEmptyArray.cons'` [])
zoomOut partName (Scoped hops) = Scoped (partName `NonEmptyArray.cons` hops)
zoomOut _ None = None

zoomIn :: Scope -> Change -> Change
zoomIn _ Some = Some
zoomIn partName (Scoped hops) = case NonEmptyArray.uncons hops of
  { head, tail } | head == partName -> case uncons tail of -- matching head
    Just { head, tail } -> Scoped $ NonEmptyArray.cons' head tail -- non empty tail
    Nothing -> Some -- empty tail
  { head: Variant twistName } -> Some -- not matching head but head is twist
  _ -> case partName of
    Variant _ -> Some -- not matching head but partName is twist
    _ -> None -- otherwise
zoomIn _ None = None

zoomIn' :: Scope -> Change -> Change
zoomIn' partName part = let result = zoomIn partName part in spy ("\nzoomin:\npartName: " <> show partName <> " part: " <> show part <> " result: " <> show result) result

invField :: forall @l i r1 r a . InvCartesian i => IsSymbol l => Row.Cons l a r r1 => i (Changed a) -> i (Changed (Record r1))
invField = invField' (reflectSymbol (Proxy @l)) (flip (set (Proxy @l))) (get (Proxy @l))
  where
    invField' :: forall i a s. InvCartesian i => String -> (s -> a -> s) -> (s -> a) -> i (Changed a) -> i (Changed s)
    invField' partName setter getter = invfirst >>> invmap
      (\(Tuple (Changed c a) s) -> Changed (zoomOut (Part partName) c) (setter s a))
      (\(Changed c s) -> Tuple (Changed (zoomIn (Part partName) c) (getter s)) s)

invConstructor :: forall i a s. InvCocartesian i => String -> (a -> s) -> (s -> Maybe a) -> i (Changed a) -> i (Changed s)
invConstructor name construct deconstruct = invleft >>> invmap
  (\saors -> either (\(Changed c a) -> Changed (zoomOut (Part name) c) (construct a)) identity saors)
  (\(Changed c s) -> maybe (Right (Changed c s)) (\a -> Left (Changed (zoomIn (Part name) c) a)) (deconstruct s))

invProjection :: forall i a s . InvCartesian i => String -> (s -> a) -> i (Changed a) -> i (Changed s)
invProjection name f = invfirst >>> invmap
  (\(Tuple (Changed c _) s) -> Changed (zoomOut (Part name) c) s)
  (\(Changed c s) -> Tuple (Changed (zoomIn (Part name) c) (f s)) s)

invAdapter :: forall i a b. Invariant i => String -> (a -> b) -> (b -> a) -> i (Changed a) -> i (Changed b)
invAdapter name f g = invmap
  (\(Changed c a) -> Changed (zoomOut (Variant name) c) (f a))
  (\(Changed c b) -> Changed (zoomIn (Variant name) c) (g b))
