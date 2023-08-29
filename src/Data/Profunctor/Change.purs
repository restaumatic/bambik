module Data.Profunctor.Change
  ( Change(..)
  , Changed(..)
  , Scope(..)
  , class ChProfunctor
  , static
  , scopemap
  , chmap
  )
  where

import Prelude

import Data.Array (uncons, (:))
import Data.Array.NonEmpty (intercalate)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor (class Profunctor)

-- for debugging uncomment:
-- import Debug (spy)
-- and comment out:
spy :: forall a. String -> a -> a
spy _ = identity

scopemap :: forall p a b. ChProfunctor p => Scope -> p a b -> p a b
scopemap scope = chmap zoomIn zoomOut
  where
    zoomOut :: Change -> Change
    zoomOut change = case zoomOut' change of
      ch@None -> ch
      ch -> spy ("change: " <> show ch <> " < " <> show scope <> " < " <> show change) ch
      where
        zoomOut' :: Change -> Change
        zoomOut' Some = Scoped (scope `NonEmptyArray.cons'` [])
        zoomOut' (Scoped scopes) = Scoped (scope `NonEmptyArray.cons` scopes)
        zoomOut' None = None

    zoomIn :: Change -> Change
    zoomIn change = case zoomIn' change of
      ch@None -> ch
      ch -> spy ("change: " <> show change <> " > " <> show scope <> " > " <> show ch) ch
      where
        zoomIn' :: Change -> Change
        zoomIn' Some = Some
        zoomIn' (Scoped scopes) = case NonEmptyArray.uncons scopes of
          { head, tail } | head == scope -> case uncons tail of -- matching head
            Just { head: headOtTail, tail: tailOfTail } -> Scoped $ NonEmptyArray.cons' headOtTail tailOfTail -- non empty tail
            Nothing -> Some -- empty tail
          { head: Variant _ } -> Some -- not matching head but head is twist
          _ -> case scope of
            Variant _ -> Some -- not matching head but scope is twist
            _ -> None -- otherwise
        zoomIn' None = None

class Profunctor p <= ChProfunctor p where
  chmap :: forall a b. (Change -> Change) -> (Change -> Change) -> p a b -> p a b
  static :: forall a b s t. a -> p a b -> p s t -- TODO EC change name to "not changing", "not related" or similar? Cause it doesn't have to be static (e.g. translations)

data Changed a = Changed Change a

derive instance Functor Changed

data Change = Some | Scoped (NonEmptyArray.NonEmptyArray Scope) | None -- TODO: find already existing data type for it

data Scope = Part String | Variant String

instance Show Scope where
  show (Part s) = "." <> s
  show (Variant s) = "@" <> s

derive instance Eq Scope

instance Show Change where
  show Some = "*"
  show None = "-"
  show (Scoped scopes) = intercalate "" (show <$> scopes)

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
        in if h1 == h2 then h1:fromMaybe [] (commonPrefix <$> NonEmptyArray.fromArray t1 <*> NonEmptyArray.fromArray t2)
        else []

instance Monoid Change where
  mempty = None

