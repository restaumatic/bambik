module Data.Invariant.ScopedInvariant where


import Data.Either (Either(..), either)
import Data.Invariant (class Cartesian, class CoCartesian, invfirst, invleft, invmap)
import Data.Maybe (Maybe, maybe)
import Data.Tuple (Tuple(..))

data Scope = Scope

data Scoped a = Scoped Scope a

invField :: forall i a s. Cartesian i => String -> (s -> a -> s) -> (s -> a) -> i (Scoped a) -> i (Scoped s)
invField fieldName setter getter isa = invmap (\(Tuple (Scoped c a) s) -> Scoped c (setter s a)) (\(Scoped c s) -> Tuple (Scoped c (getter s)) s) (invfirst isa)

invConstructor :: forall i a s. CoCartesian i => String -> (a -> s) -> (s -> Maybe a) -> i (Scoped a) -> i (Scoped s)
invConstructor constructorName construct deconstruct isa = invmap
  (\saors -> either (\(Scoped c a) -> Scoped c (construct a)) (\s -> s) saors)
  (\(Scoped c s) -> maybe (Right (Scoped c s)) (\a -> Left (Scoped c a)) (deconstruct s))
  (invleft isa)
