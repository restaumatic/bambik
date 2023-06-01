module Data.Plus
  ( class Plus
  , plus
  , zero
  )
  where

-- Similar to `Alt` but without `Functor` constraint
class Plus :: forall k. (k -> Type) -> Constraint
class Plus i where
    plus :: forall a . i a -> i a -> i a
    zero :: forall a . i a
    -- laws:
    --  plus a zero == a = plus zero a
    --  plus a (plus b c) == plus (plus a b) c

