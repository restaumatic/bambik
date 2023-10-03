module Data.Invariant.Plus
  ( (^)
  , class InvPlus
  , class InvPlusoid
  , invplus
  , invzero
  )
  where

class InvPlusoid :: forall k. (k -> Type) -> Constraint
class InvPlusoid i where
    invplus :: forall a . i a -> i a -> i a
    -- laws:
    -- invplus a (invplus b c) = invplus (invplus a b) c

-- Similar to `Alt` but without `Functor` constraint
class InvPlus :: forall k. (k -> Type) -> Constraint
class InvPlusoid i <= InvPlus i where
    invzero :: forall a . i a
    -- laws:
    --  invplus a invzero == a = invplus invzero a

-- lower precedence than `#`` which is 0
infixr 0 invplus as ^
