module Data.Plus
  ( (^)
  , (^^)
  , class InvPlus
  , class InvPlusoid
  , class ProPlus
  , class ProPlusoid
  , invplus
  , invzero
  , proplus
  , prozero
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

class ProPlusoid :: forall k. (k -> k -> Type) -> Constraint
class ProPlusoid p where
    proplus :: forall a . p a a -> p a a -> p a a
    -- laws:
    -- proplus a (proplus b c) = proplus (proplus a b) c

class ProPlus :: forall k. (k -> k -> Type) -> Constraint
class ProPlusoid p <= ProPlus p where
    prozero :: forall a b. p a b
    -- laws:
    --  proplus a prozero == a = proplus invzero a

-- lower precedence than `#`` which is 0
infixr 0 proplus as ^^
