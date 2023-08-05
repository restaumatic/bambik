module Data.Plus
  ( (^)
  , class Plusoid
  , class Plus
  , plus
  , pzero
  )
  where

class Plusoid :: forall k. (k -> Type) -> Constraint
class Plusoid i where
    plus :: forall a . i a -> i a -> i a

-- Similar to `Alt` but without `Functor` constraint
class Plus :: forall k. (k -> Type) -> Constraint
class Plusoid i <= Plus i where
    pzero :: forall a . i a
    -- laws:
    --  plus a pzero == a = plus pzero a
    --  plus a (plus b c) == plus (plus a b) c

-- lower precedence than `#`` which is 0
infixr 0 plus as ^
