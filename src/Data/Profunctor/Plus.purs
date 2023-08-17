module Data.Profunctor.Plus
  ( (^)
  , (<^)
  , (^>)
  , class ProPlus
  , class ProPlusoid
  , proplus
  , proplusfirst
  , proplussecond
  , prozero
  )
  where

class ProPlusoid :: forall k. (k -> k -> Type) -> Constraint
class ProPlusoid p where
    proplus :: forall a . p a a -> p a a -> p a a
    proplusfirst :: forall a b c. p a b -> p a c -> p a b
    proplussecond :: forall a b c. p a c -> p a b -> p a b
    -- laws:
    -- proplus a (proplus b c) = proplus (proplus a b) c

class ProPlus :: forall k. (k -> k -> Type) -> Constraint
class ProPlusoid p <= ProPlus p where
    prozero :: forall a b. p a b
    -- laws:
    --  proplus a prozero == a = proplus invzero a

-- lower precedence than `#`` which is 0
infixr 0 proplus as ^
infixr 0 proplusfirst as <^
infixr 0 proplussecond as ^>
