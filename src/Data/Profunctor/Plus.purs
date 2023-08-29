module Data.Profunctor.Plus
  ( (<^)
  , (^)
  , (^>)
  , class ProfunctorZero
  , class ProfunctorPlus
  , proplus
  , proplusfirst
  , proplussecond
  , pzero
  )
  where

import Data.Profunctor (class Profunctor)

class Profunctor p <= ProfunctorPlus p where -- TODO EC find fancier name
    proplus :: forall a . p a a -> p a a -> p a a
    proplusfirst :: forall a b c. p a b -> p a c -> p a b
    proplussecond :: forall a b c. p a c -> p a b -> p a b
    -- laws:
    -- proplus a (proplus b c) = proplus (proplus a b) c

class ProfunctorPlus p <= ProfunctorZero p where -- TODO EC find fancier name
    pzero :: forall a b. p a b
    -- laws:
    --  proplus a pzero == a = proplus invzero a

-- lower precedence than `#` which is 0
infixr 0 proplus as ^
infixr 0 proplusfirst as <^
infixr 0 proplussecond as ^>
