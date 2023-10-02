module Data.Profunctor.Plus
  ( (^)
  , class ProfunctorZero
  , class ProfunctorPlus
  , proplus
  , prozero
  )
  where

import Data.Profunctor (class Profunctor)

class Profunctor p <= ProfunctorPlus p where -- TODO EC find fancier name
    proplus :: forall a . p a a -> p a a -> p a a
    -- laws:
    -- proplus a (proplus b c) = proplus (proplus a b) c

class ProfunctorPlus p <= ProfunctorZero p where -- TODO EC find fancier name
    prozero :: forall a. p a a
    -- laws:
    --  proplus a prozero == a = proplus prozero a

-- lower precedence than `#` which is 0
infixr 0 proplus as ^
