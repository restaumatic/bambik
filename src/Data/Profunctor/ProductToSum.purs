module Data.Profunctor.ProductToSum
  ( bind
  , prosum
  , class ProductToSum
  , discard
  )
  where

import Data.Either (Either)
import Data.Profunctor (class Profunctor)
import Data.Tuple (Tuple)
import Data.Unit (Unit, unit)

-- generalization of `Control.Plus.Alt`
class Profunctor p <= ProductToSum p where
  prosum :: forall a b c d . p a b -> p c d -> p (Tuple a c) (Either b d)

-- qualified do notation for `Sum` profunctors
bind ∷ ∀ f a b c d. ProductToSum f ⇒ f a b → (f a b → f c d) → f (Tuple a c) (Either b d)
bind a b = a `prosum` b a

discard ∷ ∀ f a b c d. ProductToSum f ⇒ f a b → (Unit → f c d) → f (Tuple a c) (Either b d)
discard a b = a `prosum` b unit


