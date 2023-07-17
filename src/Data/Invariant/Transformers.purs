module Data.Invariant.Transformers where

import Prelude

import Data.Invariant (ComposeInvOutside)
import Data.Newtype (unwrap, wrap)

lift ∷ ∀ i f a b. (ComposeInvOutside i f a → ComposeInvOutside i f b) → i (f a) → i (f b)
lift optic ifa = unwrap $ (wrap ifa) # optic

-- allows for e.g.
-- liftAdapter :: forall i f a b. Invariant i => Functor f => (forall j. Invariant j => j a -> j b) -> i (f a) -> i (f b)
-- liftAdapter adapter ifa = lift adapter ifa
--
-- liftLens :: forall i f a b. Cartesian i => Apply f => (forall j. Cartesian j => j a -> j b) -> i (f a) -> i (f b)
-- liftLens lens ifa = lift lens ifa
-- 
-- liftPrism :: forall i f a b. CoCartesian i => CoApply f => (forall j. CoCartesian j => j a -> j b) -> i (f a) -> i (f b)
-- liftPrism prism ifa = lift prism ifa
