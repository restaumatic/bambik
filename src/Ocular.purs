module Ocular where

import Prelude

import Data.Lens (Optic)


-- reminder: type Optic p s t a b = p a b -> p s t
-- traditional optics:
-- type Lens s t a b = forall p. Strong p => Optic p s t a b
-- type Prism s t a b = forall p. Choice p => Optic p s t a b

-- the "inverse" of traditional optics is Ocular:
type Ocular p = forall a b. Optic p a b a b
-- or more generally (Mock?):
type Mock p = forall a b s t. Optic p a b s t

