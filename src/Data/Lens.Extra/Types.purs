module Data.Lens.Extra.Types where

import Data.Lens (Optic)

-- Reminder: type Optic p s t a b = p a b -> p s t
-- for example:
-- `type Lens s t a b = forall p. Strong p => Optic p s t a b`
-- `type Prism s t a b = forall p. Choice p => Optic p s t a b`
-- notice `forall p` constraint.
-- In contrast we can fix `p` and impose `forall a b` and have:
type Ocular p = forall a b. Optic p a b a b

