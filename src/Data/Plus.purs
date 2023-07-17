module Data.Plus
  ( (^)
  , class Plusoid
  , class Plus
  , plus
  , zero
  )
  where

import Prelude hiding (zero)

import Data.Functor.Compose (Compose)
import Data.Invariant (ComposeInvInside, ComposeInvOutside)
import Data.Newtype (unwrap, wrap)

class Plusoid :: forall k. (k -> Type) -> Constraint
class Plusoid i where
    plus :: forall a . i a -> i a -> i a

-- Similar to `Alt` but without `Functor` constraint
class Plus :: forall k. (k -> Type) -> Constraint
class Plusoid i <= Plus i where
    zero :: forall a . i a
    -- laws:
    --  plus a zero == a = plus zero a
    --  plus a (plus b c) == plus (plus a b) c

-- lower precedence than `#`` which is 0
infixr 0 plus as ^

-- If `i _` can be appended, `i (f _)` can be appended too
instance Plusoid i => Plusoid (ComposeInvOutside i f) where
  plus c1 c2 = wrap $ plus (unwrap c1) (unwrap c2)

-- If `i _` has zero, `i (f _)` has zero too
instance Plus i => Plus (ComposeInvOutside i f) where
  zero = wrap zero

instance (Apply f, Plus i) => Plusoid (ComposeInvInside f i) where
  plus c1 c2 = wrap $ plus <$> unwrap c1 <*> unwrap c2

instance (Applicative f, Plus i) => Plus (ComposeInvInside f i) where
  zero = wrap $ pure zero
