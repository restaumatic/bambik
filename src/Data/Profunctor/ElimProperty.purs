module Data.Profunctor.ElimProperty where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (wrap)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Cont (Cont)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)

-- ElimPropertyP

class Profunctor p <= ElimPropertyP p where
  elimPropertyP :: forall s a. p a Void -> p (Tuple a s) s

instance ElimPropertyP (Cont r) where
  elimPropertyP r = wrap \s2r (Tuple a s) -> s2r s


x :: forall a b r. (Either a b -> r) -> a -> r
x f a = f (Left a)

x' :: forall b r. (Either Void b -> r) -> Void -> r
x' f a = f (Left a)

-- what is that?
type X = Variant ()
