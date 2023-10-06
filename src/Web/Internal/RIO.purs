-- This is based on https://pursuit.purescript.org/packages/purescript-specular/0.13.1/docs/Specular.Internal.RIO
module Web.Internal.RIO
  ( RIO(..)
  , runRIO
  , local
  ) where

import Prelude

import Control.Monad.Reader.Class (class MonadAsk)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Uncurried (EffectFn1)
import Unsafe.Coerce (unsafeCoerce)

newtype RIO r a = RIO (EffectFn1 r a)

instance Functor (RIO r) where
  map = mapImpl

instance Apply (RIO r) where
  apply = applyImpl

instance Applicative (RIO r) where
  pure = pureImpl

instance Bind (RIO r) where
  bind = bindImpl

instance Monad (RIO r)

instance MonadAsk r (RIO r) where
  ask = askImpl

instance MonadEffect (RIO r) where
  liftEffect = unsafeCoerce

foreign import pureImpl :: forall r a. a -> RIO r a
foreign import mapImpl :: forall r a b. (a -> b) -> RIO r a -> RIO r b
foreign import applyImpl :: forall r a b. RIO r (a -> b) -> RIO r a -> RIO r b
foreign import bindImpl :: forall r a b. RIO r a -> (a -> RIO r b) -> RIO r b
foreign import askImpl :: forall r. RIO r r
foreign import runRIO :: forall r a. r -> RIO r a -> Effect a
foreign import local :: forall r e a. (e -> r) -> RIO r a -> RIO e a
