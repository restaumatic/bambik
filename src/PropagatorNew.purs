module PropagatorNew where

import Prelude

import Propagator (Change(..), Occurrence(..), Propagation, Propagator(..))
import Unsafe.Coerce (unsafeCoerce)
import Web (Widget)
import Web.Internal.DOM (setTextNodeValue)
import Web.Internal.DOMBuilder (speaker)
import Web.Internal.DOMBuilder as Web.Internal.DOMBuilder

data SafePropagator m i o = SafePropagator
  { speak :: m (Propagation i)
  , listen :: Propagation o -> m Unit
  }

safePropagator :: forall m i o. Apply m => SafePropagator m i o -> Propagator m i o
safePropagator (SafePropagator {speak, listen}) = Propagator \propagationo -> speak <* listen propagationo

-- compare with: instance Monad m => Profunctor (SafePropagator m) where
-- instance Functor m => Profunctor (SafePropagator m) where
--   dimap contraf cof (SafePropagator {listen, speak})= SafePropagator
--     { listen: listen <<< lcmap (map cof)
--     , speak: map (_ <<< map contraf) speak
--     }

instance Apply m => Semigroup (SafePropagator m i i) where
  append (SafePropagator p1) (SafePropagator p2) = SafePropagator {speak, listen}
    where
      speak = p1.speak *> p2.speak
      listen = unsafeCoerce unit

text :: Widget String Void
text = safePropagator $ SafePropagator {listen, speak}
  where
    listen _ = Web.Internal.DOMBuilder.text
    speak = speaker \node -> case _ of
      Occurrence None _ -> mempty
      Occurrence _ string -> setTextNodeValue node string
