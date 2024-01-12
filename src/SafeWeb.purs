module SafeWeb where

import Prelude

import Control.Monad.State (gets)
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap, wrap)
import Effect.Class (liftEffect)
import Propagator (Change(..), Occurrence(..))
import SafePropagator (SafePropagator)
import Web.Internal.DOM (setTextNodeValue)
import Web.Internal.DOMBuilder (DOMBuilder)
import Web.Internal.DOMBuilder as DOMBuilder

type SafeWidget i o = SafePropagator DOMBuilder i o

text :: String -> SafeWidget String Void
text default = wrap do
  DOMBuilder.text
  node <- gets (_.sibling)
  pure
    { speak: case _ of
      Occurrence None _ -> pure unit
      Occurrence _ mstring -> liftEffect $ setTextNodeValue node $ fromMaybe default mstring
    , listen: \_ -> pure unit
    }

--                Nothing           Just _         default
-- radioButton    deselects         selects        default select provided
-- checkboxInput  deselects         selects        default select provided
-- button         disables          enables
-- textBox        disables          enables
-- element        no-op             no-op
-- ?              detaches          attaches
element :: forall i o. String -> SafeWidget i o -> SafeWidget i o
element tagName = wrap <<< DOMBuilder.element tagName <<< unwrap
