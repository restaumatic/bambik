module LawBench
  ( Bench
  , bench
  , runBench
  ) where

import Prelude

import Data.Foldable (for_)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Class (liftEffect)
import PUI (PUI)
import PUI.Web (Node, Web, runDomInNode)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Emitted :: Type

newtype Bench = Bench { name :: String, mount :: Node -> Effect Unit }

foreign import section :: String -> Effect Node
foreign import register :: String -> String -> Array Emitted -> Array (Effect Unit) -> Effect (Emitted -> Effect Unit)

bench :: forall i o. String -> String -> Array i -> PUI Web i o -> Bench
bench name shape samples ui = Bench { name, mount }
  where
  mount node = runDomInNode node (wire :: Web Unit)
  wire = do
    { toUser, fromUser } <- unwrap ui
    liftEffect do
      emit <- register name shape (map unsafeCoerce samples) (map toUser samples)
      fromUser \o -> emit (unsafeCoerce o)

runBench :: Array Bench -> Effect Unit
runBench benches = for_ benches \(Bench { name, mount }) -> section name >>= mount
