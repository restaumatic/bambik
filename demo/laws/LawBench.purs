-- | The leaf-law bench: every component a vocabulary publishes, mounted
-- | alone in its own `<section>` and driven from outside, so the component
-- | laws of `Data.Profunctor.Row` ("The laws", 1 Repetition and 2 Answer)
-- | can be checked against the real DOM leaf rather than a probe.
-- |
-- | Each entry declares its shape and a few sample inputs. The page
-- | exposes `window.__laws[name]` with `feed(k)` — feed sample `k` and
-- | return the emissions made inside that step — and `log`, every channel
-- | emission tagged with the phase it happened in (`registration`,
-- | `feed <k>`, or `between` for anything asynchronous or user-driven).
-- | The assertions live in scripts/smoke/tests/leaf-laws.mjs.
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

-- | `shape` is one of `"×→×"`, `"×→+"`, `"+→×"`, `"+→+"`.
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
