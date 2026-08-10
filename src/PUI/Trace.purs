-- | **Development diagnostics** for propagation: the emission trace and the
-- | knowledge-gate starvation watchdog. Both are cross-cutting dev facilities
-- | rather than part of the carrier, which is why they live here and not in
-- | `PUI` — the core type module carries no foreign import of its own.
-- |
-- | Nothing here knows about any host, and this module has **no JavaScript at
-- | all**: the two switches and the log sink are all parameters, installed by
-- | a carrier (`setTracing`/`setDiagnostics`/`setSink`) and no-ops until then.
-- | `PUI.Web` installs the browser console and reads the browser's switches at
-- | its mount entries; nothing installs anything under a headless `spago test`,
-- | so a run over `PUI Effect` probes is silent.
module PUI.Trace
  ( Logged
  , Sink
  , tr
  , gateGuard
  , setSink
  , setTracing
  , setDiagnostics
  ) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Unsafe.Coerce (unsafeCoerce)

-- | Whatever a trace line carries, seen opaquely: `tr` is polymorphic in the
-- | logged value but a `Ref` cannot hold a `forall`, so the value crosses to
-- | the sink as this. Declared `foreign import data`, which needs no foreign
-- | *module* — this module has no JavaScript of its own.
foreign import data Logged :: Type

-- | Where diagnostics go. A carrier installs one; until then both are no-ops,
-- | so nothing prints even with the switches on.
type Sink =
  { trace :: String -> Logged -> Effect Unit
  , warn :: String -> Effect Unit
  }

sinkRef :: Ref Sink
sinkRef = unsafePerformEffect (Ref.new { trace: \_ _ -> pure unit, warn: \_ -> pure unit })

-- | Install the sink. `PUI.Web` passes the browser console at its mount
-- | entries; a different host passes whatever it logs to.
setSink :: Sink -> Effect Unit
setSink sink = Ref.write sink sinkRef

tracingRef :: Ref Boolean
tracingRef = unsafePerformEffect (Ref.new false)

diagnosticsRef :: Ref Boolean
diagnosticsRef = unsafePerformEffect (Ref.new false)

-- | Turn the emission trace on or off. Off at startup; a carrier calls this
-- | with whatever its host offers (`PUI.Web` reads `window.__bambikTrace` and
-- | the `bambik-trace` local-storage key).
setTracing :: Boolean -> Effect Unit
setTracing on = Ref.write on tracingRef

-- | Turn starvation warnings on or off. Off at startup, so a carrier that
-- | never opts in — the `Effect` probe carrier the law tests run on — stays
-- | silent.
setDiagnostics :: Boolean -> Effect Unit
setDiagnostics on = Ref.write on diagnosticsRef

-- | Dev-mode emission trace: with `setTracing true`, log every propagation
-- | decision — values flowing between pipeline stages, loop re-feeds and
-- | swallowed echoes, and (most importantly) emissions *withheld* by
-- | knowledge gates, which are otherwise invisible. Zero cost when off beyond
-- | one flag read per emission.
tr :: forall a. String -> a -> Effect Unit
tr tag a = do
  on <- Ref.read tracingRef
  when on do
    sink <- Ref.read sinkRef
    sink.trace tag (unsafeCoerce a)

-- | One-shot **starvation watchdog** for a knowledge gate. Every gated
-- | combinator withholds what it cannot yet complete — correct, but
-- | *silent*: an unprimed gate renders as a blank screen with no
-- | diagnostic. The guard turns that into a self-explaining failure:
-- | `blocked msg` (called on each withheld emission or input) arms a timer
-- | on its first call; if the gate hasn't opened (`fed`) within 3 seconds,
-- | a single console warning prints `msg`, naming the gate and what it is
-- | waiting for. Fires at most once per gate instance, and only under
-- | `setDiagnostics true`.
gateGuard :: Effect { blocked :: String -> Effect Unit, fed :: Effect Unit }
gateGuard = do
  fedRef <- Ref.new false
  armedRef <- Ref.new false
  pure
    { blocked: \msg -> do
        enabled <- Ref.read diagnosticsRef
        armed <- Ref.read armedRef
        when (enabled && not armed) do
          Ref.write true armedRef
          launchAff_ do
            delay (Milliseconds 3000.0)
            liftEffect do
              fed <- Ref.read fedRef
              unless fed do
                sink <- Ref.read sinkRef
                sink.warn msg
    , fed: Ref.write true fedRef
    }
