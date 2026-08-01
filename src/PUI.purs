-- | The core profunctor UI type and its combinators.
-- |
-- | **The duoidal reading.** `PUI` composes two ways: sequentially
-- | (`Semigroupoid.do`, `⊳` — emissions feed downstream) and in parallel
-- | (the row merges, `⊗` — the input broadcasts to every operand). The two
-- | interact as in a duoidal category: a pipeline can only emulate a merge
-- | through a **comonoid** — a stage that *duplicates* its input onward, not
-- | merely consumes it. `displayed` is exactly that comultiplication (render
-- | *and* forward); `muted` is only the counit (render and discard), which is
-- | why `muted` chrome ahead of a live stage starves it under `⊳` while the
-- | same chrome inside a merge needs nothing. See
-- | doc/collections-profunctor-algebra.md §0.
-- |
-- | **How to read an app.** An app is `mvu seed pipeline`: the pipeline's
-- | stages are composed with `Semigroupoid.do`, every emission travels
-- | left-to-right through the stages, and `mvu` loops the final emission
-- | back to the top — so a stage placed *before* another is not "above" it
-- | semantically; all stages see every model value on the next loop turn.
-- |
-- | A trace of the 7GUIs counter (`display # completed`, then
-- | `button # updates increment`, under `mvu { count: 0 }`):
-- |
-- |  1. registration: the seed `{ count: 0 }` is fed to the first stage;
-- |  2. the display shows `0` and echoes; `completed` widens the echo to
-- |     the full model, which flows on and arms the button's replay value
-- |     and `updates`' retained state;
-- |  3. the user clicks: the button emits, `updates` folds `increment`
-- |     into the retained model and emits `{ count: 1 }`;
-- |  4. the loop re-feeds `{ count: 1 }` to the top; the display re-renders;
-- |     the re-feed's own echoes are swallowed by the loop's re-entrancy
-- |     guard, so exactly one turn happens per event.
-- |
-- | **No nominal types in UI** (doc/no-nominal-types-in-ui.md). A view-model
-- | type is one-off and specific to its UI, so it earns no name: applications
-- | write anonymous Record rows, anonymous Variant rows, and `{}` unit
-- | payloads in place, and this vocabulary never forces a nominal type into
-- | UI code — canonical rows adopted by label, anonymous-record configs,
-- | `{ ms :: Number }` durations. `Array` and `Maybe` are the two generic
-- | containers it is generic over; nominal types live below the UI (recursive
-- | ASTs, `Aff` actions) and enter only as rows projected by business
-- | functions.
module PUI
  ( Action
  , PUI(..)
  , Hooks
  , class Hosting
  , hosting
  , action
  , action'
  , affAdapter
  , accumulated
  , announce
  , bracketed
  , constant
  , constantly
  , debounced
  , debounced'
  , dispatched
  , displayed
  , edits
  , effAdapter
  , every
  , foreach
  , fires
  , looped
  , muted
  , mvu
  , onCase
  , optional
  , resolveFor
  , settled
  , silence
  , spied
  , updates
  , with
  , module Adopters
  , module Seeding
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Lens (Optic)
import Data.Lens.Extra.Types (Ocular)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Map as Map
import Data.Set as Set
import Data.Profunctor (class Profunctor, dimap, lcmap, rmap)
import Data.Profunctor.Acting (class Acting)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Cochoice (class Cochoice)
import Data.Profunctor.Costrong (class Costrong)
import Data.Profunctor.Row.RecordToRecord (class RecordToRecord)
-- the adopter family and its companions, re-exported so demos need the row
-- modules only for the `.do` merges and the trace forms
import Data.Profunctor.Row.RecordToRecord (asField, completed, field, focusRecord, forField, forValue, projection, required, tapped) as Adopters
import Data.Profunctor.Row.RecordToVariant (asCase, toCase) as Adopters
import Data.Profunctor.Row.VariantToRecord (forCase) as Adopters
-- `widenRecordInput` is deliberately NOT re-exported: subsumption is baked
-- into the stages that consume a row (`tapped`, `displayed`, `updates`,
-- `every`, `edits`, `acted`, `completed`), so a widget's own row is always
-- stated by a business function, never coerced at the call site. It stays
-- exported from `Data.Profunctor.Row` as the merge instances' plumbing.
import Data.Profunctor.Acting (acted, optioned) as Adopters
import Data.Profunctor.Seeding (class Seeding, seeded)
import Data.Profunctor.Seeding (class Seeding, seeded) as Seeding
import Data.Profunctor.Row.RecordToVariant (class RecordToVariant, class Resolving, class Coresolving)
import Data.Profunctor.Row (class OwnedRecordOutputs, class OwnedVariantInputs, class SharedRecordInputs, exactRow, rowLabels, widenRecordInput, widenVariantOutput)
import Data.String (joinWith)
import Data.Profunctor.Row.VariantToRecord (class VariantToRecord, class Retaining, class Coretaining)
import Data.Profunctor.Row.VariantToVariant (class VariantToVariant)
import Data.Profunctor.Strong (class Strong)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for, sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant, case_, contract, on)
import Prim.Row (class Cons, class Lacks, class Nub, class Union)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Debug (class DebugWarning, spy)
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (Aff, delay, error, forkAff, killFiber, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Record (get, insert, merge, union) as Record

-- | Dev-mode emission trace: set `window.__bambikTrace = true` (or
-- | `localStorage.setItem("bambik-trace", "true")`) in the browser console
-- | and reload to log every propagation decision — values flowing between
-- | pipeline stages, loop re-feeds and swallowed echoes, and (most
-- | importantly) emissions *withheld* by knowledge gates, which are
-- | otherwise invisible. Zero cost when off beyond one flag check per
-- | emission.
foreign import traceEnabled :: Effect Boolean
foreign import traceImpl :: forall a. String -> a -> Effect Unit
foreign import diagnosticsEnabled :: Effect Boolean
foreign import warnImpl :: String -> Effect Unit

tr :: forall a. String -> a -> Effect Unit
tr tag a = do
  on <- traceEnabled
  when on $ traceImpl tag a

-- | One-shot **starvation watchdog** for a knowledge gate. Every gated
-- | combinator withholds what it cannot yet complete — correct, but
-- | *silent*: an unprimed gate renders as a blank screen with no
-- | diagnostic. The guard turns that into a self-explaining failure:
-- | `blocked msg` (called on each withheld emission or input) arms a timer
-- | on its first call; if the gate hasn't opened (`fed`) within 3 seconds,
-- | a single console warning prints `msg`, naming the gate and what it is
-- | waiting for. Fires at most once per gate instance; browser-only
-- | (silent under Node), opt out with `window.__bambikNoWarn = true`.
gateGuard :: Effect { blocked :: String -> Effect Unit, fed :: Effect Unit }
gateGuard = do
  fedRef <- Ref.new false
  armedRef <- Ref.new false
  pure
    { blocked: \msg -> do
        enabled <- diagnosticsEnabled
        armed <- Ref.read armedRef
        when (enabled && not armed) do
          Ref.write true armedRef
          launchAff_ do
            delay (Milliseconds 3000.0)
            liftEffect do
              fed <- Ref.read fedRef
              unless fed $ warnImpl msg
    , fed: Ref.write true fedRef
    }

renderFieldNames :: Array String -> String
renderFieldNames [] = "{}"
renderFieldNames ls = "{ " <> joinWith ", " ls <> " }"

-- could it be: newtype PUI m i o = PUI ((o -> Effect Unit) -> m (i -> Effect Unit))
newtype PUI m i o = PUI (m
  { toUser :: i -> Effect Unit
  , fromUser :: (o -> Effect Unit) -> Effect Unit
  })

derive instance Newtype (PUI m i o) _

instance Functor m => Profunctor (PUI m) where
  dimap pre post p = wrap ado
    p' <- unwrap p
    in
      { toUser: pre >>> p'.toUser
      , fromUser: \prop -> p'.fromUser (prop <<< post)
      }

-- Stateful instances below share one gating principle (the same one the
-- record merges follow): state a widget hasn't received yet cannot be
-- fabricated, so emissions needing it are withheld until
-- the state channel has been fed.
instance Functor m => Strong (PUI m) where
  first p = wrap ado
    p' <- unwrap p
    in
      -- ref per build (inside the applicative's result), NOT in an ado
      -- statement: a statement-position let is evaluated once per PUI
      -- value, so every `foreach` row would share one ref
      let lastab = unsafePerformEffect $ Ref.new Nothing
          guard = unsafePerformEffect gateGuard
      in
      { toUser: \ab -> do
          guard.fed
          Ref.write (Just ab) lastab
          p'.toUser $ fst ab
      , fromUser: \prop ->
          p'.fromUser \b -> do
            mab <- Ref.read lastab
            case mab of
              Nothing -> do
                guard.blocked "Strong.first: emissions dropped for 3s — the pair state was never fed (no input arrived), so the gate cannot complete a Tuple. Feed the stage a first value."
                tr "Strong.first: emission withheld (pair state unknown)" b
              Just prevab -> prop (Tuple b (snd prevab))
      }
  second p = wrap ado
    p' <- unwrap p
    in
      let lastab = unsafePerformEffect $ Ref.new Nothing
          guard = unsafePerformEffect gateGuard
      in
      { toUser: \ab -> do
          guard.fed
          Ref.write (Just ab) lastab
          p'.toUser $ snd ab
      , fromUser: \prop ->
          p'.fromUser \b -> do
            mab <- Ref.read lastab
            case mab of
              Nothing -> do
                guard.blocked "Strong.second: emissions dropped for 3s — the pair state was never fed (no input arrived), so the gate cannot complete a Tuple. Feed the stage a first value."
                tr "Strong.second: emission withheld (pair state unknown)" b
              Just prevab -> prop (Tuple (fst prevab) b)
      }

instance Functor m => Choice (PUI m) where
  left p = wrap ado
    p' <- unwrap p
    in
      let mPropRef = unsafePerformEffect $ Ref.new Nothing
      in
      { toUser: case _ of
        Right c -> do
          mProp <- Ref.read mPropRef
          for_ mProp \prop -> prop (Right c)
        Left a -> p'.toUser a
      , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        p'.fromUser \b -> prop (Left b)
      }
  right p = wrap ado
    p' <- unwrap p
    in
      let mPropRef = unsafePerformEffect $ Ref.new Nothing
      in
      { toUser: case _ of
        Left c -> do
          mProp <- Ref.read mPropRef
          for_ mProp \prop -> prop (Left c)
        Right a -> p'.toUser a
      , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        p'.fromUser \b -> prop (Right b)
      }

-- | The `×`-diagonal **trace** (dual of `Strong`): the `c` a widget emits is
-- | retained and paired with its next input — feedback of **state**.
-- | Knowledge-gated like every stateful instance: inputs are withheld until a
-- | first `c` exists, so the loop needs priming — route the initial state in
-- | through the widget's input where possible, or use `looped` for the
-- | self-feeding diagonal special case, which has no gate. The retraction law
-- | `unfirst (first g) ≅ g` holds once the state channel is primed.
instance Functor m => Costrong (PUI m) where
  unfirst p = wrap ado
    p' <- unwrap p
    in
      let cRef = unsafePerformEffect $ Ref.new Nothing
          guard = unsafePerformEffect gateGuard
      in
      { toUser: \a -> do
          mc <- Ref.read cRef
          case mc of
            Nothing -> do
              guard.blocked "Costrong.unfirst: inputs dropped for 3s — the state feedback channel was never primed (the traced widget never emitted). Use `feedback`, which takes the traced chain's initial state as an argument, or seed a raw `unfirst`/`colens` chain from inside (`seeded`)."
              tr "Costrong.unfirst: input withheld (state unprimed)" a
            Just c -> p'.toUser $ Tuple a c
      , fromUser: \prop ->
          p'.fromUser \(Tuple b c) -> do
            guard.fed
            Ref.write (Just c) cRef
            prop b
      }
  unsecond p = wrap ado
    p' <- unwrap p
    in
      let aRef = unsafePerformEffect $ Ref.new Nothing
          guard = unsafePerformEffect gateGuard
      in
      { toUser: \b -> do
          ma <- Ref.read aRef
          case ma of
            Nothing -> do
              guard.blocked "Costrong.unsecond: inputs dropped for 3s — the state feedback channel was never primed (the traced widget never emitted). Use `feedback`, which takes the traced chain's initial state as an argument, or seed a raw chain from inside (`seeded`)."
              tr "Costrong.unsecond: input withheld (state unprimed)" b
            Just a -> p'.toUser $ Tuple a b
      , fromUser: \prop ->
          p'.fromUser \(Tuple a c) -> do
            guard.fed
            Ref.write (Just a) aRef
            prop c
      }

-- | The `+`-diagonal **trace** (dual of `Choice`): a looped-branch emission
-- | re-enters the widget as input — feedback of **control**, i.e. iteration —
-- | until an exit-branch emission passes through. The re-entry is a `toUser`,
-- | so in `PUI` the loop is an *event* loop: it advances on the widget's next
-- | emission (variant-output widgets do not echo, so the leaf protocol cannot
-- | provoke a synchronous spin). Retraction law: `unleft (left g) ≅ g`.
instance Functor m => Cochoice (PUI m) where
  unleft p = wrap $ unwrap p <#> \p' ->
    { toUser: \a -> p'.toUser $ Left a
    , fromUser: \prop -> p'.fromUser case _ of
        Left b -> prop b
        Right c -> do
          tr "Cochoice.unleft: looping back" c
          p'.toUser $ Right c
    }
  unright p = wrap $ unwrap p <#> \p' ->
    { toUser: \a -> p'.toUser $ Right a
    , fromUser: \prop -> p'.fromUser case _ of
        Right b -> prop b
        Left c -> do
          tr "Cochoice.unright: looping back" c
          p'.toUser $ Left c
    }

-- | The `× → +` **co-strength** (retraction of `Resolving`): a `Right c`
-- | emission is retained as the fold state and — **eagerly** — re-fed to
-- | the widget joined with the last input (guarded), so the widget
-- | re-renders at every fold step; a `Left b` exits. Gated like `Costrong`
-- | (a first `c` must arrive before inputs pass — `announce` an initial
-- | state to prime it); `coresolve (resolve g) ≅ g` once primed.
instance Functor m => Coresolving (PUI m) where
  coresolve p = wrap ado
    p' <- unwrap p
    in
      let aRef = unsafePerformEffect $ Ref.new Nothing
          cRef = unsafePerformEffect $ Ref.new Nothing
          busyRef = unsafePerformEffect $ Ref.new false
          guard = unsafePerformEffect gateGuard
      in
      { toUser: \a -> do
          Ref.write (Just a) aRef
          mc <- Ref.read cRef
          case mc of
            Nothing -> do
              guard.blocked "Coresolving.coresolve: inputs dropped for 3s — the fold state was never primed (no loop-branch emission arrived). Use `folding`, which takes the fold's initial state as an argument, or seed a raw `coshutter` chain's loop branch (`seeded`)."
              tr "Coresolving.coresolve: input withheld (fold state unprimed)" a
            Just c -> p'.toUser $ Tuple a c
      , fromUser: \prop -> p'.fromUser case _ of
          Left b -> prop b
          Right c -> do
            guard.fed
            tr "Coresolving.coresolve: fold step, re-feeding" c
            Ref.write (Just c) cRef
            busy <- Ref.read busyRef
            unless busy do
              Ref.write true busyRef
              ma <- Ref.read aRef
              for_ ma \a -> p'.toUser $ Tuple a c
              Ref.write false busyRef
      }

-- | The `+ → ×` **co-strength** (retraction of `Retaining`): every emission
-- | `Tuple b c` yields `b` and immediately re-enters the widget as a
-- | `Right c` resume — a **productive unfold**/generator.
-- | `coretain (retain g) ≅ g` once the state channel is primed.
instance Functor m => Coretaining (PUI m) where
  coretain p = wrap $ unwrap p <#> \p' ->
    -- the resume re-entry is guarded: a record-output widget echoes on
    -- `toUser`, and an unguarded re-feed would loop on its own echo
    let busyRef = unsafePerformEffect $ Ref.new false
    in
      { toUser: \a -> p'.toUser $ Left a
      , fromUser: \prop -> p'.fromUser \(Tuple b c) -> do
          prop b
          busy <- Ref.read busyRef
          unless busy do
            tr "Coretaining.coretain: resuming with state" c
            Ref.write true busyRef
            p'.toUser $ Right c
            Ref.write false busyRef
      }

instance Apply m => Semigroupoid (PUI m) where
  compose p2 p1 = wrap ado
    p1' <- unwrap p1
    p2' <- unwrap p2
    in
      { toUser: \cha -> do
        p1'.toUser cha
      , fromUser: \prop -> do
          -- downstream registers first: emissions fired during upstream's
          -- registration (announcements, seeds) must find downstream's
          -- wiring already listening
          p2'.fromUser prop
          p1'.fromUser \x -> do
            tr "stage → stage" x
            p2'.toUser x
      }

-- | `identity` forwards its input straight to its output: a wire. The unit
-- | of `compose` (up to effect timing), the element the diagonal unary laws
-- | pin, and — at `{}` — an alternative `RecordToRecord.pempty`.
instance Applicative m => Category (PUI m) where
  -- the ref is created per unwrap (inside the functor map), NOT in a
  -- top-level `let`: `identity` is a constant, and a constant's `let` is
  -- evaluated once — every `identity` in the app would share one wire
  identity = wrap $ pure unit <#> \_ ->
    let mPropRef = unsafePerformEffect $ Ref.new Nothing
    in
      { toUser: \ch -> do
          mProp <- Ref.read mPropRef
          for_ mProp \prop -> prop ch
      , fromUser: \prop -> Ref.write (Just prop) mPropRef
      }

-- | The silent widget: shows nothing, captures nothing — at ANY types, and
-- | necessarily so (parametricity: `forall i o. p i o` can neither inspect an
-- | `i` nor fabricate an `o`). The pinned trivial operand of the mixed
-- | introduce laws, the implementation of `pempty` at the variant-output
-- | directions (where silence is forced), and the terminal sink of data-flow
-- | pipelines.
-- |
-- | Not primitive — the `dimap`-closure of the `× → +` unit (the one unit
-- | with record input and variant output, so the one that repolarizes):
-- |
-- | ```
-- | silence = dimap (const {}) case_ RecordToVariant.pempty
-- | ```
-- |
-- | Implemented directly, as elsewhere laws are stated and bodies stay lean.
silence :: forall m i o. Applicative m => PUI m i o
silence = wrap $ pure
  { toUser: mempty
  , fromUser: mempty
  }

-- | The **announcing constant**: silent except for one emission of `o` at
-- | registration — the value-level generalization of the record units'
-- | `{}` announcement (`Web.staticText`'s protocol, with a payload). As a
-- | merge operand it seeds fields or cases; composed in front of a widget
-- | it discharges the widget's initial-state obligation (`with`'s
-- | implementation) — announcing an initial state the way `pempty`
-- | announces its informationless `{}`.
announce :: forall m o. Applicative m => o -> PUI m {} o
announce o = wrap $ pure
  { toUser: mempty
  , fromUser: \prop -> prop o
  }

-- | **Discharge a widget's initial-state obligation**: `with a w` supplies
-- | `w`'s input its t=0 value — the entity `w` edits exists from the very
-- | beginning, and `a` is its initial state — leaving nothing to feed
-- | (`with a w = announce a >>> w`, so `with a identity = announce a`).
-- | The residual input row of a pipeline is exactly what is *not yet known*
-- | at t=0; `with` (and `mvu`, its looping sibling) turns that obligation
-- | into `{}`, the one self-pointed record — the type `body` demands. The
-- | standalone app reads `body $ with initial $ ...`. For a pass-through
-- | seeding *stage* (feed once, then keep forwarding inputs), use the
-- | `seeded` wire directly: `seeded a >>> w`.
with :: forall m a b. Applicative m => a -> PUI m a b -> PUI m {} b
with a w = announce a >>> w

-- | The **seeded echo wire** (the `Seeding` instance): `identity`'s
-- | pass-through plus one emission of the seed at registration — the
-- | pointedness primitive the knot-tying row forms (`feedback`/`folding`/
-- | `unfolding`) compose into their traced chains to prime the state
-- | channel with its declared initial value.
instance Applicative m => Seeding (PUI m) where
  seeded a = wrap $ pure unit <#> \_ ->
    -- ref per unwrap, like `identity` (a shared `let` would wire all
    -- instantiations together)
    let mPropRef = unsafePerformEffect $ Ref.new Nothing
    in
      { toUser: \ch -> do
          mProp <- Ref.read mPropRef
          for_ mProp \prop -> prop ch
      , fromUser: \prop -> do
          Ref.write (Just prop) mPropRef
          prop a
      }

-- | The **Mealy update stage** on the `×`-diagonal: a pass-through wire
-- | (every value fed flows on, so ticks and edits upstream keep driving
-- | the loop) that retains the last value and, on each *event* emission of
-- | the wrapped widget, folds it in and emits the updated value. Event
-- | widgets emit **bare payloads** — no smuggling the model through event
-- | cases, no pass-through `state` case in the event merge:
-- |
-- | ```
-- | looped Semigroupoid.do
-- |   form                                   -- ×→× editors
-- |   updates handle RecordToVariant.do ...  -- ×→+ events, bare payloads
-- | ```
-- |
-- | is the model–view–update shape as two named stages. Events arriving
-- | before a first value are withheld (the usual knowledge gate).
-- |
-- | **Both sides subsume** (the row layer's rule: a stated closed row may be
-- | *read* from any wider row): the handler may touch a sub-row of the model,
-- | and the wrapped event widget may be fed a sub-row of it — typically the
-- | union of an event merge's operands — so neither side needs a
-- | `widenRecordInput` at the stage boundary. With `small ≡ big` and
-- | `narrow ≡ big` this is the plain diagonal stage.
updates
  :: forall m small u big narrow extra e
   . Functor m
  => Union small big u
  => Nub u big
  => Union narrow extra big
  => (e -> Record small -> Record small)
  -> PUI m (Record narrow) e
  -> PUI m (Record big) (Record big)
updates handler w = mealy (\e big -> Record.merge (handler e (unsafeCoerce big)) big) (widenRecordInput w)

-- | The type-agnostic Mealy stage `updates` and `displayed` are built from —
-- | private, because the vocabulary's stages carry rows (subsumption is
-- | stated in their signatures) while this one is exact at any type.
mealy :: forall m s e. Functor m => (e -> s -> s) -> PUI m s e -> PUI m s s
mealy handler events = wrap $ unwrap events <#> \evts ->
  let sRef = unsafePerformEffect $ Ref.new Nothing
      mPropRef = unsafePerformEffect $ Ref.new Nothing
      guard = unsafePerformEffect gateGuard
  in
    { toUser: \s -> do
        guard.fed
        Ref.write (Just s) sRef
        evts.toUser s
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop s
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        evts.fromUser \e -> do
          ms <- Ref.read sRef
          case ms of
            Nothing -> do
              guard.blocked "updates: an event was dropped and no model has arrived for 3s — the update stage has no retained state to fold into. Seed the pipeline (`with initial`/`mvu seed`)."
              tr "updates: event withheld (no retained state yet)" e
            Just s -> do
              tr "updates: folding event" e
              let s' = handler e s
              Ref.write (Just s') sRef
              prop s'
    }

-- | Make any display an **unconditional pass-through stage**: every value
-- | fed is shown and forwarded, no echo required. The honest wrapper for
-- | displays that cannot echo — `foreach`/`foreachWith` collections (silent on
-- | an empty array, so inside a gated merge they starve the gate, and as a
-- | `mvu` pipeline's last stage they kill the loop). `tapped` and `completed`
-- | both rely on the display's echo;
-- | `displayed` does not. (The trivial `updates` fold: any event the
-- | wrapped widget does emit re-emits the retained value.)
-- | **Subsumption is built in** (like `tapped`): the display may read a
-- | *narrower* row than the stage carries, so a closed-row projection needs
-- | no `widenRecordInput` at the stage boundary.
displayed :: forall m narrow extra wider e. Functor m => Union narrow extra wider => PUI m { | narrow } e -> PUI m { | wider } { | wider }
displayed w = mealy (\_ s -> s) (widenRecordInput w)

-- | Embed `{}`-typed chrome at ANY position: the wrapped widget is fed
-- | `{}` for every value flowing through and its emissions (the statics'
-- | registration announcement) are dropped, so static chrome fits a live
-- | slot — `drawer config (muted staticNav) content` — without touching
-- | the slot's types.
muted :: forall m b i o. Functor m => PUI m {} b -> PUI m i o
muted w = wrap $ unwrap w <#> \w' ->
  { toUser: \_ -> w'.toUser {}
  , fromUser: \_ -> w'.fromUser \_ -> pure unit
  }

-- | Pin a stage's input to a known value: the wrapped widget is fed `a` for
-- | every value flowing through — a constant-fed stage (a fixed catalogue
-- | driving a collection component) with no input-type annotation. Its own
-- | input type stays free, so it fits any pipeline position.
constantly :: forall m a i o. Functor m => a -> PUI m a o -> PUI m i o
constantly a = lcmap (const a)

-- | Fire the **business outcome** of what the emitter was shown: adopt the
-- | canonical click case by applying `f` to its payload. Where `asCase @l`
-- | renames the event and leaves the payload alone, `fires` dissolves the
-- | event into the outcome `f` computes — typically a variant of business
-- | results: `button { label: "Sign up" } # fires register` emits
-- | `register`'s cases directly.
fires :: forall m i a o. Functor m => (a -> o) -> PUI m i [ clicked :: a ] -> PUI m i o
fires f = rmap (on (Proxy @"clicked") f case_)

-- | Settle a stage's emissions through a **total, type-preserving**
-- | normalization — guardrail A7's mechanism made a word: a lossy
-- | adjustment belongs in the model, after `completed`, where the loop
-- | makes it a transaction — `formula # completed # settled commit`.
-- | Type-preservation is the contract: `settled` normalizes, it cannot
-- | re-shape.
settled :: forall m i o. Functor m => (o -> o) -> PUI m i o -> PUI m i o
settled = rmap

-- | The **variant-editor bracket**: adopt a record-shaped editor ensemble
-- | (every case's payload retained) as an editor of one-at-a-time variant
-- | state — `stateOf` brackets the variant in (seeding absent payloads
-- | from the retained editor state), `caseOf` projects the selection back
-- | out, and the self-trace in between keeps the ensemble consistent. The
-- | demos' variant editors read
-- | `(RecordToRecord.do …) # bracketed fulfillmentState fulfillmentCase # field @l`.
bracketed :: forall m i s o. Functor m => (i -> s) -> (s -> o) -> PUI m s s -> PUI m i o
bracketed f g w = dimap f g (looped w)

-- | Mark a type-changing selector as **possibly unselected** — the dual of
-- | `required`. The selection state is an entity always known from the
-- | input, *including* "nothing picked yet", so `optional` completes the
-- | selector's `Just`-only leaf echo with the missing `Nothing` half (fed
-- | `Nothing` it announces `Nothing`; fed `Just` the leaf's own echo
-- | speaks — exactly one echo per feed either way) and wraps every user
-- | pick in `Just`. The model keeps the `Maybe`: an unmade choice flows as
-- | honest knowledge instead of starving the merge gate, and only a
-- | genuine pick can ever produce the bare value —
-- | `dropdown config options # optional # asField @l` seeds as `Nothing`
-- | and the stages demanding the selection stay `provided`-gated until the
-- | user picks.
optional :: forall m a. Functor m => PUI m { value :: Maybe a } { value :: a } -> PUI m { value :: Maybe a } { value :: Maybe a }
optional p = wrap $ unwrap p <#> \p' ->
  let mPropRef = unsafePerformEffect $ Ref.new Nothing
  in
    { toUser: \i -> do
        p'.toUser i
        case i.value of
          Nothing -> do
            mProp <- Ref.read mPropRef
            for_ mProp \prop -> prop { value: Nothing }
          Just _ -> pure unit
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        p'.fromUser \o -> prop { value: Just o.value }
    }

-- | The **heartbeat wire**: `identity`'s pass-through plus a periodic step.
-- | Retains the last value flowing through; every `interval`, applies
-- | `step` to it — `Just` advances (retained and emitted), `Nothing`
-- | pauses until fresh input arrives. Inside a `looped` chain this is a
-- | tick source: the 7GUIs Timer is `every { ms: 100.0 } tick`.
-- | The loop runs for the widget's whole life (no cancellation — a
-- | prototype limitation shared with `action'`).
-- |
-- | The step **subsumes** (like `updates`'s handler): it may read and rebuild
-- | a sub-row of the model, merged back over the last full value on each
-- | tick, so the tick's footprint is stated once in the step's own signature.
every
  :: forall m small u big
   . Applicative m
  => Union small big u
  => Nub u big
  => { ms :: Number }
  -> (Record small -> Maybe (Record small))
  -> PUI m (Record big) (Record big)
every interval step = heartbeat interval \big -> (\s -> Record.merge s big) <$> step (unsafeCoerce big)

-- | The type-agnostic heartbeat `every` is built from — private for the same
-- | reason as `mealy`.
heartbeat :: forall m a. Applicative m => { ms :: Number } -> (a -> Maybe a) -> PUI m a a
heartbeat interval step = wrap $ pure unit <#> \_ ->
  let lastRef = unsafePerformEffect $ Ref.new Nothing
      mPropRef = unsafePerformEffect $ Ref.new Nothing
  in
    { toUser: \a -> do
        Ref.write (Just a) lastRef
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop a
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        let
          loop = do
            delay (Milliseconds interval.ms)
            liftEffect do
              ma <- Ref.read lastRef
              for_ (ma >>= step) \a' -> do
                Ref.write (Just a') lastRef
                prop a'
            loop
        launchAff_ loop
    }

-- | The `×`-diagonal **self-trace**: feed a diagonal widget its own
-- | emissions, re-entrancy-guarded (leaf widgets echo on `toUser`, and the
-- | guard swallows the echoes the re-feed provokes). Wrapped around a record
-- | merge it supplies the sibling cross-feed the gated merge deliberately
-- | omits — every operand sees every emission re-broadcast, and per-operand
-- | *retention* falls out of the merge gates (each gate holds its side's
-- | last contribution). Primitive rather than derived: `Costrong`'s
-- | `unfirst` cannot self-feed (no `c` before the first emission, no
-- | emission before the first input — the gate deadlocks), so the
-- | self-feeding special case ties the knot directly.
looped :: forall m a. Functor m => PUI m a a -> PUI m a a
looped p = wrap $ unwrap p <#> \p' ->
  let busyRef = unsafePerformEffect $ Ref.new false
  in
    { toUser: p'.toUser
    , fromUser: \prop ->
        p'.fromUser \u -> do
          busy <- Ref.read busyRef
          if busy
            then tr "looped: echo swallowed (re-entrant)" u
            else do
              tr "looped: re-feeding emission" u
              Ref.write true busyRef
              p'.toUser u
              Ref.write false busyRef
              prop u
    }

-- | The model–view–update shape, named: `mvu seed w = with seed (looped w)`.
-- | `w` is a same-type pipeline over the model — editors (`# completed`
-- | where they don't produce the whole model), displays, wires (`every`),
-- | and event stages folded in with `updates`. The model is an **entity**:
-- | it exists from the very beginning with a known initial state, and
-- | `seed` is that state — fed once at registration; from then on every
-- | emission of any stage re-enters at the top, re-entrancy-guarded. The
-- | result is **closed** (input `{}`): supplying the seed discharges the
-- | pipeline's initial-state obligation, which is what `body` demands. The
-- | standalone app reads `body $ ... $ mvu seed pipeline`.
mvu :: forall m model. Applicative m => model -> PUI m model model -> PUI m {} model
mvu seed w = with seed (looped w)

instance Applicative m => RecordToRecord (PUI m) where
  -- the unit announces its informationless {} once, so the merge gates below
  -- never starve against it
  pempty = wrap $ pure
    { toUser: mempty
    , fromUser: \prop -> prop {}
    }
  recordToRecord = recordToRecordPUI

-- Hoisted so the merge's `RowList` variables are in scope: the starvation
-- diagnostics reify each side's field names (`rowLabels`, a
-- `MergeableRecords` superclass) to say exactly which sibling fields a
-- withholding gate is still waiting for.
recordToRecordPUI :: forall m i1 o1 i2 o2 i12 i1x i2x i o o1l o2l.
  Applicative m =>
  SharedRecordInputs i1 i2 i i12 i1x i2x =>
  OwnedRecordOutputs o1 o2 o o1l o2l =>
  PUI m { | i1 } { | o1 } -> PUI m { | i2 } { | o2 } -> PUI m { | i } { | o }
recordToRecordPUI p1 p2 = wrap ado
  p1' <- unwrap (widenRecordInput p1)
  p2' <- unwrap (widenRecordInput p2)
  in
    let p1Last = unsafePerformEffect $ Ref.new Nothing
        p2Last = unsafePerformEffect $ Ref.new Nothing
        guard1 = unsafePerformEffect gateGuard
        guard2 = unsafePerformEffect gateGuard
        fields1 = renderFieldNames (rowLabels (Proxy @o1l))
        fields2 = renderFieldNames (rowLabels (Proxy @o2l))
        starving mine sibling = "×→× record merge: emissions dropped for 3s — the operand producing " <> mine
          <> " keeps emitting, but its sibling operand producing " <> sibling
          <> " never has, so the merged record cannot complete. Prime the silent operand (`seeded`/`announce`) or check that it renders at all."
    in
    { toUser: \new -> do
          p1'.toUser new
          p2'.toUser new
    , fromUser: \prop -> do
        p1'.fromUser \partial -> do
          -- runtime-exactness: trim to the declared output row, so stale
          -- runtime copies of sibling fields (echo wires, lens rebuilds
          -- over the widening-coerced input) never shadow the other
          -- side's genuine contribution in the left-biased union
          let exact = exactRow partial
          let _ = unsafePerformEffect $ Ref.write (Just exact) p1Last
          let mp2 = unsafePerformEffect $ Ref.read p2Last
          case mp2 of
            Nothing -> do
              guard1.blocked $ starving fields1 fields2
              tr ("merge ×→×: contribution withheld (sibling fields " <> fields2 <> " not heard from yet)") exact
            Just p2val -> do
              guard1.fed *> guard2.fed
              prop $ Record.union exact p2val
        p2'.fromUser \partial -> do
          let exact = exactRow partial
          let _ = unsafePerformEffect $ Ref.write (Just exact) p2Last
          let mp1 = unsafePerformEffect $ Ref.read p1Last
          case mp1 of
            Nothing -> do
              guard2.blocked $ starving fields2 fields1
              tr ("merge ×→×: contribution withheld (sibling fields " <> fields1 <> " not heard from yet)") exact
            Just p1val -> do
              guard1.fed *> guard2.fed
              prop $ Record.union p1val exact
    }

instance Applicative m => RecordToVariant (PUI m) where
  pempty = silence
  recordToVariant p1 p2 = wrap ado
    p1' <- unwrap (widenVariantOutput (widenRecordInput p1))
    p2' <- unwrap (widenVariantOutput (widenRecordInput p2))
    in
      { toUser: \new -> do
          p1'.toUser new
          p2'.toUser new
      , fromUser: \prop -> do
          p1'.fromUser prop
          p2'.fromUser prop
      }

-- | The loop step, with transiency **derived from time**: the input
-- | `Tuple a c` shows `a` to the inner widget and retains `c`. Every
-- | emission of the inner widget loops immediately — `Right c`, the
-- | retained state escapes (withheld until a first `c` exists) — and
-- | (re)arms a quiescence timer; when the widget stays quiet for the
-- | window, the last emission resolves: `Left b`. **Loop = still moving,
-- | Done = quiescence** — which is the definition of debouncing, so the
-- | retraction law refines to `coresolve (resolve g) = debounced g ≅ g`
-- | up to time (once primed). The window is `resolveFor`'s parameter;
-- | the instance uses the same default as `debounced`.
instance Functor m => Resolving (PUI m) where
  resolve = resolveFor { ms: 300.0 }

-- | `resolve` with an explicit quiescence window — see the `Resolving`
-- | instance. `Done` needs no state and fires (after the window) even
-- | unprimed; only the `Loop` branch is gated on a first `c`.
resolveFor :: forall m a b c. Functor m => { ms :: Number } -> PUI m a b -> PUI m (Tuple a c) (Either b c)
resolveFor millis p = wrap ado
  p' <- unwrap p
  in
    let cRef = unsafePerformEffect $ Ref.new Nothing
        mFiberRef = unsafePerformEffect $ Ref.new Nothing
        guard = unsafePerformEffect gateGuard
    in
    { toUser: \(Tuple a c) -> do
        guard.fed
        Ref.write (Just c) cRef
        p'.toUser a
    , fromUser: \prop ->
        p'.fromUser \b -> do
          -- (re)arm the quiescence timer: a newer emission supersedes the
          -- pending Done, so only the last value of a burst resolves
          launchAff_ do
            mFiber <- liftEffect $ Ref.read mFiberRef
            for_ mFiber $ killFiber (error "Superseded by a newer emission")
            newFiber <- forkAff do
              delay (Milliseconds millis.ms)
              liftEffect $ prop $ Left b
            liftEffect $ Ref.write (Just newFiber) mFiberRef
          -- loop immediately with the retained state
          mc <- Ref.read cRef
          case mc of
            Nothing -> do
              guard.blocked "Resolving.resolve: loop-branch emissions dropped for 3s — no input has primed the retained state (only quiescence resolutions pass). Feed the stage a first value."
              tr "Resolving.resolve: loop branch withheld (state unprimed)" b
            Just c -> prop $ Right c
    }

-- | The Mealy step: a fresh `Left a` feeds the inner widget, a `Right c`
-- | (re)places the retained state. When the inner widget emits `b`, the
-- | output pairs it with the retained `c` — and is **withheld until a `c`
-- | has arrived** (a `Tuple b c` with unknown `c` would be a fabrication),
-- | mirroring the knowledge-gated record merges.
instance Functor m => Retaining (PUI m) where
  retain p = wrap ado
    p' <- unwrap p
    in
      let cRef = unsafePerformEffect $ Ref.new Nothing
          guard = unsafePerformEffect gateGuard
      in
      { toUser: case _ of
          Left a -> p'.toUser a
          Right c -> do
            guard.fed
            Ref.write (Just c) cRef
      , fromUser: \prop ->
          p'.fromUser \b -> do
            mc <- Ref.read cRef
            case mc of
              Nothing -> do
                guard.blocked "Retaining.retain: emissions dropped for 3s — the retained state was never fed (no state-case input arrived), so the gate cannot complete a Tuple. Prime the state channel: `unfolding` takes the unfold's initial state as an argument and feeds it as a first resume; raw chains seed the state case (`seeded`/`announce`)."
                tr "Retaining.retain: emission withheld (state unprimed)" b
              Just c -> prop $ Tuple b c
      }

instance Applicative m => VariantToRecord (PUI m) where
  pempty = wrap $ pure
    { toUser: mempty
    , fromUser: \prop -> prop {}
    }
  variantToRecord = variantToRecordPUI

-- Hoisted like `recordToRecordPUI`, for the same reason: the starvation
-- diagnostics name the sibling fields a withholding gate is waiting for.
variantToRecordPUI :: forall m i1 i1l i2 i2l o1 o2 i o o1l o2l.
  Applicative m =>
  OwnedVariantInputs i1 i2 i i1l i2l =>
  OwnedRecordOutputs o1 o2 o o1l o2l =>
  PUI m [ | i1 ] { | o1 } -> PUI m [ | i2 ] { | o2 } -> PUI m [ | i ] { | o }
variantToRecordPUI p1 p2 = wrap ado
  p1' <- unwrap p1
  p2' <- unwrap p2
  in
    -- gate like `recordToRecord`: hold propagation until both sides' fields
    -- are known (each operand emits its complete sub-record)
    let p1Last = unsafePerformEffect $ Ref.new Nothing
        p2Last = unsafePerformEffect $ Ref.new Nothing
        guard1 = unsafePerformEffect gateGuard
        guard2 = unsafePerformEffect gateGuard
        fields1 = renderFieldNames (rowLabels (Proxy @o1l))
        fields2 = renderFieldNames (rowLabels (Proxy @o2l))
        starving mine sibling = "+→× status merge: emissions dropped for 3s — the operand producing " <> mine
          <> " keeps emitting, but its sibling operand producing " <> sibling
          <> " never has, so the merged record cannot complete. Prime the silent operand (`seeded`/`announce`) or check that it renders at all."
    in
    { toUser: \v -> do
        for_ (contract v :: Maybe _) \v1 -> p1'.toUser v1
        for_ (contract v :: Maybe _) \v2 -> p2'.toUser v2
    , fromUser: \prop -> do
        p1'.fromUser \partial -> do
          -- runtime-exactness trim, as in `recordToRecord`
          let exact = exactRow partial
          let _ = unsafePerformEffect $ Ref.write (Just exact) p1Last
          let mp2 = unsafePerformEffect $ Ref.read p2Last
          case mp2 of
            Nothing -> do
              guard1.blocked $ starving fields1 fields2
              tr ("merge +→×: contribution withheld (sibling fields " <> fields2 <> " not heard from yet)") exact
            Just p2val -> do
              guard1.fed *> guard2.fed
              prop $ Record.union exact p2val
        p2'.fromUser \partial -> do
          let exact = exactRow partial
          let _ = unsafePerformEffect $ Ref.write (Just exact) p2Last
          let mp1 = unsafePerformEffect $ Ref.read p1Last
          case mp1 of
            Nothing -> do
              guard2.blocked $ starving fields2 fields1
              tr ("merge +→×: contribution withheld (sibling fields " <> fields1 <> " not heard from yet)") exact
            Just p1val -> do
              guard1.fed *> guard2.fed
              prop $ Record.union p1val exact
    }

instance Applicative m => VariantToVariant (PUI m) where
  pempty = silence
  variantToVariant p1 p2 = wrap ado
    p1' <- unwrap (widenVariantOutput p1)
    p2' <- unwrap (widenVariantOutput p2)
    in
      { toUser: \v -> do
          for_ (contract v :: Maybe _) \v1 -> p1'.toUser v1
          for_ (contract v :: Maybe _) \v2 -> p2'.toUser v2
      , fromUser: \prop -> do
          p1'.fromUser prop
          p2'.fromUser prop
      }

-- Optics

-- Optimized implementation. Not optimized would be `constant a = projection (const a)`.
constant :: forall a s t m. Functor m => a -> Optic (PUI m) s t a Void
constant a w = wrap $ ado
  w' <- unwrap w
  let initializedRef = unsafePerformEffect $ Ref.new false
  in
    { toUser: \_ -> do
      initialized <- Ref.read initializedRef
      when (not initialized) do
        Ref.write true initializedRef
        w'.toUser a
    , fromUser: mempty
    }

type Action s t a b = forall m. Functor m => Optic (PUI m) s t a b

-- | The progress slot is row-shaped like every component interface: the
-- | widget is a `{ busy :: Boolean } → {}` display citizen.
-- | Adopt a bare-input widget as the owner of input case `l` inside a
-- | `VariantToVariant.do` merge — `lcmap`-only, the input-side sibling of
-- | `asCase`: `action createPerson # onCase @"create"`.
-- | No subsumption here, deliberately: a case *payload* is pinned by the
-- | action that consumes it as often as by the widget that emits it, so
-- | widening this position would leave both unknown (see doc/
-- | experiment-ad-hoc-rows.md — the payload-boundary rule).
onCase :: forall @l p a b s. IsSymbol l => Cons l a () s => Profunctor p => p a b -> p (Variant s) b
onCase = lcmap (on (Proxy @l) identity case_)

action :: forall s t. (s -> Aff t) -> Action s t { busy :: Boolean } {}
action arr = action' \i pro post -> do
  liftEffect $ pro { busy: true }
  o <- arr i
  liftEffect $ pro { busy: false }
  liftEffect $ post o

action' :: forall a b i o m. Functor m => (i -> (a -> Effect Unit) -> (o -> Effect Unit) -> Aff Unit) -> Optic (PUI m) i o a b
action' arr w = wrap ado
  w' <- unwrap w
  in
    let oVar = unsafePerformEffect $ liftEffect AVar.empty
    in
    { toUser: \i -> launchAff_ $ arr i (\a -> void $ w'.toUser a) (\o -> void $ AVar.put o oVar mempty)
    , fromUser: \prop ->
      let waitAndPropagate = void $ AVar.take oVar case _ of
            Left error -> pure unit -- TODO handle error
            Right o -> do
              prop o
              waitAndPropagate
      in waitAndPropagate
    }

-- notice: this is not really optics, operates for given m
-- TODO add release parameter?
-- TODO is this needed?
effAdapter :: forall m a b s t. Apply m => m { pre :: s -> Effect a, post ::  b -> Effect t} -> Optic (PUI m) s t a b
effAdapter f w = wrap ado
  { toUser, fromUser } <- unwrap w
  { pre, post } <- f
  in
    { toUser: \s -> do
        a <- pre s
        toUser a
    , fromUser: \prop -> do
      fromUser \b -> do
        t <- post b
        prop t
    }

-- TODO is this needed?
affAdapter :: forall m a b s t. Apply m => m { pre :: s -> Aff a, post ::  b -> Aff t} -> Optic (PUI m) s t a b
affAdapter f w = wrap ado
  { toUser, fromUser } <- unwrap w
  { pre, post } <- f
  in
    let mInputFiberRef = unsafePerformEffect $ Ref.new Nothing
        mOutputFiberRef = unsafePerformEffect $ Ref.new Nothing
    in
    { toUser: \s -> launchAff_ do
        mFiber <- liftEffect $ Ref.read mInputFiberRef
        for_ mFiber $ killFiber (error "Obsolete input")
        newFiber <- forkAff do
          a <- pre s
          liftEffect $ toUser a
        liftEffect $ Ref.write (Just newFiber) mInputFiberRef
    , fromUser: \prop -> do
      fromUser \b -> do
        launchAff_ do
          mFiber <- liftEffect $ Ref.read mOutputFiberRef
          for_ mFiber $ killFiber (error "Obsolete output")
          newFiber <- forkAff do
            t <- post b
            liftEffect $ prop t
          liftEffect $ Ref.write (Just newFiber) mOutputFiberRef
    }

-- Oculars

-- | Debounce the widget's *input* leg: each incoming value is delayed by
-- | `millis`, and a newer value supersedes (kills) the pending one, so only
-- | the last value of a burst reaches the widget. Rapid sources (keystrokes,
-- | slider drags) emit every value; the stage that doesn't want the burst
-- | opts in here.
-- |
-- | Algebraically this is the `× → +` trace at the value level:
-- | `debounced g ≅ coresolve (resolveFor millis g)` once primed — the
-- | quiescence step composed with its retraction. Implemented directly
-- | (ungated, on the input leg) as elsewhere laws are stated and bodies
-- | stay lean.
debounced' :: forall m. Applicative m => { ms :: Number } -> Ocular (PUI m)
debounced' millis = affAdapter $ pure
  { pre: \i -> delay (Milliseconds millis.ms) *> pure i
  , post: pure
  }

debounced :: forall m. Applicative m => Ocular (PUI m)
debounced = debounced' { ms: 300.0 }

spied :: forall m. Functor m => DebugWarning => String -> Ocular (PUI m)
spied name w = wrap ado
  { toUser, fromUser } <- unwrap w
  in
    { toUser: \change -> do
      let _ = spy' "showing to user" change
      toUser change
    , fromUser: \prop -> fromUser \change -> do
      let _ = spy' "getting from user" change
      prop change
    }
  where
    spy' :: forall a. String -> a -> a
    spy' text a = spy ("Spied PUI \"" <> name <> "\" " <> text <> " new value") a

-- The container action on PUI (class in Data.Profunctor.Acting — the pure
-- algebra; instances live with the carriers, like the merge instances above).
--
-- Five collection combinators share one instance discipline and fill the
-- 2×2 of the direction square. The element is always a data-model row and
-- rows carry their identity: ×-input members are keyed by a **materialized
-- identity field** `@l` of the row (the whole array broadcasts, identity is
-- read off each row); +-input members receive the runtime variant
-- `{ key, value }`, one case at a time, whose envelope IS the tag:
--
--   combinator  | input              | element    | output           | primed by     | emits             | nullary case
--   ------------|--------------------|------------|------------------|---------------|-------------------|-------------
--   foreach @l  | Array {l::k|r} (×) | p {l::k|r} | o            (+) | — (no slots)  | each emission,    | silent
--               |                    |   o        |                  |               | as it happens     | (no o to make)
--   acted @l    | Array {l::k|ra}(×) | p {l::k|ra}| Array {l::k|rb} | element       | whole array, once | emits []
--               |                    |   {|rb}    |              (×) | emissions     | every element     | (inhabited
--               |                    |            |                  |               | spoke; retain-    | nullary)
--               |                    |            |                  |               | last after        |
--   edits @l    | Array {l::k|r} (×) | p {l::k|r} | Array {l::k|r}(×)| the fed input | whole array,      | emits []
--               |                    |   {|r}     |                  |               | immediately per   | (pass-through)
--               |                    |            |                  |               | edit              |
--   dispatched  | {key,value:a} (+)  | p a b      | {key,value:b}(+) | — (no slots)  | the fed case's    | unknown key =
--               |                    |            |                  |               | emissions, tagged | new instance
--   accumulated | {key,value:a} (+)  | p a a      | Array a      (×) | the fed cases | whole array,      | grows per new
--               |                    |            |                  | (a ≅ a)       | immediately per   | key; [] before
--               |                    |            |                  |               | case              | the first
--
-- Rule of thumb — ask what the stage means downstream, and how updates
-- arrive: everything at once (a model projection) → ×-input; one entity at a
-- time (a stream, a tick, a push) → +-input. Then: an individual event
-- ("this one fired") → foreach/dispatched; the aggregate as a joint decision
-- (exists only when everyone has spoken) → acted; the aggregate as running
-- state (always valid, updated per edit/case) → edits/accumulated. foreach
-- cannot be acted (a gate over event sources is absurd; Array b erases
-- "which one"), acted cannot be edits in general (a type-changing p a b
-- cannot fabricate the missing b's from the input), edits cannot be foreach
-- (an untagged o cannot be folded back in by key) — and the +-members never
-- detach or restack: the absence of a key is no signal, so removal and
-- ordering are array-level concerns of some ×-stage upstream.
--
-- The key's form encodes its ontology, two ways: a **label @l** on the
-- ×-members (identity is a materialized field of the model row — and in the
-- product-output members `acted`/`edits` the element's output row EXCLUDES
-- it: the key is re-attached from the *input* row, so an element cannot
-- forge or change identity; `acted` derives this in the pure algebra by
-- riding the key around the element on the Strong state channel); the
-- **{ key, value } envelope** on the +-members (identity is the structural
-- tag of the runtime variant, arriving in the input). `Ord k` is an
-- indexing requirement (the reconciler's Map); identity semantics remain
-- equality — keys must be unique, never rendered.

-- | What a stateful carrier contributes to the container action: how to
-- | **instantiate** one element widget at runtime and how to **place** it —
-- | detach a leaver, restack survivors into the current key order. The keyed
-- | reconciler and both emission modes are carrier-generic above this, so
-- | `Acting (PUI m)` holds for every hosting carrier: `PUI.Web` supplies DOM
-- | placement; `Effect` (below) is placement-free — the probe carrier the
-- | value-level law tests run on.
class MonadEffect m <= Hosting m node | m -> node where
  hosting :: forall a b. PUI m a b -> m (Hooks a b node)

type Hooks a b node =
  { instantiate :: Effect { feed :: a -> Effect Unit, subscribe :: (b -> Effect Unit) -> Effect Unit, node :: node }
  , detach :: node -> Effect Unit
  , restack :: Array node -> Effect Unit
  }

-- | Placement-free: an element instance is just its channel legs.
instance Hosting Effect Unit where
  hosting w = pure
    { instantiate: do
        inst <- unwrap w
        pure { feed: inst.toUser, subscribe: inst.fromUser, node: unit }
    , detach: \_ -> pure unit
    , restack: \_ -> pure unit
    }

-- | Keyed, retaining collection on any hosting carrier (laws in
-- | `Data.Profunctor.Acting`'s header; the vocabulary form is `acted @l`).
instance Hosting m node => Acting (PUI m) where
  actedBy key w = wrap do
    hooks <- hosting w
    liftEffect $ actedWith key hooks

-- | The dynamic collection — the **collapsed (sum-flavored) reading of the
-- | container action** on any hosting carrier. **Keyed and retaining**: each
-- | element is identified by `key a`, and on every fed array the collection
-- | reconciles *by key* — matched elements are re-fed in place (their carrier
-- | node kept), new keys built, absent keys removed, and nodes restacked only
-- | when the key sequence actually changed. So a fixed-key grid never
-- | rebuilds, a growing list only appends, and a reordered list **moves each
-- | element's node with it** — carrier-local state (focus, scroll, selection)
-- | follows the item, not the position. Keys must be unique.
-- |
-- | Written trailing, wrapped in a container ocular: `ul $ item # foreach
-- | @"id"` — keyed by the row's **materialized identity field** (the element
-- | is a data-model row; rows carry their identity; `Show` renders the
-- | reconciliation index, so unique keys must render uniquely). Every
-- | element's emission collapses onto one shared channel `o` (the
-- | sum-flavored sibling of `acted`'s gathered `Array b`), so it is ungated
-- | and lawfully **silent on an empty array** (no `o` to fabricate) — as a
-- | terminal display pass the carrier through with `# lcmap proj
-- | # displayed`, the comonoid a pipeline tail requires; when the aggregate
-- | array itself is the output, use `acted` (gathered, knowledge-gated,
-- | announces `[]`) or `edits` (input-primed, immediate). All share this
-- | keyed reconciler.
foreach :: forall @l m node k r a i o. Hosting m node => IsSymbol l => Cons l k r a => Ord k => (i -> Array { | a }) -> PUI m { | a } o -> PUI m i o
foreach f w = lcmap f $ wrap do
  hooks <- hosting w
  liftEffect $ collapsedWith (Record.get (Proxy @l)) hooks

-- | The **collection editor** — lift an element *editor* (`p a a`, emitting
-- | its own edited row with its key intact, the shape `field @l`/`asField @l
-- | … # completed` produces) over the array: every element emission is folded
-- | back in **by key** and the whole updated array emits **immediately**.
-- | It can afford immediacy because it is **input-primed** — the retained fed
-- | array supplies every unedited slot (`a ≅ a`: the input is a valid
-- | output) — where `acted` is **emission-primed** and must gather (a
-- | type-changing `p a b` cannot fabricate the missing `b`s, so the `Array b`
-- | is withheld until every element has spoken). Rule of thumb: the aggregate
-- | as running state → `edits`; the aggregate as joint decision → `acted`;
-- | individual emissions → `foreach`. A first-class `Array a → Array a`
-- | editor citizen, nestable like any editor (`# field @l` into a form, or
-- | straight into `# mvu`); element addition, removal and reordering are
-- | array-level concerns and stay outside.
-- |
-- | Like every ×-member, `edits` is keyed by a **label** — but here the
-- | element's output row is the key's **complement** `{ | r }`: the key is
-- | not just identity but the edit's *return address*, so the element
-- | structurally *cannot* emit it, let alone change it. The carrier
-- | re-attaches each emission's key itself (it knows which instance
-- | emitted), completing `{ | r }` back to the full row — which also
-- | dissolves the old convention that the element must `# completed` its key
-- | field through. `Ord k` is the reconciler's indexing requirement;
-- | identity semantics remain equality — keys must be unique.
edits :: forall @l m node k r a narrow extra. Hosting m node => IsSymbol l => Cons l k r a => Lacks l r => Ord k => Union narrow extra a => PUI m { | narrow } { | r } -> PUI m (Array { | a }) (Array { | a })
edits item0 = let item = widenRecordInput item0 in wrap do
  hooks <- hosting item
  liftEffect do
    propRef <- Ref.new Nothing
    entriesRef <- Ref.new []
    busyRef <- Ref.new false
    arrRef <- Ref.new []
    let
      keyOf = Record.get (Proxy @l)
      emitAll = do
        arr <- Ref.read arrRef
        mProp <- Ref.read propRef
        for_ mProp \prop -> prop arr
      -- complete the key-less emission with ITS OWN row's key — the return
      -- address is supplied by the carrier, never by the element
      onEmit k _ edited = do
        Ref.modify_ (map \x -> if keyOf x == k then Record.insert (Proxy @l) (Record.get (Proxy @l) x) edited else x) arrRef
        busy <- Ref.read busyRef
        unless busy emitAll
    pure
      { toUser: \arr -> do
          Ref.write arr arrRef
          reconcileKeyed keyOf hooks onEmit busyRef entriesRef arr
          emitAll
      , fromUser: \prop -> Ref.write (Just prop) propRef
      }

-- | The **keyed dispatch** — the +→+ member: one runtime case at a time.
-- | A fed `{ key, value }` reaches exactly the instance whose key matches —
-- | instantiated on first appearance (an unknown key is a new case, not an
-- | error) — and that instance's emissions leave tagged with its key. The
-- | targeted-update direction: no whole-array re-feed, O(1) per case, the
-- | shape for streams/pushes that arrive one entity at a time. No key
-- | function: the runtime variant input carries its tag, as a variant case
-- | carries its label.
dispatched :: forall m node k i a b. Hosting m node => Ord k => (i -> { key :: k, value :: a }) -> PUI m a b -> PUI m i { key :: k, value :: b }
dispatched f w = lcmap f $ wrap do
  hooks <- hosting w
  liftEffect do
    propRef <- Ref.new Nothing
    indexRef <- Ref.new Map.empty
    orderRef <- Ref.new []
    let
      onEmit k _ b = do
        mProp <- Ref.read propRef
        for_ mProp \prop -> prop { key: k, value: b }
    pure
      { toUser: \u -> do
          e <- ensureInstance hooks onEmit indexRef orderRef u.key
          e.feed u.value
      , fromUser: \prop -> Ref.write (Just prop) propRef
      }

-- | The **keyed Mealy** — the +→× member: retain the array, feed one case,
-- | emit the whole. Each fed `{ key, value }` updates (or, on a new key,
-- | appends) its slot and re-emits the whole array immediately — input-primed
-- | like `edits`, so there is never a hole to withhold over; element
-- | emissions fold back into their slot the same way. The board/ledger shape
-- | for keyed streams: the aggregate as running state, built one entity at a
-- | time. Emits `[]` for no keys yet only in the sense that nothing has been
-- | fed; order is first-appearance order.
accumulated :: forall m node k i a. Hosting m node => Ord k => (i -> { key :: k, value :: a }) -> PUI m a a -> PUI m i (Array a)
accumulated f w = lcmap f $ wrap do
  hooks <- hosting w
  liftEffect do
    propRef <- Ref.new Nothing
    indexRef <- Ref.new Map.empty
    orderRef <- Ref.new []
    busyRef <- Ref.new false
    let
      emitAll = do
        entries <- Ref.read orderRef
        slots <- for entries \e -> Ref.read e.slot
        for_ (sequence slots) \values -> do
          mProp <- Ref.read propRef
          for_ mProp \prop -> prop values
      onEmit _ slot a = do
        Ref.write (Just a) slot
        busy <- Ref.read busyRef
        unless busy emitAll
    pure
      { toUser: \u -> do
          Ref.write true busyRef
          e <- ensureInstance hooks onEmit indexRef orderRef u.key
          Ref.write (Just u.value) e.slot
          e.feed u.value
          Ref.write false busyRef
          emitAll
      , fromUser: \prop -> Ref.write (Just prop) propRef
      }

-- Look up the instance for a runtime case, instantiating it on first
-- appearance and wiring its emissions through `onEmit` over its own key and
-- slot. The +-input members never detach or restack: the absence of a key is
-- no signal, and node order is first-appearance order (kept in the order
-- array; the Map is the O(log n) index for the per-case feed path).
ensureInstance
  :: forall k a b node
   . Ord k
  => Hooks a b node
  -> (k -> Ref.Ref (Maybe b) -> b -> Effect Unit)
  -> Ref.Ref (Map.Map k (ActingEntry k a b node))
  -> Ref.Ref (Array (ActingEntry k a b node))
  -> k
  -> Effect (ActingEntry k a b node)
ensureInstance hooks onEmit indexRef orderRef k = do
  index <- Ref.read indexRef
  case Map.lookup k index of
    Just e -> pure e
    Nothing -> do
      slot <- Ref.new Nothing
      inst <- hooks.instantiate
      inst.subscribe \b -> onEmit k slot b
      let e = { key: k, feed: inst.feed, slot, node: inst.node }
      Ref.write (Map.insert k e index) indexRef
      Ref.modify_ (_ <> [ e ]) orderRef
      pure e

-- The shared keyed reconciler: one entry per key, holding the element
-- instance's feed leg, its retained last output (the gather slot), and its
-- carrier node. Both emission modes are wired through `onEmit` at build time.
-- `Ord k` is an indexing requirement (the reconciler keeps a `Map` for
-- O(log n) lookups); identity semantics remain equality — keys must be
-- unique, never rendered.
type ActingEntry k a b node =
  { key :: k
  , feed :: a -> Effect Unit
  , slot :: Ref.Ref (Maybe b)
  , node :: node
  }

-- Reconcile the entry vector against a fed array: survivors re-fed in place,
-- entrants instantiated (their emissions wired to `onEmit` over their own
-- slot), leavers detached, nodes restacked only when the key sequence
-- changed. The busy guard stops an element echo from double-building
-- mid-reconcile.
reconcileKeyed
  :: forall k a b node
   . Ord k
  => (a -> k)
  -> Hooks a b node
  -> (k -> Ref.Ref (Maybe b) -> b -> Effect Unit)
  -> Ref.Ref Boolean
  -> Ref.Ref (Array (ActingEntry k a b node))
  -> Array a
  -> Effect Unit
reconcileKeyed key hooks onEmit busyRef entriesRef items = do
  busy <- Ref.read busyRef
  unless busy do
    Ref.write true busyRef
    old <- Ref.read entriesRef
    let oldByKey = Map.fromFoldable (map (\e -> Tuple e.key e) old)
    entries <- for items \a -> do
      let k = key a
      case Map.lookup k oldByKey of
        Just e -> do
          e.feed a
          pure e
        Nothing -> do
          slot <- Ref.new Nothing
          inst <- hooks.instantiate
          inst.subscribe \b -> onEmit k slot b
          inst.feed a
          pure { key: k, feed: inst.feed, slot, node: inst.node }
    let keep = Set.fromFoldable (map _.key entries)
    for_ old \e -> unless (Set.member e.key keep) (hooks.detach e.node)
    when (map _.key old /= map _.key entries) $ hooks.restack (map _.node entries)
    Ref.write entries entriesRef
    Ref.write false busyRef

-- Was this reconcile skipped by the re-entrancy guard? (A guarded skip must
-- also skip the post-reconcile gather, or a mid-reconcile echo would emit a
-- half-updated vector.)
actingGuarded :: Ref.Ref Boolean -> Effect Unit -> Effect Unit
actingGuarded busyRef act = do
  busy <- Ref.read busyRef
  unless busy act

-- The gather mode: element emissions land in their slot, then the whole
-- array re-emits from retained slots once every element has spoken —
-- including immediately after a reconcile, so `[]` emits `[]` and survivors'
-- retained slots re-emit without waiting.
actedWith :: forall k a b node. Ord k => (a -> k) -> Hooks a b node -> Effect { toUser :: Array a -> Effect Unit, fromUser :: (Array b -> Effect Unit) -> Effect Unit }
actedWith key hooks = do
  propRef <- Ref.new Nothing
  entriesRef <- Ref.new []
  busyRef <- Ref.new false
  let
    gather = do
      entries <- Ref.read entriesRef
      slots <- for entries \e -> Ref.read e.slot
      for_ (sequence slots) \bs -> do
        mProp <- Ref.read propRef
        for_ mProp \prop -> prop bs
    onEmit _ slot b = do
      Ref.write (Just b) slot
      gather
  pure
    { toUser: \items -> actingGuarded busyRef do
        reconcileKeyed key hooks onEmit busyRef entriesRef items
        gather
    , fromUser: \prop -> Ref.write (Just prop) propRef
    }

-- The forward (collapsed) mode: element emissions exit onto the shared
-- channel as they happen; the slot is kept written so a later gather-mode
-- reading of the same core stays possible, but nothing gates.
collapsedWith :: forall k a o node. Ord k => (a -> k) -> Hooks a o node -> Effect { toUser :: Array a -> Effect Unit, fromUser :: ((o -> Effect Unit) -> Effect Unit) }
collapsedWith key hooks = do
  propRef <- Ref.new Nothing
  entriesRef <- Ref.new []
  busyRef <- Ref.new false
  let
    onEmit _ slot o = do
      Ref.write (Just o) slot
      mProp <- Ref.read propRef
      for_ mProp \prop -> prop o
  pure
    { toUser: reconcileKeyed key hooks onEmit busyRef entriesRef
    , fromUser: \prop -> Ref.write (Just prop) propRef
    }
