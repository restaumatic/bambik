-- | The core profunctor UI type and its combinators.
-- |
-- | **The duoidal reading.** `PUI` composes two ways: sequentially
-- | (`Category.do` — `QualifiedDo.Category` as applications import it;
-- | `⊳`, emissions feed downstream) and in parallel
-- | (the row merges, `⊗` — the input broadcasts to every operand). The two
-- | interact as in a duoidal category: a pipeline can only emulate a merge
-- | through a **comonoid** — a stage that *duplicates* its input onward, not
-- | merely consumes it. A fulfillment-gated display is exactly that
-- | comultiplication (render
-- | *and* forward); a bare display is only the counit (render and discard),
-- | which is why undisplayed chrome ahead of a live stage starves it under
-- | `⊳` while the same chrome inside a merge needs nothing — a display is
-- | made into a pass-through stage precisely by its gate. See
-- | doc/collections-profunctor-algebra.md §0.
-- |
-- | **How to read an app.** An app is `mvu seed pipeline`: the pipeline's
-- | stages are composed with `Category.do`, every emission travels
-- | left-to-right through the stages, and `mvu` loops the final emission
-- | back to the top — so a stage placed *before* another is not "above" it
-- | semantically; all stages see every model value on the next loop turn.
-- |
-- | A trace of a counter (a `shown` display stage, then an event
-- | emitter `# applied increment`, under `mvu { count: 0 }`):
-- |
-- |  1. registration: the seed `{ count: 0 }` is fed to the first stage;
-- |  2. the display shows `0` and releases the fed row, which flows on
-- |     and arms the emitter's replay value
-- |     and `applied`'s retained state;
-- |  3. the user acts: the emitter fires, `applied` steps the retained
-- |     model by `increment` and emits `{ count: 1 }`;
-- |  4. the loop re-feeds `{ count: 1 }` to the top; the display re-renders;
-- |     the re-feed's own echoes are swallowed by the loop's re-entrancy
-- |     guard, so exactly one turn happens per event.
-- |
-- | **The rows are a presentation model.** What a `PUI` pipeline operates
-- | over is not the domain model but its presentation: source fields and
-- | the derived fields they render as — formatted readouts, unit-suffixed
-- | quantities, composed sentence lines — side by side in one row.
-- | Displays are **verbatim** (no leaf takes a formatter); the derived
-- | fields are written by one normalization per app
-- | (`present<App> :: row -> row`, run as `# settled present<App>`, the
-- | seed pre-normalized), so everything the user reads is a model field
-- | and the screen's copy is a pure function under `spago test`, no
-- | browser required. Context-pinned rows (collection items, pane
-- | payloads) carry their copy from the business function producing them.
-- | See doc/research-presentation-model.md.
-- |
-- | **No nominal types in UI.** A view-model
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
  , Ocular
  , PUI(..)
  , Hooks
  , Logged
  , Sink
  , class Hosting
  , hosting
  , setSink
  , setTracing
  , setDiagnostics
  , action
  , static
  , accumulated
  , applied
  , debounced
  , dispatched
  , edited
  , every
  , foreach
  , observed
  , optional
  , resolveFor
  , updated
  , module Adopters
  , module Looping
  , module Seeding
  )
  where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Lens (Optic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Map as Map
import Data.Set as Set
import Data.Profunctor (class Profunctor, lcmap)
import Data.Profunctor.Acting (class Acting)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Cochoice (class Cochoice)
import Data.Profunctor.Costrong (class Costrong)
import Data.Profunctor.Row.RecordToRecord (class RecordToRecord, field)
-- the adopter family and its companions, re-exported so demos need the row
-- modules only for the `.do` merges and the trace forms
import Data.Profunctor.Row.RecordToRecord (asField, atField, blank, field, mvu, subStrong, forProperty, muted, required, settled, with) as Adopters
import Data.Profunctor.Row.RecordToVariant (armed, silence, toCase, toCases) as Adopters
import Data.Profunctor.Row.VariantToRecord (forCase, forCases) as Adopters
-- `widenRecordInput` is deliberately NOT re-exported: subsumption is baked
-- into the stages that consume a row (the gated displays, `updated`,
-- `every`, `settled`, `armed`, `edited`, `acted`), so a UI component's own row is always
-- stated by a business function, never coerced at the call site. It stays
-- exported from `Data.Profunctor.Row` as the merge instances' plumbing.
import Data.Profunctor.Row.VariantToVariant (atCase, bracketed, subChoice) as Adopters
import Data.Profunctor.Acting (acted, optioned) as Adopters
import Data.Profunctor.Looping (class Looping)
import Data.Profunctor.Looping (class Looping, looped) as Looping
import Data.Profunctor.Seeding (class Seeding, seeded)
import Data.Profunctor.Seeding (class Seeding, announce, seeded) as Seeding
import Data.Profunctor.Coresolving (class Coresolving, coresolve)
import Data.Profunctor.Resolving (class Resolving)
import Data.Profunctor.Row.RecordToVariant (class RecordToVariant)
import Data.Profunctor.Row (class OwnedRecordOutputs, class OwnedVariantInputs, class SharedRecordInputs, exactRow, rowLabels, widenRecordInput, widenVariantOutput)
import Data.String (joinWith)
import Data.Profunctor.Coretaining (class Coretaining)
import Data.Profunctor.Retaining (class Retaining)
import Data.Profunctor.Row.VariantToRecord (class VariantToRecord)
import Data.Profunctor.Row.VariantToVariant (class VariantToVariant)
import Data.Profunctor.Strong (class Strong)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for, sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Symbol (class IsSymbol)
import Data.Variant (class Contractable, contract, inj, prj)
import Prim.Row (class Cons, class Lacks, class Union)
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (Aff, attempt, delay, error, forkAff, killFiber, launchAff_, message)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Record (get, insert, union) as Record
import Record.Unsafe.Union (unsafeUnion)

--------------------------------------------------------------------------------
-- The carrier
--------------------------------------------------------------------------------

newtype PUI m i o = PUI (m
  { toUser :: i -> Effect Unit
  , fromUser :: (o -> Effect Unit) -> Effect Unit
  })


--------------------------------------------------------------------------------
-- Instances (the Hosting/Acting instances live with the container-action machinery below)
--------------------------------------------------------------------------------

derive instance Newtype (PUI m i o) _

instance Functor m => Profunctor (PUI m) where
  dimap pre post p = wrap ado
    p' <- unwrap p
    in
      { toUser: pre >>> p'.toUser
      , fromUser: \prop -> p'.fromUser (prop <<< post)
      }

-- Stateful instances below share one gating principle (the same one the
-- record merges follow): state a UI component hasn't received yet cannot be
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

-- | The `×`-diagonal **trace** (dual of `Strong`): the `c` a UI component emits is
-- | retained and paired with its next input — feedback of **state**.
-- | Knowledge-gated like every stateful instance: inputs are withheld until a
-- | first `c` exists, so the loop needs priming — route the initial state in
-- | through the UI component's input where possible, or use `looped` for the
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
              guard.blocked "Costrong.unfirst: inputs dropped for 3s — the state feedback channel was never primed (the traced UI component never emitted). Use `feedback`, which takes the traced chain's initial state as an argument, or seed a raw `unfirst`/`colens` chain from inside (`seeded`)."
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
              guard.blocked "Costrong.unsecond: inputs dropped for 3s — the state feedback channel was never primed (the traced UI component never emitted). Use `feedback`, which takes the traced chain's initial state as an argument, or seed a raw chain from inside (`seeded`)."
              tr "Costrong.unsecond: input withheld (state unprimed)" b
            Just a -> p'.toUser $ Tuple a b
      , fromUser: \prop ->
          p'.fromUser \(Tuple a c) -> do
            guard.fed
            Ref.write (Just a) aRef
            prop c
      }

-- | The `+`-diagonal **trace** (dual of `Choice`): a looped-branch emission
-- | re-enters the UI component as input — feedback of **control**, i.e. iteration —
-- | until an exit-branch emission passes through. The re-entry is a `toUser`,
-- | so in `PUI` the loop is an *event* loop: it advances on the UI component's next
-- | emission (variant-output UI components do not echo, so the leaf protocol cannot
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
-- | the UI component joined with the last input (guarded), so the UI component
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
-- | `Tuple b c` yields `b` and immediately re-enters the UI component as a
-- | `Right c` resume — a **productive unfold**/generator.
-- | `coretain (retain g) ≅ g` once the state channel is primed.
instance Functor m => Coretaining (PUI m) where
  coretain p = wrap $ unwrap p <#> \p' ->
    -- the resume re-entry is guarded: a record-output UI component echoes on
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
-- | of `compose`, the element the diagonal unary laws pin, and the unit of
-- | both diagonal merges at their unit object — `identity @{}` for `×→×`,
-- | `identity @(Variant ())` for `+→+` — exactly, since the record gates
-- | ignore a contribution of zero fields; those merges have no unit of their
-- | own, and `VariantToRecord`'s is this wire entered from the empty
-- | variant, `lcmap case_ identity`.
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

-- | The **point** (the `Seeding` instance): one emission of `a` at
-- | registration, then nothing — the informationless `{}` it is fed is
-- | ignored. The pointedness primitive; the seeded echo wire the knot-tying
-- | row forms (`feedback`/`folding`/`unfolding`) prime their state channels
-- | with is derived from it through `Choice` (`Data.Profunctor.Seeding`).
instance Applicative m => Seeding (PUI m) where
  announce a = wrap $ pure
    { toUser: mempty
    , fromUser: \prop -> prop a
    }

-- | Self-reference as carrier structure (`Data.Profunctor.Looping`,
-- | `Seeding`'s sibling): feed a UI component its own emissions,
-- | re-entrancy-guarded — leaf UI components echo on `toUser`, and the
-- | guard swallows the echoes the re-feed provokes (the class's
-- | idempotence law, operationally). Wrapped around a record merge it
-- | supplies the sibling cross-feed the gated merge deliberately omits —
-- | every operand sees every emission re-broadcast, and per-operand
-- | *retention* falls out of the merge gates (each gate holds its side's
-- | last contribution). For **whole-row editor stages** the re-broadcast
-- | is what keeps exactness honest: each editor's `field @l` lift
-- | re-attaches the background it retained at its last feed, and the loop
-- | re-feeds every stage on every emission, so no editor can emit a stale
-- | sibling for longer than the turn in flight — which is why editor
-- | ensembles live inside `mvu`/`looped`/`bracketed`, and a loop-free
-- | flow wraps its editor window in `looped` (order-form). The instance
-- | is the primitive the class exists
-- | for: `Costrong`'s gated `unfirst` cannot self-feed, so the knot is
-- | tied directly here.
instance Functor m => Looping (PUI m) where
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

instance Applicative m => RecordToRecord (PUI m) where
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
    { toUser: \new -> do
          p1'.toUser new
          p2'.toUser new
    , fromUser: gatedRecordOutputs "×→×"
        (rowLabels (Proxy @o1l))
        (rowLabels (Proxy @o2l))
        exactRow exactRow p1'.fromUser p2'.fromUser
    }

instance Applicative m => RecordToVariant (PUI m) where
  -- the one unit no wire reaches (terminal → initial): silent at any rows
  silence = wrap $ pure
    { toUser: mempty
    , fromUser: mempty
    }
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
-- | `Tuple a c` shows `a` to the inner UI component and retains `c`. Every
-- | emission of the inner UI component loops immediately — `Right c`, the
-- | retained state escapes (withheld until a first `c` exists) — and
-- | (re)arms a quiescence timer; when the UI component stays quiet for the
-- | window, the last emission resolves: `Left b`. **Loop = still moving,
-- | Done = quiescence** — which is the definition of debouncing, so the
-- | retraction law refines to `coresolve (resolve g) = debounced g ≅ g`
-- | up to time (once primed). The window is `resolveFor`'s parameter;
-- | the instance uses a 300ms default.
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

-- | The Mealy step: a fresh `Left a` feeds the inner UI component, a `Right c`
-- | (re)places the retained state. When the inner UI component emits `b`, the
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
                guard.blocked "Retaining.retain: emissions dropped for 3s — the retained state was never fed (no state-case input arrived), so the gate cannot complete a Tuple. Prime the state channel: `unfolding` takes the unfold's initial state as an argument and feeds it as a first resume; raw chains seed the state case (`seeded`)."
                tr "Retaining.retain: emission withheld (state unprimed)" b
              Just c -> prop $ Tuple b c
      }

instance Applicative m => VariantToRecord (PUI m) where
  variantToRecord = variantToRecordPUI

-- | The **output gate** both record-output merges run on, stated once.
-- |
-- | `recordToRecord` and `variantToRecord` differ only in how the *input*
-- | reaches the operands (broadcast to both, versus dispatched by case); the
-- | output side is one algorithm: hold each operand's contribution until the
-- | sibling has spoken at least once, then emit their left-biased union,
-- | retaining the last contribution of each thereafter.
-- |
-- | Runtime-exactness: each contribution is trimmed to its declared output
-- | row before the union, so stale runtime copies of sibling fields (echo
-- | wires, lens rebuilds over the widening-coerced input) can never shadow
-- | the other side's genuine contribution.
-- |
-- | `direction` names the merge in the trace and starvation copy ("×→×",
-- | "+→×"), and `fields1`/`fields2` are the operands' rendered output labels,
-- | so a withholding gate says exactly which sibling it is waiting for.
gatedRecordOutputs
  :: forall e1 e2 o1 o2 o
   . Union o1 o2 o
  => String
  -> Array String
  -> Array String
  -> (e1 -> { | o1 })
  -> (e2 -> { | o2 })
  -> ((e1 -> Effect Unit) -> Effect Unit)
  -> ((e2 -> Effect Unit) -> Effect Unit)
  -> ({ | o } -> Effect Unit)
  -> Effect Unit
gatedRecordOutputs direction labels1 labels2 exact1 exact2 sub1 sub2 prop = do
  -- a side owning zero fields contributes nothing: its only possible
  -- emission is the informationless {}, pre-known below, so it neither
  -- opens the gate nor re-fires it — `identity @{}`, a silent display and
  -- an announcing one are indistinguishable as operands
  sub1 \partial -> unless (Array.null labels1) do
    let exact = exact1 partial
    let _ = unsafePerformEffect $ Ref.write (Just exact) p1Last
    let mp2 = unsafePerformEffect $ Ref.read p2Last
    case mp2 of
      Nothing -> do
        guard1.blocked $ starving fields1 fields2
        tr ("merge " <> direction <> ": contribution withheld (sibling fields " <> fields2 <> " not heard from yet)") exact
      Just p2val -> do
        guard1.fed *> guard2.fed
        prop $ Record.union exact p2val
  sub2 \partial -> unless (Array.null labels2) do
    let exact = exact2 partial
    let _ = unsafePerformEffect $ Ref.write (Just exact) p2Last
    let mp1 = unsafePerformEffect $ Ref.read p1Last
    case mp1 of
      Nothing -> do
        guard2.blocked $ starving fields2 fields1
        tr ("merge " <> direction <> ": contribution withheld (sibling fields " <> fields1 <> " not heard from yet)") exact
      Just p1val -> do
        guard1.fed *> guard2.fed
        prop $ Record.union p1val exact
  where
  fields1 = renderFieldNames labels1
  fields2 = renderFieldNames labels2
  -- a side that owns zero fields is pre-satisfied: `{}` is the
  -- informationless record, always known (L6), so the gate never waits
  -- for it — a display-side operand cannot starve its siblings whether or
  -- not it has spoken (the silence law in test/Main.purs)
  prime :: forall r. Array String -> Maybe { | r }
  prime labels = if Array.null labels then Just (unsafeCoerce {}) else Nothing
  p1Last = unsafePerformEffect $ Ref.new (prime labels1)
  p2Last = unsafePerformEffect $ Ref.new (prime labels2)
  guard1 = unsafePerformEffect gateGuard
  guard2 = unsafePerformEffect gateGuard
  starving mine sibling = direction <> " merge: emissions dropped for 3s — the operand producing " <> mine
    <> " keeps emitting, but its sibling operand producing " <> sibling
    <> " never has, so the merged record cannot complete. Prime the silent operand (`seeded`/`announce`) or check that it renders at all."

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
    -- the input side is what differs from `recordToRecord`: one case at a
    -- time, dispatched to whichever operand owns it. The output side is the
    -- same gate, held until both operands have contributed.
    { toUser: \v -> do
        for_ (contract v :: Maybe _) \v1 -> p1'.toUser v1
        for_ (contract v :: Maybe _) \v2 -> p2'.toUser v2
    , fromUser: gatedRecordOutputs "+→×"
        (rowLabels (Proxy @o1l))
        (rowLabels (Proxy @o2l))
        exactRow exactRow p1'.fromUser p2'.fromUser
    }

instance Applicative m => VariantToVariant (PUI m) where
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


--------------------------------------------------------------------------------
-- Combinators, machinery, diagnostics
--------------------------------------------------------------------------------

-- ## Development diagnostics
--
-- The emission trace and the knowledge-gate starvation watchdog: an instrument
-- pointed at the combinators below, not part of the algebra they state. Both
-- switches **and the log sink** are parameters, so this has no JavaScript of
-- its own — every `.js` in the library lives under the Web layer — and both
-- are no-ops until a carrier installs a sink. `PUI.Web.adoptHostDiagnostics`
-- passes the browser console and reads the browser's switches at the mount
-- entries, so a headless `spago test` over `PUI Effect` probes is silent.
-- They live here rather than in a module of their own because `PUI` is their
-- only caller and a carrier's diagnostics are the carrier's own business;
-- they cannot live under `PUI.Web`, which imports this module.

-- | Whatever a trace line carries, seen opaquely: `tr` is polymorphic in the
-- | logged value but a `Ref` cannot hold a `forall`, so the value crosses to
-- | the sink as this. Declared `foreign import data`, which needs no foreign
-- | *module*.
foreign import data Logged :: Type

-- | Where diagnostics go. A carrier installs one; until then both are no-ops,
-- | so nothing prints even with the switches on.
type Sink =
  { trace :: String -> Logged -> Effect Unit
  , warn :: String -> Effect Unit
  }

sinkRef :: Ref.Ref Sink
sinkRef = unsafePerformEffect (Ref.new { trace: \_ _ -> pure unit, warn: \_ -> pure unit })

-- | Install the sink. `PUI.Web` passes the browser console at its mount
-- | entries; a different host passes whatever it logs to.
setSink :: Sink -> Effect Unit
setSink sink = Ref.write sink sinkRef

tracingRef :: Ref.Ref Boolean
tracingRef = unsafePerformEffect (Ref.new false)

diagnosticsRef :: Ref.Ref Boolean
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

-- | Report a failure through the installed sink. Unlike `tr` this is **not**
-- | gated on the trace switch — a swallowed failure is exactly what leaves a
-- | UI dead with no diagnosis — but it is still a no-op until a carrier
-- | installs a sink, so a headless `spago test` stays silent.
warn :: String -> Effect Unit
warn msg = do
  sink <- Ref.read sinkRef
  sink.warn msg

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

renderFieldNames :: Array String -> String
renderFieldNames [] = "{}"
renderFieldNames ls = "{ " <> joinWith ", " ls <> " }"

-- | The **Mealy update stage** on the `×`-diagonal: a pass-through wire
-- | (every value fed flows on, so ticks and edits upstream keep driving
-- | the loop) that retains the last value and, on each *event* emission of
-- | the wrapped UI component, folds it in and emits the updated value. Event
-- | UI components emit **bare payloads** — no smuggling the model through event
-- | cases, no pass-through `state` case in the event merge:
-- |
-- | ```
-- | looped Category.do
-- |   form                                   -- ×→× editors
-- |   updates handle RecordToVariant.do ...  -- ×→+ events, bare payloads
-- | ```
-- |
-- | is the model–view–update shape as two named stages. Events arriving
-- | before a first value are withheld (the usual knowledge gate).
-- |
-- | The handler is the Mealy step's own shape, `payload -> state -> state`,
-- | and applications write it as such: the payload is an occurrence, the
-- | retained row is knowledge, and the two stay two records
-- | (`applyRefund :: { amount } -> { balance } -> { balance }`); nothing lays
-- | one over the other.
-- |
-- | **Both sides subsume** (the row layer's rule: a stated closed row may be
-- | *read* from any wider row): the handler may touch a sub-row of the model,
-- | and the wrapped event UI component may be fed a sub-row of it — typically the
-- | union of an event merge's operands — so neither side needs a
-- | `widenRecordInput` at the stage boundary. Each side is one constraint
-- | reading as the fact it is: `Union small rest big` — the model is the
-- | handler's footprint plus the rest — and `Union narrow extra big`
-- | likewise for the fed row. With `small ≡ big` and `narrow ≡ big` this is
-- | the plain diagonal stage.
updated
  :: forall m small rest big narrow extra e
   . Functor m
  => Union small rest big
  => Union narrow extra big
  => (e -> { | small } -> { | small })
  -> PUI m { | narrow } e
  -> PUI m { | big } { | big }
updated handler w = wrap $ unwrap (widenRecordInput w) <#> \evts ->
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
              guard.blocked "updated: an event was dropped and no model has arrived for 3s — the update stage has no retained state to fold into. Seed the pipeline (`with initial`/`mvu seed`)."
              tr "updated: event withheld (no retained state yet)" e
            Just s -> do
              tr "updated: folding event" e
              let s' = unsafeUnion (handler e (unsafeCoerce s)) s :: { | big }
              Ref.write (Just s') sRef
              prop s'
    }

-- | The **occurrence stage** — `updated` for an emitter that carries no
-- | payload of its own. A `× → +` leaf fed the row it acts on (a button,
-- | a `fab`, a `menuItem`, a `clicked` row `# toCase @l identity`)
-- | replays that row on click, so its "payload" is the very row the stage
-- | retains: the Mealy step degenerates to a state transformer, and this
-- | rung takes it as one — `f :: { | small } -> { | small }` is applied to
-- | the retained row on each emission, whatever the emission carries.
-- | `button @"Add" {} # applied addTodo` states the label once and the
-- | model once; the case is left alone (it is what the emission trace
-- | prints), only its `match` restatement goes.
-- |
-- | Derivation, and the law:
-- |
-- | ```
-- | applied f = updated (const f)
-- | ```
-- |
-- | — the state-only handler shape, which at a bare `updated` leaves the
-- | emitter's input row unpinned (nothing else states what a button is
-- | fed); this signature pins it to `f`'s footprint, the one thing the
-- | discarded payload was doing in `const <<< f`. The output row `[ | s ]`
-- | keeps the wrapped component an emitter — an editor's edits are values,
-- | not occurrences — and is otherwise unread. Everything else is
-- | `updated`'s: a pass-through wire, gated before a first row, `f`'s
-- | footprint read from the model by subsumption (`Union small rest big`:
-- | the model is `f`'s row plus the rest).
-- |
-- | Not the Mealy form under a second name: an emitter whose payload is
-- | real — a key from `toCase @l _.key`, an `action`'s outcome, a pane's
-- | payload under `provided`, a seeded patch under `# with patch` — keeps
-- | `updated (match { … })`, as does a stage whose emitters mean different
-- | things.
applied
  :: forall m small rest big s
   . Functor m
  => Union small rest big
  => ({ | small } -> { | small })
  -> PUI m { | small } [ | s ]
  -> PUI m { | big } { | big }
applied f = updated (const f)

-- | Make a status an **event pass-through stage** — the gated displays'
-- | sibling on
-- | the `+`-diagonal: every event flowing
-- | through is forwarded exactly once, at feed time, and the events the
-- | status consumes are also shown — `status # forCases (match { charge:
-- | retryLine }) # observed` narrates a retry loop without interrupting it. Subsumption
-- | runs the variant way (`Contractable`, the `+`-dual of the record stages'
-- | `Union` widening): the status may consume a *narrower* row than the
-- | stage carries — its cases are contracted out and shown, background cases
-- | pass untouched. The status's own emissions are dropped, deliberately —
-- | events are one-shot, so re-emitting the last event would duplicate it —
-- | and the drop is lossless by type: like the gated displays, `observed` accepts
-- | only a status whose output is `{}`.
observed
  :: forall m narrow wider
   . Functor m
  => Contractable wider narrow
  => PUI m [ | narrow ] {}
  -> PUI m [ | wider ] [ | wider ]
observed status = wrap $ unwrap status <#> \st ->
  let mPropRef = unsafePerformEffect $ Ref.new Nothing
  in
    { toUser: \v -> do
        case contract v of
          Just n -> do
            tr "observed → status" n
            st.toUser n
          Nothing -> pure unit
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop v
    , fromUser: \prop -> do
        st.fromUser \_ -> pure unit
        Ref.write (Just prop) mPropRef
    }


-- | Mark a type-changing selector as **possibly unselected** — the dual of
-- | `required`. The selection state is an entity always known from the
-- | input, *including* "nothing picked yet", so `optional` completes the
-- | selector's `Just`-only leaf echo with the missing half (fed the unmade
-- | case it announces that case; fed the made case the leaf's own echo
-- | speaks — exactly one echo per feed either way) and wraps every user
-- | pick in the made case. The model keeps a **named two-case variant**,
-- | never a `Maybe`: the application names both states —
-- | `dropdown @l config options # optional @"chosen" @"unchosen"` — and its
-- | seed spells the unmade one (`"Room": .unchosen {}`), so an unmade choice
-- | flows as honest knowledge instead of starving anything downstream, the
-- | stages demanding the selection adopt the made case
-- | (`# inCase @"chosen" roomOf`, `# provided @"complete" plan`), and
-- | only a genuine pick can ever produce the bare value. The field label is
-- | not repeated — `RowToList`'s fundep reads it from the leaf's row. Like
-- | `required`, the result is a **whole-row citizen**
-- | `p { l :: [ c :: a, n :: {} ] | rest } { l :: [ c :: a, n :: {} ] | rest }` —
-- | the echo-completed selector lifted under `field @l`, background carried.
optional :: forall @c @n l m a b s v cr nr ri ro. RowToList ri (RL.Cons l (Maybe a) RL.Nil) => IsSymbol l => IsSymbol c => IsSymbol n => Cons l (Maybe a) () ri => Cons l a () ro => Cons c a cr v => Cons n {} nr v => Cons l [ | v ] b s => Functor m => PUI m { | ri } { | ro } -> PUI m { | s } { | s }
optional p = field @l scalar
  where
  scalar :: PUI m [ | v ] [ | v ]
  scalar = wrap $ unwrap p <#> \p' ->
    let mPropRef = unsafePerformEffect $ Ref.new Nothing
    in
      { toUser: \i -> do
          let picked = prj (Proxy @c) i
          p'.toUser (Record.insert (Proxy @l) picked {})
          case picked of
            Nothing -> do
              mProp <- Ref.read mPropRef
              for_ mProp \prop -> prop (inj (Proxy @n) {})
            Just _ -> pure unit
      , fromUser: \prop -> do
          Ref.write (Just prop) mPropRef
          p'.fromUser \o -> prop (inj (Proxy @c) (Record.get (Proxy @l) o))
      }

-- | The **heartbeat wire**: `identity`'s pass-through plus a periodic step.
-- | Retains the last value flowing through; every `interval`, applies
-- | `step` to it — `Just` advances (retained and emitted), `Nothing`
-- | pauses until fresh input arrives. Inside a `looped` chain this is a
-- | tick source: the 7GUIs Timer is `every { ms: 100.0 } tick`.
-- | The loop runs for the UI component's whole life (no cancellation — a
-- | prototype limitation shared with `action'`).
-- |
-- | The step **subsumes** (like `updated`'s handler): it may read and rebuild
-- | a sub-row of the model, merged back over the last full value on each
-- | tick, so the tick's footprint is stated once in the step's own signature.
every
  :: forall m small rest big
   . Applicative m
  => Union small rest big
  => { ms :: Number }
  -> ({ | small } -> Maybe { | small })
  -> PUI m { | big } { | big }
every interval step = heartbeat interval \big -> (\s -> unsafeUnion s big :: { | big }) <$> step (unsafeCoerce big)

-- | The type-agnostic heartbeat `every` is built from — private, because the
-- | vocabulary's stages carry rows while this one is exact at any type.
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


-- Optics

-- Optimized implementation. Not optimized would be an `lcmap` writing the
-- constant over the field per feed.

type Action s t a b = forall m. Functor m => Optic (PUI m) s t a b

-- | The transpose of an optic. `Optic p s t a b = p a b -> p s t`, and the
-- | optic families quantify the *carrier* (`Lens s t a b = forall p. Strong
-- | p => Optic p s t a b`) to gain access to the data. Fix the carrier and
-- | quantify the *data* instead and you gain access to the carrier: a
-- | shape-preserving decorator — a design system's chrome — that by
-- | parametricity cannot read or produce a single value flowing through it.
-- |
-- | It is the endomorphism monoid of `p` (`identity` and `<<<`; here, carrier
-- | nesting). Naturality is free, so every ocular commutes with the
-- | `dimap`-only adopters (`asField`, `field`, `toCase`, `forProperty`).
-- | Commuting with the *strengths* is the extra law, and the admission test
-- | for anything called an ocular:
-- |
-- | ```
-- | ocular (first w) = first (ocular w)
-- | ocular (left w)  = left (ocular w)
-- | ```
-- |
-- | It holds for everything that merely wraps nodes, which is why chrome
-- | slides freely past `field @l`/`subStrong`. A decorator that captures —
-- | buffers, replays or withholds a feed — is still a natural transformation
-- | but breaks these, and needs a stated protocol instead (a modal, say).
type Ocular p = forall a b. Optic p a b a b

-- | An element with **nothing in it**: an ocular applied to the wire, its
-- | rows pinned at `{} → {}` — a ripple, a focus ring, a decorative
-- | circle, an empty cell. Reads nothing, contributes nothing; the merge
-- | gates ignore its echo (a zero-field side is pre-known and inert), so it
-- | sits in any `RecordToRecord.do` beside `staticText` and `staticHTML`,
-- | the other two statics. `static (span >>> cl "mdc-button__ripple")`.
static :: forall p. Category p => Ocular p -> p {} {}
static o = o identity

-- | The progress slot is row-shaped like every component interface: the
-- | UI component is a `{ busy :: Boolean } → {}` display citizen — and the
-- | slot is exactly that row, so a stage with no indicator passes `blank`,
-- | the faceless leaf: `blank # action …`.
-- |
-- | A failing action is **reported, not swallowed**: the progress slot is
-- | cleared whichever way the `Aff` ends — so a throw cannot strand the
-- | spinner — and the error reaches the diagnostics sink by name. Nothing is
-- | posted onward, since there is no output to post.
action :: forall s t. (s -> Aff t) -> Action s t { busy :: Boolean } {}
action arr w = action'
  (\i pro post -> do
    liftEffect $ pro { busy: true }
    result <- attempt (arr i)
    liftEffect $ pro { busy: false }
    case result of
      Left err -> liftEffect $ warn $ "action: the Aff failed and nothing was emitted — " <> message err
      Right o -> liftEffect $ post o)
  w

action' :: forall a b i o m. Functor m => (i -> (a -> Effect Unit) -> (o -> Effect Unit) -> Aff Unit) -> Optic (PUI m) i o a b
action' arr w = wrap ado
  w' <- unwrap w
  in
    let oVar = unsafePerformEffect $ liftEffect AVar.empty
    in
    { toUser: \i -> launchAff_ $ arr i (\a -> void $ w'.toUser a) (\o -> void $ AVar.put o oVar mempty)
    , fromUser: \prop ->
      let waitAndPropagate = void $ AVar.take oVar case _ of
            -- the take failed (the var was killed): the chain stops here, so
            -- say so rather than going quiet — a dead stage with no output and
            -- no diagnostic is the failure mode the watchdog exists to prevent
            Left err -> warn $ "action: the output channel closed, so this stage will emit nothing further — " <> message err
            Right o -> do
              prop o
              waitAndPropagate
      in waitAndPropagate
    }

-- Oculars

-- | Debounce the UI component: values feed it live, and its emissions forward
-- | only after `millis` of quiescence — the last value of a burst resolves,
-- | the rest loop. Rapid sources (keystrokes, continuous drags) emit every
-- | value; the stage that doesn't want the burst opts in here.
-- |
-- | Not primitive — the `× → +` trace at the value level, the stated law
-- | `coresolve (resolve g) = debounced g` made the body: the quiescence
-- | step composed with its retraction, the loop channel primed by a
-- | `seeded` wire exactly as `folding` primes its fold state.
debounced :: forall m. Applicative m => { ms :: Number } -> Ocular (PUI m)
debounced millis w = coresolve (resolveFor millis w >>> seeded (Right unit))

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
--   edited @l    | Array {l::k|r} (×) | p {l::k|r} | Array {l::k|r}(×)| the fed input | whole array,      | emits []
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
-- product-output members `acted`/`edited` the element's output row EXCLUDES
-- it: the key is re-attached from the *input* row, so an element cannot
-- forge or change identity; `acted` derives this in the pure algebra by
-- riding the key around the element on the Strong state channel); the
-- **{ key, value } envelope** on the +-members (identity is the structural
-- tag of the runtime variant, arriving in the input). `Ord k` is an
-- indexing requirement (the reconciler's Map); identity semantics remain
-- equality — keys must be unique, never rendered.

-- | What a stateful carrier contributes to the container action: how to
-- | **instantiate** one element UI component at runtime and how to **place** it —
-- | detach a leaver, restack survivors into the current key order. The keyed
-- | reconciler and both emission modes are carrier-generic above this, so
-- | `Acting (PUI m)` holds for every hosting carrier: a display carrier
-- | supplies node placement, while `Effect` (below) is placement-free — the
-- | probe carrier the value-level law tests run on.
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
-- | # muted` inside a gated stage, the comonoid a pipeline tail requires; when the aggregate
-- | array itself is the output, use `acted` (gathered, knowledge-gated,
-- | announces `[]`) or `edited` (input-primed, immediate). All share this
-- | keyed reconciler.
foreach :: forall @l m node k r a i o. Hosting m node => IsSymbol l => Cons l k r a => Ord k => (i -> Array { | a }) -> PUI m { | a } o -> PUI m i o
foreach f w = lcmap f $ wrap do
  hooks <- hosting w
  liftEffect $ collapsedWith (Record.get (Proxy @l)) hooks

-- | The **collection editor** — lift an element *editor* (`p a a`, emitting
-- | its own edited row, the whole-row-citizen shape a `field @l`-lifted
-- | leaf produces) over the array: every element emission is folded
-- | back in **by key** and the whole updated array emits **immediately**.
-- | It can afford immediacy because it is **input-primed** — the retained fed
-- | array supplies every unedited slot (`a ≅ a`: the input is a valid
-- | output) — where `acted` is **emission-primed** and must gather (a
-- | type-changing `p a b` cannot fabricate the missing `b`s, so the `Array b`
-- | is withheld until every element has spoken). Rule of thumb: the aggregate
-- | as running state → `edited`; the aggregate as joint decision → `acted`;
-- | individual emissions → `foreach`. A first-class `Array a → Array a`
-- | editor citizen, nestable like any editor (`# field @l` into a form, or
-- | straight into `# mvu`); element addition, removal and reordering are
-- | array-level concerns and stay outside.
-- |
-- | Like every ×-member, `edited` is keyed by a **label** — but here the
-- | element's output row is the key's **complement** `{ | r }`: the key is
-- | not just identity but the edit's *return address*, so the element
-- | structurally *cannot* emit it, let alone change it. The carrier
-- | re-attaches each emission's key itself (it knows which instance
-- | emitted), completing `{ | r }` back to the full row — which also
-- | dissolves any need for the element to pass its key
-- | field through. `Ord k` is the reconciler's indexing requirement;
-- | identity semantics remain equality — keys must be unique.
edited :: forall @l m node k r a narrow extra. Hosting m node => IsSymbol l => Cons l k r a => Lacks l r => Ord k => Union narrow extra a => PUI m { | narrow } { | r } -> PUI m (Array { | a }) (Array { | a })
edited item0 = let item = widenRecordInput item0 in wrap do
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
      onEmit k _ freshRow = do
        Ref.modify_ (map \x -> if keyOf x == k then Record.insert (Proxy @l) (Record.get (Proxy @l) x) freshRow else x) arrRef
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
-- | like `edited`, so there is never a hole to withhold over; element
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
