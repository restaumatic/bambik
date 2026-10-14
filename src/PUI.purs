-- | The core profunctor UI type and its combinators.
-- |
-- | **The duoidal reading.** `PUI` composes two ways: sequentially
-- | (`Semigroupoid.do` — the ecosystem's `QualifiedDo.Semigroupoid`;
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
-- | **The carrier contract.** A `PUI` value denotes, per instantiation, one
-- | process reached in three phases: constructed in `m`, where every
-- | stateful instance allocates its state — hence `MonadEffect m` on
-- | exactly those instances, and `Functor`/`Apply` on the rest saying *no
-- | state here* — then `fromUser` registered once, then feeds and emissions
-- | interleaving. The protocol, `≈`, `⊑` and the component laws every
-- | instance below is stated against are doc/observational-semantics.md
-- | §1–3; the six laws — two on components, four on merges — are stated
-- | once in `Data.Profunctor.Row` ("The laws") and read per shape in the
-- | four `Data.Profunctor.Row.*` headers ("Laws at `×→×`" and siblings);
-- | nothing of them is restated here.
-- |
-- | How applications are written over this module — the presentation rows,
-- | copy as a read function at the leaf, no nominal types in UI, the
-- | `looped` app loop and how to read a pipeline — is
-- | `.claude/skills/developing-bambik-apps/writing.md` and
-- | doc/research-copy-is-a-function.md, stated once there.
-- |
-- | **The gate is a pure machine.** The knowledge gate every product-shaped
-- | output runs on — the `×→×` merge over its owned labels, the container
-- | action's gather over its element keys — is
-- | `PUI.Gate.gateStep`, one total step over a small state in a module with
-- | no `Effect`; `driveGate` below is its whole effectful part (read, step,
-- | write, act). That separation is what lets the merge laws be checked
-- | exhaustively rather than sampled (doc/observational-semantics.md §9,
-- | test/Exhaustive.purs).
module PUI
  ( Action
  , Ocular
  , PUI(..)
  , Hooks
  , Logged
  , Sink
  , Scope
  , ScopeState(..)
  , class Scoped
  , scope
  , newScope
  , suspendScope
  , resumeScope
  , cancelScope
  , onCancel
  , scopeActive
  , scopeCancelled
  , inScope
  , class Hosting
  , hosting
  , setSink
  , setTracing
  , setDiagnostics
  , diagnosticsOn
  , action
  , fold
  , reset
  , captured
  , static
  , ticks
  , accumulated
  , debounced
  , dispatched
  , foreach
  , anyOf
  , anyOfAt
  , observed
  , resolveFor
  , module Adopters
  , module Looping
  , module Seeding
  )
  where

import Data.Profunctor.Row.Structural (withStructuralOrd)
import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (elem, foldl, for_)
import Data.Lens (Optic)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Map as Map
import Data.Set as Set
import Data.Profunctor (class Profunctor, dimap, lcmap)
import Data.Profunctor.Acting (class Acting)
import Data.Profunctor.Choice (class Choice, left)
import Data.Profunctor.Cochoice (class Cochoice)
import Data.Profunctor.Costrong (class Costrong)
import Data.Profunctor.PointedCostrong (class PointedCostrong)
import Data.Profunctor.Row.RecordToRecord (class RecordToRecord, recordToRecord)
-- the adopter family and its companions, re-exported so demos need the row
-- modules only for the `.do` merges and the knot
-- `focusField` is deliberately absent: the leaf lift is design-system plumbing —
-- every vocabulary editor is `focusField @l`-lifted inside, the labelled group
-- (`group @l`) carries sub-model nesting, so application code never lifts a
-- focus itself. `atAnyRow` is absent for the same reason: a stage is typed
-- at one row (the gated displays, `settled`, `acted`), so a UI
-- component's own row is always stated by a
-- business function, never placed at another row at the call site; the
-- placement is the vocabularies' plumbing for their statics.
import Data.Profunctor.Row (atCase, atField, settled, toCase, toField, with) as Adopters
import Data.Profunctor.Row.RecordToVariant (joined, replaying) as Adopters
import Data.Profunctor.Silencing (silence) as Adopters
import Data.Profunctor.Row.VariantToRecord (unshown) as Adopters
import Data.Profunctor.Acting (acted, actedAt, optioned) as Adopters
import Data.Profunctor.Looping (class Looping)
import Data.Profunctor.Looping (class Looping, looped) as Looping
import Data.Profunctor.Seeding (class Seeding, isHole, seeded)
import Data.Profunctor.Seeding (class Seeding, announce, seeded) as Seeding
import Data.Profunctor.Coresolving (class Coresolving, coresolve)
import Data.Profunctor.Resolving (class Resolving)
import Data.Profunctor.Row.RecordToVariant (class RecordToVariant, recordToVariant)
import Data.Profunctor.Silencing (class Silencing, silence)
import Data.Profunctor.Row (class RowLabels, exactRow, fedFirst, fedSecond, rowLabels, widenVariantOutput)
import Data.String (joinWith)
import Data.Profunctor.Coretaining (class Coretaining)
import Data.Profunctor.Retaining (class Retaining)
import Data.Profunctor.Row.VariantToRecord (class VariantToRecord)
import Data.Profunctor.Row.VariantToVariant (class VariantToVariant)
import Data.Profunctor.Strong (class Strong)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for, sequence, sequence_, traverse_)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Symbol (class IsSymbol)
import Data.Variant (class Contractable, case_, contract, inj, match, on)
import Prim.Row (class Cons)
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
import PUI.Gate (GateInput(..), GateOutput(..), GateState, StepKind(..), gateStep, initialGate)
import Record (get) as Record
import Record.Unsafe (unsafeGet, unsafeSet)

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
--
-- The price is named: the ecosystem Strong law `lmap fst = rmap fst <<< first`
-- holds only as primed equivalence — a pre-feed emission is dropped, not
-- delayed (a permanent prefix difference, tested as such in test/Main.purs;
-- doc/observational-semantics.md).
instance MonadEffect m => Strong (PUI m) where
  first p = wrap do
    p' <- unwrap p
    -- state is allocated in the construction monad, per instantiation — the
    -- phase it belongs to (doc/observational-semantics.md §1), which is also
    -- what keeps every `foreach` row on its own ref
    lastab <- liftEffect $ Ref.new Nothing
    guard <- liftEffect gateGuard
    pure
      { toUser: \ab -> do
          guard.fed
          Ref.write (Just ab) lastab
          p'.toUser $ fst ab
      , fromUser: \prop ->
          p'.fromUser \b -> do
            mab <- Ref.read lastab
            case mab of
              Nothing -> do
                guard.blocked "Strong.first: emissions dropped for 3s — the pair state was never fed (no input arrived), so the gate cannot complete a Tuple. Feed the stage a first value." []
                tr "Strong.first: emission withheld (pair state unknown)" b
              Just prevab -> prop (Tuple b (snd prevab))
      }
  second p = wrap do
    p' <- unwrap p
    lastab <- liftEffect $ Ref.new Nothing
    guard <- liftEffect gateGuard
    pure
      { toUser: \ab -> do
          guard.fed
          Ref.write (Just ab) lastab
          p'.toUser $ snd ab
      , fromUser: \prop ->
          p'.fromUser \b -> do
            mab <- Ref.read lastab
            case mab of
              Nothing -> do
                guard.blocked "Strong.second: emissions dropped for 3s — the pair state was never fed (no input arrived), so the gate cannot complete a Tuple. Feed the stage a first value." []
                tr "Strong.second: emission withheld (pair state unknown)" b
              Just prevab -> prop (Tuple (fst prevab) b)
      }

instance MonadEffect m => Choice (PUI m) where
  left p = wrap do
    p' <- unwrap p
    mPropRef <- liftEffect $ Ref.new Nothing
    pure
      { toUser: case _ of
        Right c -> do
          mProp <- Ref.read mPropRef
          for_ mProp \prop -> prop (Right c)
        Left a -> p'.toUser a
      , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        p'.fromUser \b -> prop (Left b)
      }
  right p = wrap do
    p' <- unwrap p
    mPropRef <- liftEffect $ Ref.new Nothing
    pure
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
-- | self-feeding diagonal special case, which has no gate.
-- |
-- | The raw retraction composite `unfirst (first g)` is **dead** on this
-- | carrier: `unfirst`'s input gate waits on an emission that `first`'s own
-- | gate withholds until fed, and no outside channel breaks the circle. So
-- | the retraction law is stated (and tested) in **seeded** form, for `g`
-- | honoring the record-echo protocol (each feed answered):
-- |
-- | ```
-- | unfirst (seeded (Tuple a0 c0) >>> first g) ≈ seeded a0 >>> g
-- | ```
-- |
-- | — which is why the knot at a record junction is `Looping`'s `looped`
-- | (the self-feeding diagonal, ungated) rather than a derivation from
-- | this class. Contrast `Cochoice` below,
-- | whose retraction holds raw: this carrier is genuinely traced over `+`
-- | and only pointed-traced over `×` (doc/observational-semantics.md).
instance MonadEffect m => Costrong (PUI m) where
  unfirst p = wrap do
    p' <- unwrap p
    cRef <- liftEffect $ Ref.new Nothing
    guard <- liftEffect gateGuard
    pure
      { toUser: \a -> do
          mc <- Ref.read cRef
          case mc of
            Nothing -> do
              guard.blocked "Costrong.unfirst: inputs dropped for 3s — the state feedback channel was never primed (the traced UI component never emitted). Loop state through the model instead (`looped`, closed by `with`), or seed a raw `unfirst`/`colens` chain from inside (`seeded`)." []
              tr "Costrong.unfirst: input withheld (state unprimed)" a
            Just c -> p'.toUser $ Tuple a c
      , fromUser: \prop ->
          p'.fromUser \(Tuple b c) -> do
            guard.fed
            Ref.write (Just c) cRef
            prop b
      }
  unsecond p = wrap do
    p' <- unwrap p
    aRef <- liftEffect $ Ref.new Nothing
    guard <- liftEffect gateGuard
    pure
      { toUser: \b -> do
          ma <- Ref.read aRef
          case ma of
            Nothing -> do
              guard.blocked "Costrong.unsecond: inputs dropped for 3s — the state feedback channel was never primed (the traced UI component never emitted). Loop state through the model instead (`looped`, closed by `with`), or seed a raw chain from inside (`seeded`)." []
              tr "Costrong.unsecond: input withheld (state unprimed)" b
            Just a -> p'.toUser $ Tuple a b
      , fromUser: \prop ->
          p'.fromUser \(Tuple a c) -> do
            guard.fed
            Ref.write (Just a) aRef
            prop c
      }

-- | The **pointed** `×`-trace: `unfirst` with its state `Ref` starting at
-- | `c0`, so the first input joins the starting state instead of waiting on
-- | an emission. The pointed trace at the value level; a row form gave
-- | way to looping the state through the model, so no library word stands
-- | on it now.
instance MonadEffect m => PointedCostrong (PUI m) where
  -- a hole start (guardrails L18) is absent: the state channel is unprimed
  -- and every input is withheld, as under the gated `unfirst` — so a chain
  -- whose seed is not written yet lets nothing through to the handlers that
  -- are not written yet either
  unfirstFrom c0 p = wrap do
    p' <- unwrap p
    cRef <- liftEffect $ Ref.new (if isHole c0 then Nothing else Just c0)
    pure
      { toUser: \a -> do
          mc <- Ref.read cRef
          case mc of
            Nothing -> tr "PointedCostrong.unfirstFrom: input withheld (the start is a hole)" a
            Just c -> p'.toUser $ Tuple a c
      , fromUser: \prop ->
          p'.fromUser \(Tuple b c) -> do
            Ref.write (Just c) cRef
            prop b
      }

-- | The `+`-diagonal **trace** (dual of `Choice`): a looped-branch emission
-- | re-enters the UI component as input — feedback of **control**, i.e. iteration —
-- | until an exit-branch emission passes through. The re-entry is a `toUser`,
-- | so in `PUI` the loop is an *event* loop: it advances on the UI component's next
-- | emission (variant-output UI components do not echo, so the leaf protocol cannot
-- | provoke a synchronous spin — the no-synchronous-event-echo law is this
-- | instance's termination argument). Retraction law: `unleft (left g) = g`,
-- | holding **raw** — no seed: the one trace whose yanking needs no pointing
-- | (the `+`/`×` asymmetry, doc/observational-semantics.md).
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
-- | state to prime it). The raw composite `coresolve (resolve g)` is
-- | input-dead (each gate waits on the other), so the retraction is stated
-- | seeded — and the seeded composite IS `debounced`:
-- | `coresolve (resolve g >>> seeded (Right c0)) ≈ debounced g`.
instance MonadEffect m => Coresolving (PUI m) where
  coresolve p = wrap do
    p' <- unwrap p
    aRef <- liftEffect $ Ref.new Nothing
    cRef <- liftEffect $ Ref.new Nothing
    busyRef <- liftEffect $ Ref.new false
    guard <- liftEffect gateGuard
    pure
      { toUser: \a -> do
          Ref.write (Just a) aRef
          mc <- Ref.read cRef
          case mc of
            Nothing -> do
              guard.blocked "Coresolving.coresolve: inputs dropped for 3s — the fold state was never primed (no loop-branch emission arrived). Seed the loop branch (`seeded`, as `debounced` does); an application loop closes at a junction instead (`looped`/`cycled`)." []
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
-- | The raw composite `coretain (retain g)` is output-dead (`retain`'s gate
-- | waits on a resume only emissions can trigger), so the retraction is
-- | stated seeded: `coretain (seeded (Right c0) >>> retain g) ≈ g`.
instance MonadEffect m => Coretaining (PUI m) where
  coretain p = wrap do
    p' <- unwrap p
    -- the resume re-entry is guarded: a record-output UI component echoes on
    -- `toUser`, and an unguarded re-feed would loop on its own echo
    busyRef <- liftEffect $ Ref.new false
    pure
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

-- | `identity` forwards its input straight to its output: the echo wire, and
-- | the unit of `compose`. It is also the unit of both diagonal merges at
-- | their unit object and, entered from the empty variant, of `+→×` — the
-- | unit axis of each shape header.
instance MonadEffect m => Category (PUI m) where
  -- the ref is allocated in the construction monad, per instantiation:
  -- `identity` is a constant, and a top-level `let` would be evaluated
  -- once — every `identity` in the app would share one wire
  identity = wrap do
    mPropRef <- liftEffect $ Ref.new Nothing
    pure
      { toUser: \ch -> do
          mProp <- Ref.read mPropRef
          for_ mProp \prop -> prop ch
      , fromUser: \prop -> Ref.write (Just prop) mPropRef
      }

-- | The **point** (the `Seeding` instance): one emission of `a` at
-- | registration, then nothing — the informationless `{}` it is fed is
-- | ignored. The pointedness primitive: `with` closes the knot over it,
-- | and the seeded echo wire `debounced` primes its loop
-- | branch with is derived from it through `Choice`
-- | (`Data.Profunctor.Seeding`).
instance MonadEffect m => Seeding (PUI m) where
  announce a = wrap $ pure
    { toUser: mempty
    -- an `if`, not `unless`: the branch must not even be built for a hole,
    -- since a downstream stage (a gated merge's `exactRow`) reads the value
    -- while constructing its effect (holey twins of a fold beside a status)
    , fromUser: \prop -> if isHole a then pure unit else prop a
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
-- | is what keeps exactness honest: each editor's `focusField @l` lift
-- | re-attaches the background it retained at its last feed, and the loop
-- | re-feeds every stage on every emission, so no editor can emit a stale
-- | sibling for longer than the turn in flight — which is why editor
-- | ensembles live inside `looped`, and a loop-free
-- | flow wraps its editor window in `looped` (order-form). The instance
-- | is the primitive the class exists
-- | for: `Costrong`'s gated `unfirst` cannot self-feed, so the knot is
-- | tied directly here.
instance MonadEffect m => Looping (PUI m) where
  looping p = wrap do
    p' <- unwrap p
    busyRef <- liftEffect $ Ref.new false
    -- `Just pending` while the body is being wired: downstream registers
    -- first (`compose`), so an emission fired during registration — a
    -- `fold`'s seed, announced from the loop's last stage — would be re-fed
    -- to a top whose stages have no listener yet (a display's release would
    -- reach no one). It is held and re-fed once the body is wired.
    wiringRef <- liftEffect $ Ref.new (Just [])
    pure
      { toUser: p'.toUser
      , fromUser: \prop -> do
          let
            refeed u = do
              busy <- Ref.read busyRef
              if busy
                then tr "looped: echo swallowed (re-entrant)" u
                else do
                  tr "looped: re-feeding emission" u
                  Ref.write true busyRef
                  p'.toUser u
                  Ref.write false busyRef
                  prop u
          p'.fromUser \u -> do
            wiring <- Ref.read wiringRef
            case wiring of
              Just pending -> do
                tr "looped: emission while wiring, held until the body is wired" u
                Ref.write (Just (pending <> [ u ])) wiringRef
              Nothing -> refeed u
          pending <- Ref.read wiringRef
          Ref.write Nothing wiringRef
          for_ (fromMaybe [] pending) refeed
      }

-- The four row merges, one instance per shape, in the order of the grid in
-- Data.Profunctor.Row ("The laws"): ×→×, ×→+, +→+, +→×. Only the first
-- gates — the machinery that follows the block, shared with the container
-- action; the other three are routing (+→× the copairing).

instance MonadEffect m => RecordToRecord (PUI m) where
  recordToRecord p1 p2 = wrap do
    p1' <- unwrap (lcmap fedFirst p1)
    p2' <- unwrap (lcmap fedSecond p2)
    gate <- liftEffect $ newRecordGate "×→×" labels1 labels2
    pure
      -- broadcast in, gate out, one feed one step — the shape's projection
      -- and preservation laws; the carrier's part is `steppedFeed`, the
      -- batched broadcast
      { toUser: \new -> steppedRecordFeed Shared gate do
            p1'.toUser new
            p2'.toUser new
      , fromUser: gatedRecordOutputs gate exactRow exactRow p1'.fromUser p2'.fromUser
      }
    where
    labels1 = labelsOf p1
    labels2 = labelsOf p2

-- the one unit no wire reaches (terminal → initial): silent at any rows
instance Applicative m => Silencing (PUI m) where
  silence = wrap $ pure
    { toUser: mempty
    , fromUser: mempty
    }

instance Applicative m => RecordToVariant (PUI m) where
  -- broadcast in, passage out — the shape's projection law. A variant
  -- output has nothing to gate, so the merge is a bare pass-through and
  -- the one-thing-per-feed obligation falls on the operands (the answer
  -- law at ×→+, arming); hence `Applicative m`: no state here.
  recordToVariant p1 p2 = wrap ado
    p1' <- unwrap (widenVariantOutput (lcmap fedFirst p1))
    p2' <- unwrap (widenVariantOutput (lcmap fedSecond p2))
    in
      { toUser: \new -> do
          p1'.toUser new
          p2'.toUser new
      , fromUser: \prop -> do
          p1'.fromUser prop
          p2'.fromUser prop
      }

-- dispatch in, passage out — the shape's projection law: no
-- broadcast, so nothing to step; a variant out, so nothing to gate. Hence
-- `Applicative m`: the one merge with no state at all.
instance Applicative m => VariantToVariant (PUI m) where
  variantToVariant p1 p2 = wrap ado
    p1' <- unwrap p1
    p2' <- unwrap p2
    in
      { toUser: \v -> do
          for_ (contract v :: Maybe _) \v1 -> p1'.toUser v1
          for_ (contract v :: Maybe _) \v2 -> p2'.toUser v2
      , fromUser: \prop -> do
          p1'.fromUser prop
          p2'.fromUser prop
      }

instance MonadEffect m => VariantToRecord (PUI m) where
  -- the copairing: an occurrence is dispatched to the one operand owning
  -- its case, and whatever either operand releases is the whole row, so it
  -- is forwarded as it comes — no gate, nothing retained
  variantToRecord p1 p2 = wrap do
    p1' <- unwrap p1
    p2' <- unwrap p2
    pure
      { toUser: \v -> do
          for_ (contract v :: Maybe _) \v1 -> p1'.toUser v1
          for_ (contract v :: Maybe _) \v2 -> p2'.toUser v2
      , fromUser: \prop -> do
          p1'.fromUser prop
          p2'.fromUser prop
      }

-- The gate the `×→×` merge and the container action share: the pure
-- machine is `PUI.Gate`; this is its runner, then the record merge's
-- enrolment of its owned labels as its participants (the
-- collection's enrolment of its keys is `actedWith`, with the reconciler).

-- | The rendered output labels of an operand, read off its type: a
-- | `RowToList` over its output row, resolved from the operand value rather
-- | than from a named `RowList` variable, which is what lets the two
-- | record-output merges be written directly as instance methods (an
-- | instance member cannot restate the class signature, so the method's
-- | `RowList` variables are not in scope in its body).
labelsOf :: forall m a r rl. RowToList r rl => RowLabels rl => PUI m a { | r } -> Array String
labelsOf _ = rowLabels (Proxy @rl)

-- | The gate as run: the pure machine's state in one `Ref` and the
-- | downstream continuation, written at registration and read at release,
-- | allocated in the construction monad of whatever it gates (the phase
-- | state belongs to). `k` is the participant key — a field label, an
-- | element key — and `v` the slot, never inspected.
type Gate k v =
  { state :: Ref.Ref (GateState k v)
  , prop :: Ref.Ref (Maybe (Array (Tuple k v) -> Effect Unit))
  }

newGate :: forall k v. Array k -> Effect (Gate k v)
newGate order = { state: _, prop: _ } <$> Ref.new (initialGate order) <*> Ref.new Nothing

-- | One transition of the gate machine (`PUI.Gate.gateStep`), run: read the
-- | state, step, write the new state **before** acting on the output — so a
-- | re-entrant feed provoked by the release sees the post-release state —
-- | then act: a `Released` vector goes to the downstream continuation. The
-- | output comes back for the caller's own diagnostics (a record merge arms
-- | its starvation guards on `Withheld` and traces it; the collection
-- | traces it). This is the whole of the effectful part of the gate:
-- | everything decided is decided in the pure step.
driveGate :: forall k v. Ord k => Gate k v -> GateInput k v -> Effect (GateOutput k v)
driveGate gate input = do
  s <- Ref.read gate.state
  let Tuple s' out = gateStep s input
  Ref.write s' gate.state
  case out of
    Released kvs -> Ref.read gate.prop >>= \mProp -> for_ mProp \prop -> prop kvs
    _ -> pure unit
  pure out

-- | Bracket a feed as one gate step: `StepBegun`, the feed, `StepEnded`.
-- | Contributions landing inside the brackets are held and released once at
-- | the end, so a feed changing several fields emits one whole row and a
-- | reconcile whose elements echo gathers one whole vector (closure, at
-- | every gated shape). Steps nest: a re-entrant feed provoked by a release
-- | only feeds, and the outermost `StepEnded` releases — its output is what
-- | comes back.
steppedFeed :: forall k v. Ord k => StepKind -> Gate k v -> Effect Unit -> Effect (GateOutput k v)
steppedFeed kind gate feed = do
  _ <- driveGate gate StepBegun
  feed
  driveGate gate (StepEnded kind)

-- | A record merge's enrolment: the gate over the two operands' owned
-- | labels, a starvation guard per side, and the copy naming the merge
-- | ("×→×", "+→×") in the trace and the warning. A side owning no field
-- | enrols no participant, so it is born satisfied and can never starve its
-- | sibling — the unit axis's zero-field clause, a consequence of the
-- | enrolment rather than a configuration of the machine.
type RecordGate =
  { gate :: Gate String Field
  , shape :: String
  , labels1 :: Array String
  , labels2 :: Array String
  , guard1 :: GateGuard
  , guard2 :: GateGuard
  }

-- | A field's value on its way through the gate: stored under its label and
-- | re-attached at release, never inspected — the machine's `v` here.
data Field

type GateGuard = { blocked :: String -> Array String -> Effect Unit, fed :: Effect Unit }

newRecordGate :: String -> Array String -> Array String -> Effect RecordGate
newRecordGate shape labels1 labels2 = do
  gate <- newGate (labels1 <> labels2)
  guard1 <- gateGuard
  guard2 <- gateGuard
  pure { gate, shape, labels1, labels2, guard1, guard2 }

-- | The streaming-phase subscription of a record merge's gate. Each operand
-- | emission, trimmed to its declared row by `exact1`/`exact2` (the type
-- | says exactly what the machine may see), contributes its fields under
-- | their labels; a release is assembled back into the merged row, `Union
-- | o1 o2 o` witnessing that the assembled fields are exactly `o`'s.
gatedRecordOutputs
  :: forall e1 e2 r1 r2 r
   . RecordGate
  -> (e1 -> { | r1 })
  -> (e2 -> { | r2 })
  -> ((e1 -> Effect Unit) -> Effect Unit)
  -> ((e2 -> Effect Unit) -> Effect Unit)
  -> ({ | r } -> Effect Unit)
  -> Effect Unit
gatedRecordOutputs rg exact1 exact2 sub1 sub2 prop = do
  -- the release reads the downstream continuation from the gate, so it is
  -- written before the operands subscribe: a registration-time announcement
  -- releases into a listening channel
  Ref.write (Just (prop <<< assembled)) rg.gate.prop
  -- a side owning zero fields contributes nothing: its only possible
  -- emission is the informationless {}, which fills no participant, so it
  -- neither opens the gate nor re-fires it — `identity @{}`, a silent
  -- display and an announcing one are indistinguishable as operands
  sub1 \partial -> contributed rg (fieldsOf rg.labels1 (exact1 partial))
  sub2 \partial -> contributed rg (fieldsOf rg.labels2 (exact2 partial))
  where
  assembled :: Array (Tuple String Field) -> { | r }
  assembled = unsafeCoerce <<< foldl (\r (Tuple l v) -> unsafeSet l v r) {}

fieldsOf :: forall r. Array String -> { | r } -> Array (Tuple String Field)
fieldsOf labels r = labels <#> \l -> Tuple l (unsafeGet l r)

contributed :: RecordGate -> Array (Tuple String Field) -> Effect Unit
contributed rg fields = driveGate rg.gate (Contributed fields) >>= reported rg

steppedRecordFeed :: StepKind -> RecordGate -> Effect Unit -> Effect Unit
steppedRecordFeed kind rg feed = steppedFeed kind rg.gate feed >>= reported rg

-- | What a record merge does with its gate's output beyond the release
-- | itself: a release marks both starvation guards fed; a withheld
-- | contribution is traced and arms the guard of the side that has spoken,
-- | naming the fields the gate still waits for (handed to the sink as data,
-- | so the browser can point at their stamped host elements).
reported :: RecordGate -> GateOutput String Field -> Effect Unit
reported rg = case _ of
  Released _ -> rg.guard1.fed *> rg.guard2.fed
  Withheld missing -> do
    tr ("merge " <> rg.shape <> ": contribution withheld (fields " <> renderFieldNames missing <> " not heard from yet)") missing
    armSpoken missing rg.labels1 rg.guard1
    armSpoken missing rg.labels2 rg.guard2
  Quiet -> pure unit
  where
  -- the side that spoke owns fields and none of them is missing
  armSpoken missing labels guard =
    when (not (Array.null labels) && not (Array.any (_ `elem` missing) labels)) $
      guard.blocked (starving (renderFieldNames labels) (renderFieldNames missing)) missing
  starving mine sibling = rg.shape <> " merge: emissions dropped for 3s — the operand producing " <> mine
    <> " keeps emitting, but the sibling fields " <> sibling
    <> " have never been heard from, so the merged record cannot complete. Prime the silent operand (`seeded`/`announce`) or check that it renders at all."

-- | The loop step, with transiency **derived from time**: the input
-- | `Tuple a c` shows `a` to the inner UI component and retains `c`. Every
-- | emission of the inner UI component loops immediately — `Right c`, the
-- | retained state escapes (withheld until a first `c` exists) — and
-- | (re)arms a quiescence timer; when the UI component stays quiet for the
-- | window, the last emission resolves: `Left b`. **Loop = still moving,
-- | Done = quiescence** — which is the definition of debouncing, so the
-- | seeded retraction reads
-- | `coresolve (resolve g >>> seeded (Right c0)) ≈ debounced g` — the tied
-- | loop IS debouncing. The window is `resolveFor`'s parameter; the
-- | instance fixes the carrier's **quiescence quantum**, 300ms — a semantic
-- | constant of this instance, not of the class (`resolveFor` re-scopes it
-- | per stage).
instance MonadEffect m => Resolving (PUI m) where
  resolve = resolveFor { ms: 300.0 }

-- | `resolve` with an explicit quiescence window — see the `Resolving`
-- | instance. `Done` needs no state and fires (after the window) even
-- | unprimed; only the `Loop` branch is gated on a first `c`.
resolveFor :: forall m a b c. MonadEffect m => { ms :: Number } -> PUI m a b -> PUI m (Tuple a c) (Either b c)
resolveFor millis p = wrap do
  p' <- unwrap p
  cRef <- liftEffect $ Ref.new Nothing
  mFiberRef <- liftEffect $ Ref.new Nothing
  guard <- liftEffect gateGuard
  pure
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
              guard.blocked "Resolving.resolve: loop-branch emissions dropped for 3s — no input has primed the retained state (only quiescence resolutions pass). Feed the stage a first value." []
              tr "Resolving.resolve: loop branch withheld (state unprimed)" b
            Just c -> prop $ Right c
    }

-- | The Mealy step: a fresh `Left a` feeds the inner UI component, a `Right c`
-- | (re)places the retained state. When the inner UI component emits `b`, the
-- | output pairs it with the retained `c` — and is **withheld until a `c`
-- | has arrived** (a `Tuple b c` with unknown `c` would be a fabrication),
-- | mirroring the knowledge-gated record merges.
instance MonadEffect m => Retaining (PUI m) where
  retain p = wrap do
    p' <- unwrap p
    cRef <- liftEffect $ Ref.new Nothing
    guard <- liftEffect gateGuard
    pure
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
                guard.blocked "Retaining.retain: emissions dropped for 3s — the retained state was never fed (no state-case input arrived), so the gate cannot complete a Tuple. Prime the state channel: seed the state case (`seeded`); an application loop closes at a junction instead (`looped`/`cycled`)." []
                tr "Retaining.retain: emission withheld (state unprimed)" b
              Just c -> prop $ Tuple b c
      }


--------------------------------------------------------------------------------
-- Combinators, machinery, diagnostics
--------------------------------------------------------------------------------

-- ## Scope: a subtree that is out
--
-- Every word whose emissions have no cause on the channel — a tick, an
-- effect settling — runs in a **scope**, the carrier's record of whether the
-- subtree it was built in is on the screen. A pane parks its content and
-- **suspends** its scope, so a `ticks` inside skips its beats and an
-- `action` that settles inside posts nothing: the pane law — a detached
-- emitter *is* `silence` — holds for emitters that originate occurrences
-- themselves, not only for those whose occurrences come from a node the
-- document no longer holds. A collection's leaver and a `dynamic` rebuild
-- **cancel** their scope: a cancelled `ticks` ends its loop and the
-- finalizers registered on the scope run (an observer disconnects). Scopes
-- nest, a child active only while every ancestor is. Retention is untouched:
-- suspension stops a subtree from speaking, not from remembering, and a
-- re-attached pane resumes where it was.

-- | Whether the subtree a scope governs is on the screen (`Active`), parked
-- | by its pane (`Suspended`) or gone for good (`Cancelled`).
data ScopeState = Active | Suspended | Cancelled

derive instance Eq ScopeState

-- | The scope of a subtree: its own state, its parent (a child is active only
-- | while every ancestor is), its children (cancelled with it) and the
-- | finalizers to run once when it is cancelled.
newtype Scope = Scope
  { id :: Int
  , state :: Ref.Ref ScopeState
  , parent :: Maybe Scope
  , children :: Ref.Ref (Array Scope)
  , finalizers :: Ref.Ref (Array (Effect Unit))
  }

scopeCounter :: Ref.Ref Int
scopeCounter = unsafePerformEffect (Ref.new 0)

-- | A scope under its parent (`Nothing`: a root), starting in the given state.
newScope :: Maybe Scope -> ScopeState -> Effect Scope
newScope parent st = do
  id <- Ref.modify (_ + 1) scopeCounter
  s <- Scope <$> ({ id, state: _, parent, children: _, finalizers: _ } <$> Ref.new st <*> Ref.new [] <*> Ref.new [])
  for_ parent \(Scope p) -> Ref.modify_ (_ <> [ s ]) p.children
  pure s

-- | On the screen: this scope and every ancestor is `Active`.
scopeActive :: Scope -> Effect Boolean
scopeActive (Scope s) = do
  st <- Ref.read s.state
  if st /= Active then pure false else maybe (pure true) scopeActive s.parent

-- | Gone for good: this scope or an ancestor is `Cancelled`.
scopeCancelled :: Scope -> Effect Boolean
scopeCancelled (Scope s) = do
  st <- Ref.read s.state
  if st == Cancelled then pure true else maybe (pure false) scopeCancelled s.parent

-- | Park the subtree (a pane detaching its content); a cancelled scope stays cancelled.
suspendScope :: Scope -> Effect Unit
suspendScope (Scope s) = Ref.modify_ (\st -> if st == Cancelled then st else Suspended) s.state

-- | Put the subtree back (a pane attaching its content); a cancelled scope stays cancelled.
resumeScope :: Scope -> Effect Unit
resumeScope (Scope s) = Ref.modify_ (\st -> if st == Cancelled then st else Active) s.state

-- | End the subtree: its children are cancelled first, then its finalizers
-- | run once, and it leaves its parent's children.
cancelScope :: Scope -> Effect Unit
cancelScope (Scope s) = do
  st <- Ref.read s.state
  unless (st == Cancelled) do
    Ref.write Cancelled s.state
    Ref.read s.children >>= traverse_ cancelScope
    Ref.write [] s.children
    fins <- Ref.read s.finalizers
    Ref.write [] s.finalizers
    sequence_ fins
    for_ s.parent \(Scope p) -> Ref.modify_ (Array.filter (\(Scope c) -> c.id /= s.id)) p.children

-- | Register what to undo when the scope is cancelled (an observer to
-- | disconnect, a timer to clear); on a scope already cancelled it runs now.
onCancel :: Scope -> Effect Unit -> Effect Unit
onCancel sc@(Scope s) fin = do
  cancelled <- scopeCancelled sc
  if cancelled then fin else Ref.modify_ (_ <> [ fin ]) s.finalizers

-- | A construction monad that knows the scope of the subtree being built:
-- | `Web` keeps it in its state, opened by the panes and the collections;
-- | `Effect`, the probe carrier, reads an ambient one (`inScope`), the root
-- | by default, so the law tests run in an always-active scope.
class MonadEffect m <= Scoped m where
  scope :: m Scope

ambientScopeRef :: Ref.Ref Scope
ambientScopeRef = unsafePerformEffect (newScope Nothing Active >>= Ref.new)

instance Scoped Effect where
  scope = Ref.read ambientScopeRef

-- | Build on the `Effect` carrier under a scope of the test's choosing.
inScope :: forall a. Scope -> Effect a -> Effect a
inScope sc act = do
  prev <- Ref.read ambientScopeRef
  Ref.write sc ambientScopeRef
  r <- act
  Ref.write prev ambientScopeRef
  pure r

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
-- | so nothing prints even with the switches on. `warn` carries, beside the
-- | message, the **field labels the failure is about** (the starving gate's
-- | missing sibling fields; empty when the failure has no field locus) — as
-- | data, so a carrier's sink can point at the labels' stamped hosts rather
-- | than parse prose: the browser sink resolves each label through the stamp
-- | invariant (`[name=…]`/`[aria-label=…]`) and logs the elements themselves,
-- | clickable in DevTools.
type Sink =
  { trace :: String -> Logged -> Effect Unit
  , warn :: String -> Array String -> Effect Unit
  }

sinkRef :: Ref.Ref Sink
sinkRef = unsafePerformEffect (Ref.new { trace: \_ _ -> pure unit, warn: \_ _ -> pure unit })

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

-- | Whether development diagnostics are on — for a carrier's own
-- | dev-only annotations (`PUI.Web.text`'s comment marker).
diagnosticsOn :: Effect Boolean
diagnosticsOn = Ref.read diagnosticsRef

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
  sink.warn msg []

-- | One-shot **starvation watchdog** for a knowledge gate. Every gated
-- | combinator withholds what it cannot yet complete — correct, but
-- | *silent*: an unprimed gate renders as a blank screen with no
-- | diagnostic. The guard turns that into a self-explaining failure:
-- | `blocked msg labels` (called on each withheld emission or input) arms a
-- | timer on its first call; if the gate hasn't opened (`fed`) within 3
-- | seconds, a single console warning prints `msg`, naming the gate and what
-- | it is waiting for — and hands the sink `labels`, the missing fields as
-- | data (the record merges pass the starving sibling's labels; guards with
-- | no field locus pass `[]`), so the browser sink can log the labels'
-- | stamped host elements beside the message. Fires at most once per gate
-- | instance, and only under `setDiagnostics true`.
gateGuard :: Effect { blocked :: String -> Array String -> Effect Unit, fed :: Effect Unit }
gateGuard = do
  fedRef <- Ref.new false
  armedRef <- Ref.new false
  pure
    { blocked: \msg labels -> do
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
                sink.warn msg labels
    , fed: Ref.write true fedRef
    }

renderFieldNames :: Array String -> String
renderFieldNames [] = "{}"
renderFieldNames ls = "{ " <> joinWith ", " ls <> " }"

-- | Make a status an **event pass-through stage** — the gated displays'
-- | sibling on
-- | the `+`-diagonal: every event flowing
-- | through is forwarded exactly once, at feed time, and the events the
-- | status consumes are also shown — `snackbar @"Charge card" chargingLine
-- | # observed` narrates a charge on its way to the action without
-- | interrupting it (payment). The status
-- | owns its cases within the stage's row (`Contractable s f`, the
-- | shot/focus relation `splitVariant` dispatches by): the focus
-- | cases are contracted out and shown, the background cases pass
-- | untouched. The status's own emissions are dropped, deliberately —
-- | events are one-shot, so re-emitting the last event would duplicate it —
-- | and the drop is lossless by type: like the gated displays, `observed` accepts
-- | only a status whose output is `{}`.
-- |
-- | **Derived**: the wire and the status, merged by the
-- | `× → +` merge over the event nested as a record field — the status
-- | behind `left` so it sees only its contracted cases, `>>> silence` so its
-- | emissions leave as nothing.
observed
  :: forall m v v1
   . MonadEffect m
  => Contractable v v1
  => PUI m [ | v1 ] {}
  -> PUI m [ | v ] [ | v ]
observed status = dimap (\v -> { event: v }) (match { forwarded: identity }) $ recordToVariant
  (dimap _.event (inj (Proxy @"forwarded")) identity :: PUI m { event :: [ | v ] } [ forwarded :: [ | v ] ])
  (lcmap (\r -> maybe (Right unit) Left (contract r.event)) (left (status >>> silence)) >>> lcmap (const {}) silence :: PUI m { event :: [ | v ] } [ forwarded :: [ | v ] ])

-- | The **tick source**, opened by its status like a fold or an action
-- | (`unshown @"Clock ticked" # ticks tickPeriod`, timer): every
-- | `interval`, the row it was last fed leaves as case `l`, told to the
-- | status first — replay is `Strong`'s retention, the fed row riding the
-- | state channel as under `clicked`. Feeds are retained, never answered;
-- | nothing ticks before the first feed or while the period is a hole
-- | (guardrails L18). A beat in a suspended scope (a detached pane) is
-- | skipped, and a cancelled scope ends the loop.
ticks
  :: forall m l r v
   . Scoped m
  => RowToList v (RL.Cons l { | r } RL.Nil)
  => IsSymbol l
  => Cons l { | r } () v
  => { ms :: Number }
  -> PUI m [ | v ] {}
  -> PUI m { | r } [ | v ]
ticks interval status = wrap do
  status' <- unwrap status
  sc <- scope
  lastRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \r -> Ref.write (Just r) lastRef
    , fromUser: \prop -> if isHole interval then pure unit else do
        status'.fromUser \_ -> pure unit
        let
          loop = do
            delay (Milliseconds interval.ms)
            cancelled <- liftEffect $ scopeCancelled sc
            unless cancelled do
              liftEffect do
                active <- scopeActive sc
                fed <- Ref.read lastRef
                when active $ for_ fed \r -> do
                  let occurrence = inj (Proxy @l) r
                  status'.toUser occurrence
                  prop occurrence
              loop
        launchAff_ loop
    }


-- Optics

type Action s t a b = forall m. MonadEffect m => Optic (PUI m) s t a b

-- | The transpose of an optic. `Optic p s t a b = p a b -> p s t`, and the
-- | optic families quantify the *carrier* (`Lens s t a b = forall p. Strong
-- | p => Optic p s t a b`) to gain access to the data. Fix the carrier and
-- | quantify the *data* instead and you gain access to the carrier: a
-- | shape-preserving decorator — a design system's chrome — that by
-- | parametricity cannot read or produce a single value flowing through it.
-- |
-- | It is the endomorphism monoid of `p` (`identity` and `<<<`; here, carrier
-- | nesting). Naturality is free, so every ocular commutes with the
-- | `dimap`-only adopters (`atField`/`atCase`, `toField`/`toCase`) and
-- | the focus `focusField`.
-- | Commuting with the *strengths* is the extra law, and the admission test
-- | for anything called an ocular:
-- |
-- | ```
-- | ocular (first w) = first (ocular w)
-- | ocular (left w)  = left (ocular w)
-- | ```
-- |
-- | It holds for everything that merely wraps nodes, which is why chrome
-- | slides freely past `focusField @l`. A decorator that captures —
-- | buffers, replays or withholds a feed — is still a natural transformation
-- | but breaks these, and needs a stated protocol instead (a modal, say).
-- | Both halves are tested in test/Main.purs: a node-wrapping ocular (a
-- | construction-time effect, untouched channels) satisfies the law; a
-- | capturing decorator counting emissions fails it before priming, seeing
-- | inside the gate what the gate drops outside.
type Ocular p = forall a b. Optic p a b a b

-- | An element with **nothing in it**: an ocular applied to the wire — a
-- | ripple, a focus ring, a decorative circle, an empty cell. Reads
-- | nothing, changes nothing: like every static it is the wire at its row,
-- | so it is a line of a `Semigroupoid.do` block beside `staticText` and
-- | `staticHTML`, the other two statics. `static (span >>> cl
-- | "mdc-button__ripple")`.
static :: forall p r. Category p => Ocular p -> p { | r } { | r }
static o = o identity

-- | An action is **opened by its statuses**: the slot is a `+→×` citizen
-- | owning any of the run's cases — the two lifecycle occurrences `started`
-- | and `ended` (a progress indicator, `indeterminateLinearProgress # action createPerson`) and the **outcome cases** the `Aff`
-- | returns as a variant (`snackbar @"Flight booked" bookedLine # action
-- | submit`), or a `VariantToRecord.do` block of both. The slot's row is
-- | `[ started, ended | t ]`, so a status names only the cases it shows,
-- | and the outcome leaves as the variant `[ | t ]` named by the `Aff`, so
-- | no `toCase` follows. A loop's actions stand in one
-- | `VariantToVariant.do`, one line per case, each typed at the block's
-- | outcome row — one row, an equality — which the fold block after it
-- | fixes (crud, reorder), or which `@( … )` declares where only statuses
-- | follow (flight-booker). No application owns a
-- | "busy": the indicator shows between `started` and `ended` and, like
-- | every status, owes the channel nothing. A stage with nothing to show
-- | opens with `unshown @l` naming its outcome case, which determines
-- | the slot's row.
-- |
-- | A failing action is **reported, not swallowed**: `ended` is dispatched
-- | whichever way the `Aff` ends — so a throw cannot strand the spinner —
-- | and the error reaches the diagnostics sink by name. Nothing is posted
-- | onward, since there is no output to post. An action whose function is a
-- | hole never runs (guardrails L18): a view's own literal seed may reach
-- | it before its logic exists. An outcome that settles in a suspended or
-- | cancelled scope — the pane that held the action detached meanwhile — is
-- | dropped: the case the model left gets no occurrence from it, so a
-- | detached action is `silence` like a detached button; the slot is still
-- | told `ended`, so a re-attached indicator is not left running.
action
  :: forall @t a v1 m
   . Scoped m
  => Contractable ( started :: {}, ended :: {} | t ) v1
  => (a -> Aff [ | t ])
  -> PUI m [ | v1 ] {}
  -> PUI m a [ | t ]
action arr = running \i -> if isHole arr then Nothing else Just (arr i)

-- The run behind `action`: `Nothing` is a hole, which
-- never runs (guardrails L18); otherwise `started` and `ended` are told to
-- the slot around the `Aff`, and its outcome is posted onward and told too.
running
  :: forall t a v1 m
   . Scoped m
  => Contractable ( started :: {}, ended :: {} | t ) v1
  => (a -> Maybe (Aff [ | t ]))
  -> PUI m [ | v1 ] {}
  -> PUI m a [ | t ]
running run w = action'
  (\i pro post active -> case run i of
    Nothing -> pure unit
    Just aff -> do
      let tell v = for_ (contract (v :: [ started :: {}, ended :: {} | t ]) :: Maybe _) pro
      liftEffect $ tell (inj (Proxy @"started") {})
      result <- attempt aff
      liftEffect $ tell (inj (Proxy @"ended") {})
      case result of
        Left err -> liftEffect $ warn $ "action: the Aff failed and nothing was emitted — " <> message err
        Right o -> liftEffect do
          on <- active
          if on
            then do
              post o
              -- the outcome widened to the slot's row: a variant is a tagged
              -- value, so the widening is the identity. It stays a coercion
              -- because the solver cannot discharge `expand`'s
              -- `Union t ( started, ended ) ( started, ended | t )` — a
              -- `Union` whose open tail is on the left (tried again
              -- 2026-10-11)
              tell (unsafeCoerce o)
            else tr "action: outcome dropped, the scope is suspended or cancelled" o)
  w

-- | One case **folded into the record** (`+→×`), opened by its status —
-- | `snackbar @"Count" countedLine # fold increment`, as a run is opened by
-- | its progress indicator (`indeterminateLinearProgress # action
-- | createPerson`). The status names the case (its closed singleton row is
-- | the fold's input) and shows the **event**, never the fold's result: it
-- | is fed the occurrence as it arrives, so its copy function is typed at
-- | the payload, and whatever a status says must be in the event. `f` of
-- | the payload is the row released. A heartbeat nobody needs told about
-- | opens with `unshown @l`. A loop's folds are one per event case,
-- | merged by `VariantToRecord.do`, each releasing the whole next model.
-- | Memoryless: a replaying emitter's payload is the row it was fed, an
-- | event with something of its own arrives `joined @l` with that row as
-- | `{ event, model }`, and an effect returns the model (`… # captured`,
-- | which is `fold identity`), so every handler has the model in hand and the loop's
-- | memory stays at the emitters and in the knot.
-- |
-- | **Derived**: the wire and the status under the `×→×` merge, fed the
-- | occurrence's outcome and the occurrence in one two-field record — the
-- | status contributes no field and is inert to the gate, the wire answers
-- | every feed with the row, released once per feed. (A carrier-free
-- | `fold @l f` once lived in `Data.Profunctor.Row.VariantToRecord`, the
-- | statuses standing beside the folds as sibling operands; CHANGELOG,
-- | 2026-10-07.)
fold
  :: forall m l a v r
   . MonadEffect m
  => RowToList v (RL.Cons l a RL.Nil)
  => IsSymbol l
  => Cons l a () v
  => (a -> { | r })
  -> PUI m [ | v ] {}
  -> PUI m [ | v ] { | r }
fold f status = dimap (\v -> { event: v, model: on (Proxy @l) f case_ v }) _.model $ recordToRecord
  (dimap _.model { model: _ } identity :: PUI m { event :: [ | v ], model :: { | r } } { model :: { | r } })
  (lcmap _.event status :: PUI m { event :: [ | v ], model :: { | r } } {})

-- | One case folded into the record by **resetting** it to `x`, opened by
-- | its status like `fold` (`unshown @"New game" # reset openingPosition`):
-- | the payload is the status's alone, the row released is `x` whole.
-- | `reset x = fold (const x)`.
reset
  :: forall m l a v r
   . MonadEffect m
  => RowToList v (RL.Cons l a RL.Nil)
  => IsSymbol l
  => Cons l a () v
  => { | r }
  -> PUI m [ | v ] {}
  -> PUI m [ | v ] { | r }
reset x = fold (const x)

-- | One case folded into the record by **capturing** its payload as the
-- | model whole, opened by its status like `fold`
-- | (`snackbar @"Order loaded" orderLoadedLine # captured`): the case an
-- | effect returns the model under — a load, a fetch, a server's answer.
-- | `captured = fold identity`.
captured
  :: forall m l v r
   . MonadEffect m
  => RowToList v (RL.Cons l { | r } RL.Nil)
  => IsSymbol l
  => Cons l { | r } () v
  => PUI m [ | v ] {}
  -> PUI m [ | v ] { | r }
captured = fold identity

action' :: forall a b s t m. Scoped m => (s -> (a -> Effect Unit) -> (t -> Effect Unit) -> Effect Boolean -> Aff Unit) -> Optic (PUI m) s t a b
action' arr w = wrap do
  w' <- unwrap w
  sc <- scope
  oVar <- liftEffect AVar.empty
  pure
    { toUser: \i -> launchAff_ $ arr i (\a -> void $ w'.toUser a) (\o -> void $ AVar.put o oVar mempty) (scopeActive sc)
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
-- | `seeded` wire, the one place a seeded `×`-side trace survives.
debounced :: forall m. MonadEffect m => { ms :: Number } -> Ocular (PUI m)
debounced millis w = coresolve (resolveFor millis w >>> seeded (Right unit))

-- The container action on PUI (class in Data.Profunctor.Acting — the pure
-- algebra; instances live with the carriers, like the merge instances above).
--
-- Five collection combinators share one instance discipline and fill the
-- 2×2 of the shape square. The element is always a data-model row and
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
-- (exists only when everyone has spoken; for element editors, which answer
-- every feed, that is every feed and every edit) → acted; the aggregate as
-- running state of a stream → accumulated. foreach cannot be acted (a gate
-- over event sources is absurd; Array b erases "which one") — and the
-- +-members never
-- detach or restack: the absence of a key is no signal, so removal and
-- ordering are array-level concerns of some ×-stage upstream.
--
-- The key's form encodes its ontology, two ways: a **label @l** on the
-- ×-members (identity is a materialized field of the model row — and in the
-- product-output member `acted` the element's output row EXCLUDES
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
  { instantiate :: Effect { feed :: a -> Effect Unit, subscribe :: (b -> Effect Unit) -> Effect Unit, node :: node, cancel :: Effect Unit }
  , detach :: node -> Effect Unit
  , restack :: Array node -> Effect Unit
  }

-- | Placement-free: an element instance is just its channel legs, built in
-- | a scope of its own under the ambient one, cancelled when it leaves.
instance Hosting Effect Unit where
  hosting w = pure
    { instantiate: do
        outer <- scope
        child <- newScope (Just outer) Active
        inst <- inScope child (unwrap w)
        pure { feed: inst.toUser, subscribe: inst.fromUser, node: unit, cancel: cancelScope child }
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
-- | @"id" rowsOf` — keyed by the row's **materialized identity field** (the
-- | element is a data-model row; rows carry their identity). The element
-- | is a display of its row — it releases the row it shows — and the list
-- | is a display of the row it is fed: the elements' releases stay inside
-- | and the fed row passes, so a list of displays is a line of the
-- | pipeline like any display. An array the model stores is `actedAt`,
-- | the container action, which writes the elements' releases back; a
-- | list whose elements **emit** is `anyOf`. Both share this keyed
-- | reconciler.
foreach :: forall @l @r m node k b a. Hosting m node => IsSymbol l => Cons l k b r => (a -> Array { | r }) -> PUI m { | r } { | r } -> PUI m a a
foreach f w = withStructuralOrd @k $ wrap do
  hooks <- hosting w
  liftEffect do
    list <- collapsedWith (Record.get (Proxy @l)) hooks
    list.fromUser \_ -> pure unit
    propRef <- Ref.new Nothing
    pure
      { toUser: \a -> do
          list.toUser (if isHole f then [] else let xs = f a in if isHole xs then [] else xs)
          Ref.read propRef >>= traverse_ (_ $ a)
      , fromUser: \prop -> Ref.write (Just prop) propRef
      }

-- | The **emitting collection** — `foreach`'s `×→+` sibling: the same keyed
-- | and retaining reconciliation over elements that emit (a clickable row,
-- | a cell reporting its key). Every element's emission collapses onto one
-- | shared channel as it happens, ungated and lawfully **silent on an empty
-- | array** (no event to fabricate):
-- | `clicked @"Key entered" @"key" cell # anyOf @"key" keyPad`.
anyOf :: forall @l @r m node k b a v. Hosting m node => IsSymbol l => Cons l k b r => (a -> Array { | r }) -> PUI m { | r } [ | v ] -> PUI m a [ | v ]
anyOf f w = withStructuralOrd @k (lcmap (\r -> if isHole f then [] else let xs = f r in if isHole xs then [] else xs) $ wrap do
  hooks <- hosting w
  liftEffect $ collapsedWith (Record.get (Proxy @l)) hooks)

-- | `anyOf` over an array the model stores: keyed by field `l` of each
-- | element, the array field `t` of the fed row, both named by label
-- | (`# anyOfAt @"key" @"cells"`, cells). An array derived from the row is
-- | a projection, `anyOf`.
anyOfAt :: forall @l @t m node k b1 b2 r1 r v. Hosting m node => IsSymbol l => IsSymbol t => Cons l k b1 r1 => Cons t (Array { | r1 }) b2 r => PUI m { | r1 } [ | v ] -> PUI m { | r } [ | v ]
anyOfAt = anyOf @l @r1 (Record.get (Proxy @t))

-- | The **keyed dispatch** — the +→+ member: one runtime case at a time.
-- | A fed `{ key, value }` reaches exactly the instance whose key matches —
-- | instantiated on first appearance (an unknown key is a new case, not an
-- | error) — and that instance's emissions leave tagged with its key. The
-- | targeted-update direction: no whole-array re-feed, O(1) per case, the
-- | shape for streams/pushes that arrive one entity at a time. No key
-- | function: the runtime variant input carries its tag, as a variant case
-- | carries its label.
dispatched :: forall @k @a m node i b. Hosting m node => (i -> { key :: k, value :: a }) -> PUI m a b -> PUI m i { key :: k, value :: b }
dispatched f w = withStructuralOrd @k (wrap do
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
      -- a hole projection is absent (guardrails L18): the feed reaches no case
      { toUser: \i -> if isHole f then pure unit else do
          let u = f i
          e <- ensureInstance hooks onEmit indexRef orderRef u.key
          e.feed u.value
      , fromUser: \prop -> Ref.write (Just prop) propRef
      })

-- | The **keyed Mealy** — the +→× member: retain the array, feed one case,
-- | emit the whole. Each fed `{ key, value }` updates (or, on a new key,
-- | appends) its slot and re-emits the whole array immediately — input-primed
-- | so there is never a hole to withhold over; element
-- | emissions fold back into their slot the same way. The board/ledger shape
-- | for keyed streams: the aggregate as running state, built one entity at a
-- | time. Emits `[]` for no keys yet only in the sense that nothing has been
-- | fed; order is first-appearance order.
accumulated :: forall @k @a m node i. Hosting m node => (i -> { key :: k, value :: a }) -> PUI m a a -> PUI m i (Array a)
accumulated f w = withStructuralOrd @k (wrap do
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
      { toUser: \i -> if isHole f then pure unit else do
          let u = f i
          Ref.write true busyRef
          e <- ensureInstance hooks onEmit indexRef orderRef u.key
          Ref.write (Just u.value) e.slot
          e.feed u.value
          Ref.write false busyRef
          emitAll
      , fromUser: \prop -> Ref.write (Just prop) propRef
      })

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
      let e = { key: k, feed: inst.feed, slot, node: inst.node, cancel: inst.cancel }
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
  , cancel :: Effect Unit
  }

-- Reconcile the entry vector against a fed array: survivors re-fed in place,
-- entrants instantiated (their emissions wired to `onEmit` over their own
-- slot), leavers detached and their scopes cancelled, nodes restacked only
-- when the key sequence changed. The busy guard stops an element echo from double-building
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
          pure { key: k, feed: inst.feed, slot, node: inst.node, cancel: inst.cancel }
    let keep = Set.fromFoldable (map _.key entries)
    for_ old \e -> unless (Set.member e.key keep) (hooks.detach e.node *> e.cancel)
    when (map _.key old /= map _.key entries) $ hooks.restack (map _.node entries)
    Ref.write entries entriesRef
    Ref.write false busyRef

-- Skip a feed that arrives while a reconcile is in flight: the whole step,
-- rekeying included, so a mid-reconcile re-feed can neither enrol keys the
-- reconcile is not building nor gather a half-updated vector.
actingGuarded :: Ref.Ref Boolean -> Effect Unit -> Effect Unit
actingGuarded busyRef act = do
  busy <- Ref.read busyRef
  unless busy act

-- The gather mode: the record gate at runtime labels (`PUI.Gate`, run by
-- `driveGate` exactly as the merges run it). A feed is one step: it rekeys
-- the gate to the fed key sequence — survivors keep their retained slot,
-- entrants are unknown, leavers are forgotten — then reconciles, and the
-- step's single release gathers the whole vector once every element has
-- spoken: `[]` releases `[]`, survivors re-release from retained slots
-- without waiting, and an echo provoked *during* the reconcile (an element
-- answering its feed, per the record-echo protocol) lands but cannot gather
-- a half-updated vector (the wire-law test in test/Main.purs; conformance
-- to the pure step over every script in test/Exhaustive.purs). Between
-- feeds each element emission contributes its one slot and the gate
-- re-releases the whole, retain-last. Rekeying precedes the reconcile so
-- that an entrant's echo finds its participant enrolled.
actedWith :: forall k a b node. Ord k => (a -> k) -> Hooks a b node -> Effect { toUser :: Array a -> Effect Unit, fromUser :: (Array b -> Effect Unit) -> Effect Unit }
actedWith key hooks = do
  gate <- newGate []
  entriesRef <- Ref.new []
  busyRef <- Ref.new false
  let
    traced = case _ of
      Withheld missing -> tr "acted: vector withheld (elements not heard from yet)" missing
      _ -> pure unit
    onEmit k _ b = driveGate gate (Contributed [ Tuple k b ]) >>= traced
  pure
    { toUser: \items -> actingGuarded busyRef do
        out <- steppedFeed Shared gate do
          _ <- driveGate gate (Rekeyed (map key items))
          reconcileKeyed key hooks onEmit busyRef entriesRef items
        traced out
    , fromUser: \prop -> Ref.write (Just (prop <<< map snd)) gate.prop
    }

-- The forward (collapsed) mode: element emissions exit onto the shared
-- channel as they happen; the slot is kept written so a later gather-mode
-- reading of the same core stays possible, but nothing gates.
collapsedWith :: forall k a b node. Ord k => (a -> k) -> Hooks a b node -> Effect { toUser :: Array a -> Effect Unit, fromUser :: ((b -> Effect Unit) -> Effect Unit) }
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
