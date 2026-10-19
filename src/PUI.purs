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
-- | process reached in three phases: constructed in `m`, then registered
-- | once, which hands the downstream continuation over and returns the
-- | input leg, then feeds and emissions interleaving. A feed cannot precede
-- | registration, so every instance keeps its state — a gate, a retained
-- | pair, a re-entry cell — in the registration `Effect`, allocated once per
-- | instantiation, and asks of `m` only the structure it composes with:
-- | `Functor` for one operand, `Apply` for two, `Applicative` for none.
-- | `MonadEffect m` is left to what genuinely builds in `m` — the scope a
-- | word reads (`Scoped`), the host placing collection elements
-- | (`Hosting`) — and to `handles`, the two-leg bridge. The protocol, `≈`, `⊑` and the component laws every
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
  , Registration
  , Handles
  , fromHandles
  , handles
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
  , attachable
  , setSink
  , setTracing
  , setDiagnostics
  , diagnosticsOn
  , isHole
  -- carrier plumbing the vocabularies and the law tests build on, not
  -- application vocabulary: no demo writes one (`with` closes over
  -- `announce`, `debounced` over `seeded` and `resolveFor`)
  , announce
  , seeded
  , resolveFor
  , folding
  , recordToRecordWiring
  , recordToVariantWiring
  , variantToRecordWiring
  , variantToVariantWiring
  , selectedAt
  , selectedUnpickedAt
  , selectedOptionalAt
  , replayed
  , source
  , class Interacting
  , gestured
  , focused
  , Gesture(..)
  , occurrencesOn
  , departures
  -- the vocabulary: what a view writes, with the words `Adopters` re-exports
  , action
  , load
  , fold
  , reset
  , capture
  , static
  , ticks
  , accumulated
  , debounced
  , dispatched
  , foreach
  , anyOf
  , anyOfAt
  , anyEdit
  , with
  , looped
  , inCase
  , inCaseAt
  , provided
  , providedAt
  , introduced
  , module Adopters
  )
  where

import Data.Profunctor.Row.Structural (withStructuralEq, withStructuralOrd)
import Prelude

import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Foldable (elem, foldl, for_)
import Data.Lens (Optic)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype)
import Data.Map as Map
import Data.Set as Set
import Data.Profunctor (class Profunctor, dimap, lcmap, rmap)
import Data.Profunctor.Acting (class Acting)
import Data.Profunctor.Choice (class Choice, left)
import Data.Profunctor.Cochoice (class Cochoice)
import Data.Profunctor.Costrong (class Costrong)
import Data.Profunctor.PointedCostrong (class PointedCostrong)
import Data.Profunctor.Row.RecordToRecord (class RecordToRecord, focusField, recordToRecord)
-- the words a view writes beside the leaves, re-exported so demos need the
-- row modules only for the `.do` merges and the knot. `focusField` is
-- deliberately absent: the leaf lift is design-system plumbing — every
-- vocabulary editor is `focusField @l`-lifted inside, the labelled group
-- (`group @l`) carries sub-model nesting, so application code never lifts a
-- focus itself. No placement word is here either: every leaf narrows its
-- input and widens its output itself — a status or an action owns the one
-- case its label names, an emitter is open on its output, an action is
-- typed at exactly its outcomes and `joined` widens it into its block — so a stage is typed at one
-- row (the gated displays, `settled`, `acted`) and a UI component's own
-- row is always stated by a business function, never placed at another
-- row at the call site. The floor's single-label adopters (`atField`/
-- `atCase`, `toField`) stay inside the vocabularies, where a face is
-- adopted at its label; `toCase` alone is a view's, the one shape change
-- a business case can name (an editor's release leaving as an event).
-- `{}`-input chrome is placed by the vocabulary's `eventLeaf` with a bare
-- `lcmap (const {})`.
import Data.Profunctor.Row (settled) as Adopters
import Data.Profunctor.Row.RecordToVariant (joined, joined2, kept) as Adopters
import Data.Profunctor.Acting (acted, actedAt) as Adopters
import Data.Profunctor.Coresolving (class Coresolving, coresolve)
import Data.Profunctor.Resolving (class Resolving)
import Data.Profunctor.Row.RecordToVariant (class RecordToVariant)
import Data.Profunctor.Row (class RowLabels, atCase, exactRow, rowLabels)
import Data.String (joinWith)
import Data.Profunctor.Coretaining (class Coretaining)
import Data.Profunctor.Retaining (class Retaining)
import Data.Profunctor.Row.VariantToRecord (class VariantToRecord, unshown)
import Data.Profunctor.Row.VariantToVariant (class VariantToVariant)
import Data.Profunctor.Strong (class Strong)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for, sequence, sequence_, traverse_)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Symbol (class IsSymbol)
import Data.Variant (class Contractable, case_, contract, inj, on, prj)
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (Aff, attempt, delay, error, forkAff, killFiber, launchAff_, message)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import PUI.Gate (GateInput(..), GateOutput(..), GateState, StepKind(..), gateStep, initialGate)
import Record (get, insert) as Record
import Record.Unsafe (unsafeGet, unsafeSet)

--------------------------------------------------------------------------------
-- The carrier
--------------------------------------------------------------------------------

-- | Built in `m`, then **registered**: handed the downstream continuation,
-- | registration returns the input leg. A feed therefore cannot precede
-- | registration, and a stage that only forwards needs no state: the
-- | continuation is in scope where the input leg is made.
newtype PUI m i o = PUI (m (Registration i o))

type Registration i o = (o -> Effect Unit) -> Effect (i -> Effect Unit)

-- | The two legs of a component as separate closures — the shape most
-- | leaves are written in: `toUser` takes a feed, `fromUser` subscribes.
type Handles i o =
  { toUser :: i -> Effect Unit
  , fromUser :: (o -> Effect Unit) -> Effect Unit
  }

-- | A component from its two legs: registering subscribes, then hands back
-- | the input leg.
fromHandles :: forall m i o. Functor m => m (Handles i o) -> PUI m i o
fromHandles = PUI <<< map \h prop -> h.fromUser prop $> h.toUser

-- | The two legs of a component, for code that subscribes and feeds a
-- | child separately. The component registers on whichever leg is used
-- | first, its continuation read through a ref: a child that is only fed
-- | (chrome a leaf drives and never listens to) works unsubscribed, and an
-- | emission before `fromUser` reaches no one.
handles :: forall m i o. MonadEffect m => PUI m i o -> m (Handles i o)
handles (PUI mreg) = do
  reg <- mreg
  propRef <- liftEffect $ Ref.new Nothing
  -- Nothing: not registered; Left: registering, feeds held (a component
  -- re-feeding itself from an emission inside its own registration);
  -- Right: the input leg
  legRef <- liftEffect $ Ref.new Nothing
  let
    registered = do
      Ref.write (Just (Left [])) legRef
      leg <- reg \o -> Ref.read propRef >>= traverse_ (_ $ o)
      held <- Ref.read legRef
      Ref.write (Just (Right leg)) legRef
      for_ held (either (traverse_ leg) (const (pure unit)))
    fed i = Ref.read legRef >>= case _ of
      Nothing -> registered *> fed i
      Just (Left held) -> Ref.write (Just (Left (held <> [ i ]))) legRef
      Just (Right leg) -> leg i
  pure
    { toUser: fed
    , fromUser: \prop -> do
        Ref.write (Just prop) propRef
        Ref.read legRef >>= case _ of
          Nothing -> registered
          _ -> pure unit
    }

-- The leaf protocol, as functions, one per shape
-- a leaf answers in, named after it: every leaf of every vocabulary is one
-- of these wirings over its own element (an emitter is `eventLeaf`, a
-- derivation), so
-- the echo a gate depends on, the busy guard around a programmatic write
-- and the one-registration listener are written once here and nowhere
-- else.

-- | The **`×→×` leaf**'s wiring — an editor's, and a display's, which is
-- | an editor the user never changes (`held` false, `reports` registering
-- | nothing, `read` `pure`): `show` writes each fed value to the
-- | element unless `held` says the user is in it (the focus guard — a model
-- | update never clobbers a field being typed in); every feed is **echoed**
-- | whether written or not, so the lifted stage releases the row and an
-- | enclosing gate opens; `reports` registers the platform event that
-- | means the user changed the value, and `read` reads that value off the
-- | element given the row last fed (a slider reads its position into the
-- | quantity it was shown). A report while a write is in progress is the
-- | write's own echo and is dropped, and a report before any feed has
-- | nothing to complete and is dropped too.
recordToRecordWiring
  :: forall a
   . { show :: a -> Effect Unit
     , held :: Effect Boolean
     , reports :: Effect Unit -> Effect Unit
     , read :: a -> Effect a
     }
  -> Effect (Handles a a)
recordToRecordWiring leaf = do
  propRef <- Ref.new Nothing
  lastRef <- Ref.new Nothing
  busyRef <- Ref.new false
  pure
    { toUser: \a -> do
        Ref.write (Just a) lastRef
        held <- leaf.held
        unless held do
          Ref.write true busyRef
          leaf.show a
          Ref.write false busyRef
        Ref.read propRef >>= traverse_ (_ $ a)
    , fromUser: \prop -> do
        Ref.write (Just prop) propRef
        leaf.reports do
          busy <- Ref.read busyRef
          unless busy do
            last <- Ref.read lastRef
            for_ last \a -> leaf.read a >>= prop
    }

-- | The **`×→+` leaf**'s wiring, a selector's over its option list: it
-- | answers no feed and reports the user's pick — the `×→+` modality at
-- | the leaf, the `×→×` answer added by the lift around it. In index terms:
-- | `show` checks the option at the index (none for `Nothing`), `reports`
-- | registers the platform's pick event, `read` reads the picked index
-- | (`Nothing` for a cleared control). The option's value travels through
-- | `selectedAt` and its siblings, which lift this `Maybe a` leaf to the
-- | field editor and answer the feed themselves — so, unlike
-- | `recordToRecordWiring`, nothing is echoed here. A pick reported while a
-- | programmatic selection is in progress is that selection's own event
-- | and is dropped.
recordToVariantWiring
  :: Array String
  -> { show :: Maybe Int -> Effect Unit
     , reports :: Effect Unit -> Effect Unit
     , read :: Effect (Maybe Int)
     }
  -> Effect (Handles (Maybe String) (Maybe String))
recordToVariantWiring options leaf = do
  propRef <- Ref.new Nothing
  busyRef <- Ref.new false
  pure
    { toUser: \ma -> do
        Ref.write true busyRef
        leaf.show (ma >>= \a -> Array.findIndex (\o -> o == a) options)
        Ref.write false busyRef
    , fromUser: \prop -> do
        Ref.write (Just prop) propRef
        leaf.reports do
          busy <- Ref.read busyRef
          unless busy do
            mi <- leaf.read
            Ref.read propRef >>= traverse_ (_ $ (mi >>= (Array.index options)))
    }

-- | The **`+→×` leaf**'s wiring, a status's: `show` renders each occurrence, and the
-- | status releases nothing — the `+→×` merge forwards every release as
-- | the whole shared row, so a status is typed at every row, as the
-- | silent component is.
variantToRecordWiring :: forall a b. (a -> Effect Unit) -> Handles a b
variantToRecordWiring show = { toUser: show, fromUser: \_ -> pure unit }

-- | The **`+→+` leaf**'s wiring, a dialog's: the leaf is `a → a`, a
-- | case's payload, lifted by `focusCase @l` into the event row as an
-- | editor's `a → a` is lifted by `focusField @l` into the model row. On a
-- | feed it `open`s and feeds the payload to its `content` — the stage the
-- | dialog shows, a display or editors — and answers **nothing**: a `+→+`
-- | leaf may wait, and a dialog waits for the user. When `reports` fires —
-- | the deciding button — it `close`s and releases the row the content
-- | last answered, once; a decision before any feed has nothing to release
-- | and is dropped, as is a second one before the next feed. When
-- | `cancels` fires — the cancel button — it `close`s and releases
-- | nothing, forgetting the row, as does a dismiss the platform performs
-- | itself (Escape, the scrim). `acknowledged` drops the release
-- | (`onCase @l`, the case consumed) and has no cancel; `confirmed`
-- | re-emits it (`focusCase @l`).
variantToVariantWiring
  :: forall a
   . { open :: Effect Unit
     , close :: Effect Unit
     , content :: Handles a a
     , reports :: Effect Unit -> Effect Unit
     , cancels :: Effect Unit -> Effect Unit
     }
  -> Effect (Handles a a)
variantToVariantWiring leaf = do
  shownRef <- Ref.new Nothing
  pure
    { toUser: \a -> do
        leaf.open
        leaf.content.toUser a
    , fromUser: \prop -> do
        leaf.content.fromUser \a -> Ref.write (Just a) shownRef
        leaf.reports do
          shown <- Ref.read shownRef
          for_ shown \a -> do
            Ref.write Nothing shownRef
            leaf.close
            prop a
        leaf.cancels do
          Ref.write Nothing shownRef
          leaf.close
    }


-- | Lift a bare selection leaf — the option to check in (`Nothing`: none),
-- | the option the user checked out (`Nothing`: cleared) — to the
-- | **selection editor** of field `l`, a whole-row citizen
-- | `{ l :: a | rest } → { l :: a | rest }` like every other editor: the
-- | widget stores a value, so it has an editor's shape and owes an editor's
-- | answer — every feed is answered with the row, and every pick is stored
-- | into field `l`. Here the field is the option itself, so the model has a
-- | choice at all times. Vocabulary plumbing, beside `focusField @l`: every
-- | plain selector in every vocabulary is its leaf lifted with this.
selectedAt :: forall @l m a b r. MonadEffect m => IsSymbol l => Cons l a b r => PUI m (Maybe a) (Maybe a) -> PUI m { | r } { | r }
selectedAt = selectedWith @l Just identity

-- | `selectedAt` for a choice owed but not yet made: field `l` is a variant
-- | whose case `c` is the made choice, seeded at an unpicked case the
-- | application names. Every other case shows nothing checked, a pick
-- | stores case `c` and there is no way back, so the stages demanding the
-- | selection adopt the made case. Every `…Unpicked` selector is its leaf
-- | lifted with this.
selectedUnpickedAt :: forall @l @c m a b1 v b r. MonadEffect m => IsSymbol l => IsSymbol c => Cons c a b1 v => Cons l [ | v ] b r => PUI m (Maybe a) (Maybe a) -> PUI m { | r } { | r }
selectedUnpickedAt = selectedWith @l (prj (Proxy @c)) (map (inj (Proxy @c)))

-- | `selectedAt` for a choice the user may leave unmade: field `l` is a
-- | variant whose case `c` is the made choice and whose case `n` is none,
-- | and clearing the widget stores case `n`. Every `…Optional` selector is
-- | its leaf lifted with this.
selectedOptionalAt :: forall @l @c @n m a b1 b2 v b r. MonadEffect m => IsSymbol l => IsSymbol c => IsSymbol n => Cons c a b1 v => Cons n {} b2 v => Cons l [ | v ] b r => PUI m (Maybe a) (Maybe a) -> PUI m { | r } { | r }
selectedOptionalAt = selectedWith @l (prj (Proxy @c)) (Just <<< maybe (inj (Proxy @n) {}) (inj (Proxy @c)))

-- checkedOf: the option the field shows checked; stored: what a pick (`Just`)
-- or a clear (`Nothing`) stores, if anything; `checkedOf <=< stored` must give
-- back the pick, so a pick's echo is the pick
selectedWith :: forall @l m f a b r. MonadEffect m => IsSymbol l => Cons l f b r => (f -> Maybe a) -> (Maybe a -> Maybe f) -> PUI m (Maybe a) (Maybe a) -> PUI m { | r } { | r }
selectedWith checkedOf stored w = focusField @l $ fromHandles do
  w' <- handles w
  mPropRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \v -> do
        w'.toUser (checkedOf v)
        Ref.read mPropRef >>= traverse_ (_ $ v)
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        w'.fromUser (traverse_ prop <<< stored)
    }

-- | Register with the input leg in scope of the continuation: a feedback
-- | re-entering the component from its own emission. A re-entry fired
-- | during registration is held and delivered once the leg exists.
registeredSelf :: forall i o. Registration i o -> ((i -> Effect Unit) -> o -> Effect Unit) -> Effect (i -> Effect Unit)
registeredSelf reg continuation = do
  legRef <- Ref.new (Left [])
  let
    self i = Ref.read legRef >>= case _ of
      Right leg -> leg i
      Left held -> Ref.write (Left (held <> [ i ])) legRef
  leg <- reg (continuation self)
  held <- Ref.read legRef
  Ref.write (Right leg) legRef
  either (traverse_ leg) (const (pure unit)) held
  pure leg


--------------------------------------------------------------------------------
-- Instances (the Hosting/Acting instances live with the container-action machinery below)
--------------------------------------------------------------------------------

derive instance Newtype (PUI m i o) _

instance Functor m => Profunctor (PUI m) where
  dimap pre post (PUI p) = PUI $ p <#> \reg prop -> (_ <<< pre) <$> reg (prop <<< post)

-- Stateful instances below share one gating principle (the same one the
-- record merges follow): state a UI component hasn't received yet cannot be
-- fabricated, so emissions needing it are withheld until
-- the state channel has been fed.
--
-- The price is named: the ecosystem Strong law `lmap fst = rmap fst <<< first`
-- holds only as primed equivalence — a pre-feed emission is dropped, not
-- delayed (a permanent prefix difference, tested as such in test/Main.purs;
-- doc/observational-semantics.md).
instance Functor m => Strong (PUI m) where
  first (PUI p) = PUI $ retainingPair "Strong.first" fst (\b ab -> Tuple b (snd ab)) <$> p
  second (PUI p) = PUI $ retainingPair "Strong.second" snd (\b ab -> Tuple (fst ab) b) <$> p

-- The pair state is allocated at registration, once per instantiation —
-- the phase it belongs to (doc/observational-semantics.md §1), which is
-- also what keeps every `foreach` row on its own ref: the last pair fed,
-- the inner leg shown its focus, an emission completed with the retained
-- side or withheld before any pair arrived.
retainingPair :: forall a b x y. String -> (x -> a) -> (b -> x -> y) -> Registration a b -> Registration x y
retainingPair name focus completed reg prop = do
  lastRef <- Ref.new Nothing
  guard <- gateGuard
  leg <- reg \b -> Ref.read lastRef >>= case _ of
    Nothing -> do
      guard.blocked (name <> ": emissions dropped for 3s — the pair state was never fed (no input arrived), so the gate cannot complete a Tuple. Feed the stage a first value.") []
      tr (name <> ": emission withheld (pair state unknown)") b
    Just x -> prop (completed b x)
  pure \x -> do
    guard.fed
    Ref.write (Just x) lastRef
    leg (focus x)

instance Functor m => Choice (PUI m) where
  left (PUI p) = PUI $ p <#> \reg prop -> reg (prop <<< Left) <#> \leg -> either leg (prop <<< Right)
  right (PUI p) = PUI $ p <#> \reg prop -> reg (prop <<< Right) <#> \leg -> either (prop <<< Left) leg

-- | The **introducing form of an editor**: field `l` inserted into the
-- | row fed, from `init` of the rest, in front of the lens editor — `{ | b }
-- | → { l :: a | b }`. For a knotless form, which states each field's
-- | blank at its leaf (`filledTextFieldFrom @"Email" blankEmail`, MDC3)
-- | instead of in a seed, so it needs no `looped` and no `with`; a document
-- | whose logic writes into its fields keeps the lens editor in a `looped`
-- | row, where the knot re-feeds the field's current value and a reset
-- | lands as a feed of the whole row.
-- |
-- | Not `lcmap (insert (init b))`: in a chain of such editors an upstream
-- | edit re-feeds every editor below it, which would re-show each init over
-- | the user's value. So the lift **retains** the user's last value and
-- | re-shows `init` only when `init` itself changed since the last feed
-- | (structural equality, so no `Eq` is demanded and a hole needs none).
-- | Consequences, accepted with the word (CHANGELOG, 2026-10-17): a state
-- | the row does not show, an init typed at the row the editors above it
-- | have built (screen order in the view model's signatures), and no
-- | introducing editor under a pane — a pane is `{ | r } → { | r }`, so a
-- | field whose editor exists sometimes is model state. A hole `init`
-- | (guardrails L18) introduces nothing: the feed is withheld.
introduced
  :: forall @l m a b r
   . Functor m
  => IsSymbol l
  => Lacks l b
  => Cons l a b r
  => ({ | b } -> a)
  -> PUI m { | r } { | r }
  -> PUI m { | b } { | r }
introduced init (PUI p) = PUI $ p <#> \reg prop -> do
  lastInit <- Ref.new Nothing
  current <- Ref.new Nothing
  leg <- reg \r -> do
    Ref.write (Just (Record.get (Proxy @l) r)) current
    prop r
  pure \b -> if isHole init then pure unit else do
    let i = init b
    prev <- Ref.read lastInit
    cur <- Ref.read current
    Ref.write (Just i) lastInit
    let shown = withStructuralEq @a (if prev == Just i then fromMaybe i cur else i)
    Ref.write (Just shown) current
    leg (Record.insert (Proxy @l) shown b)

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
-- | — which is why the knot at a record junction is `looped`
-- | (the self-feeding diagonal, ungated) rather than a derivation from
-- | this class. Contrast `Cochoice` below,
-- | whose retraction holds raw: this carrier is genuinely traced over `+`
-- | and only pointed-traced over `×` (doc/observational-semantics.md).
instance Functor m => Costrong (PUI m) where
  unfirst (PUI p) = PUI $ p <#> \reg prop -> do
    cRef <- Ref.new Nothing
    guard <- gateGuard
    leg <- reg \(Tuple b c) -> do
      guard.fed
      Ref.write (Just c) cRef
      prop b
    pure \a -> Ref.read cRef >>= case _ of
      Nothing -> do
        guard.blocked "Costrong.unfirst: inputs dropped for 3s — the state feedback channel was never primed (the traced UI component never emitted). Loop state through the model instead (`looped`, closed by `with`), or seed a raw `unfirst`/`colens` chain from inside (`seeded`)." []
        tr "Costrong.unfirst: input withheld (state unprimed)" a
      Just c -> leg $ Tuple a c
  unsecond (PUI p) = PUI $ p <#> \reg prop -> do
    aRef <- Ref.new Nothing
    guard <- gateGuard
    leg <- reg \(Tuple a c) -> do
      guard.fed
      Ref.write (Just a) aRef
      prop c
    pure \b -> Ref.read aRef >>= case _ of
      Nothing -> do
        guard.blocked "Costrong.unsecond: inputs dropped for 3s — the state feedback channel was never primed (the traced UI component never emitted). Loop state through the model instead (`looped`, closed by `with`), or seed a raw chain from inside (`seeded`)." []
        tr "Costrong.unsecond: input withheld (state unprimed)" b
      Just a -> leg $ Tuple a b

-- | The **pointed** `×`-trace: `unfirst` with its state `Ref` starting at
-- | `c0`, so the first input joins the starting state instead of waiting on
-- | an emission. The pointed trace at the value level; a row form gave
-- | way to looping the state through the model, so no library word stands
-- | on it now.
instance Functor m => PointedCostrong (PUI m) where
  -- a hole start (guardrails L18) is absent: the state channel is unprimed
  -- and every input is withheld, as under the gated `unfirst` — so a chain
  -- whose seed is not written yet lets nothing through to the handlers that
  -- are not written yet either
  unfirstFrom c0 (PUI p) = PUI $ p <#> \reg prop -> do
    cRef <- Ref.new (if isHole c0 then Nothing else Just c0)
    leg <- reg \(Tuple b c) -> do
      Ref.write (Just c) cRef
      prop b
    pure \a -> Ref.read cRef >>= case _ of
      Nothing -> tr "PointedCostrong.unfirstFrom: input withheld (the start is a hole)" a
      Just c -> leg $ Tuple a c

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
  unleft (PUI p) = PUI $ p <#> \reg prop -> (_ <<< Left) <$> registeredSelf reg \self -> case _ of
    Left b -> prop b
    Right c -> do
      tr "Cochoice.unleft: looping back" c
      self $ Right c
  unright (PUI p) = PUI $ p <#> \reg prop -> (_ <<< Right) <$> registeredSelf reg \self -> case _ of
    Right b -> prop b
    Left c -> do
      tr "Cochoice.unright: looping back" c
      self $ Left c

-- | The `× → +` **co-strength** (retraction of `Resolving`): a `Right c`
-- | emission is retained as the fold state and — **eagerly** — re-fed to
-- | the UI component joined with the last input (guarded), so the UI component
-- | re-renders at every fold step; a `Left b` exits. Gated like `Costrong`
-- | (a first `c` must arrive before inputs pass — `announce` an initial
-- | state to prime it). The raw composite `coresolve (resolve g)` is
-- | input-dead (each gate waits on the other), so the retraction is stated
-- | seeded — and the seeded composite IS `debounced`:
-- | `coresolve (resolve g >>> seeded (Right c0)) ≈ debounced g`.
instance Functor m => Coresolving (PUI m) where
  coresolve (PUI p) = PUI $ p <#> \reg prop -> do
    aRef <- Ref.new Nothing
    cRef <- Ref.new Nothing
    busyRef <- Ref.new false
    guard <- gateGuard
    leg <- registeredSelf reg \self -> case _ of
      Left b -> prop b
      Right c -> do
        guard.fed
        tr "Coresolving.coresolve: fold step, re-feeding" c
        Ref.write (Just c) cRef
        busy <- Ref.read busyRef
        unless busy do
          Ref.write true busyRef
          Ref.read aRef >>= traverse_ \a -> self $ Tuple a c
          Ref.write false busyRef
    pure \a -> do
      Ref.write (Just a) aRef
      Ref.read cRef >>= case _ of
        Nothing -> do
          guard.blocked "Coresolving.coresolve: inputs dropped for 3s — the fold state was never primed (no loop-branch emission arrived). Seed the loop branch (`seeded`, as `debounced` does); an application loop closes at its record junction instead (`looped`)." []
          tr "Coresolving.coresolve: input withheld (fold state unprimed)" a
        Just c -> leg $ Tuple a c

-- | The `+ → ×` **co-strength** (retraction of `Retaining`): every emission
-- | `Tuple b c` yields `b` and immediately re-enters the UI component as a
-- | `Right c` resume — a **productive unfold**/generator.
-- | The raw composite `coretain (retain g)` is output-dead (`retain`'s gate
-- | waits on a resume only emissions can trigger), so the retraction is
-- | stated seeded: `coretain (seeded (Right c0) >>> retain g) ≈ g`.
instance Functor m => Coretaining (PUI m) where
  coretain (PUI p) = PUI $ p <#> \reg prop -> do
    -- the resume re-entry is guarded: a record-output UI component echoes on
    -- its input, and an unguarded re-feed would loop on its own echo
    busyRef <- Ref.new false
    leg <- registeredSelf reg \self (Tuple b c) -> do
      prop b
      busy <- Ref.read busyRef
      unless busy do
        tr "Coretaining.coretain: resuming with state" c
        Ref.write true busyRef
        self $ Right c
        Ref.write false busyRef
    pure \a -> leg $ Left a

instance Apply m => Semigroupoid (PUI m) where
  -- downstream registers first: emissions fired during upstream's
  -- registration (announcements, seeds) must find downstream's wiring
  -- already listening — and its input leg is what upstream registers with
  compose (PUI p2) (PUI p1) = PUI ado
    reg1 <- p1
    reg2 <- p2
    in \prop -> do
      leg2 <- reg2 prop
      reg1 \x -> do
        tr "stage → stage" x
        leg2 x

-- | `identity` forwards its input straight to its output: the echo wire, and
-- | the unit of `compose`. It is also the unit of both diagonal merges at
-- | their unit object and, entered from the empty variant, of `+→×` — the
-- | unit axis of each shape header.
instance Applicative m => Category (PUI m) where
  -- registration hands the continuation over, so the wire is the
  -- continuation itself: no state, nothing per instantiation
  identity = PUI $ pure pure

-- | **Pointedness: the terminal record's one answer.** An entity (a record
-- | channel) has a known initial state; an event (a variant channel) merely
-- | occurs. The **point** `announce a :: p {} a` is how this carrier
-- | supplies that initial state: one emission of `a` at registration, then
-- | nothing — the informationless `{}` it is fed is ignored. `with` closes
-- | the knot over it, and the seeded echo wire `debounced` primes its loop
-- | branch with is derived from it through `Choice` (`seeded`).
-- |
-- | What the point *is* follows from rows alone: `{}` has exactly one
-- | value, so every feed of a `{}`-input component is a repetition of the
-- | first, and the Repetition law of a record input makes answering it
-- | once and answering it every time the same behaviour —
-- |
-- |   * **answer** — `announce a ≈ lcmap (const a) identity`: the point is
-- |     the wire's answer to the terminal record's one value (up to
-- |     stutter, doc/observational-semantics.md §2). On a timeless carrier
-- |     that is an equality, `const`.
-- |   * **early** — a carrier with a registration moment gives the answer
-- |     at registration, before any feed arrives, and (by Repetition)
-- |     never again: feeding it `{}` changes nothing.
-- |
-- | That earliness is carrier structure, the operational ⊥ of the trace
-- | asymmetry theorem (doc §5): `debounced` composes a `seeded` wire into a
-- | positional loop no feed can reach, and primes it only because the
-- | answer comes before the first input. It was a class, `Seeding`, with
-- | `(->)` and `Cont` instances, until the class was dropped as a carrier
-- | fact dressed as algebra (CHANGELOG, 2026-10-14). The merge units are
-- | *not* pointed: the record gates ignore a contribution of zero fields —
-- | pointing is this word's business alone.
announce :: forall m a. Applicative m => a -> PUI m {} a
announce a = PUI $ pure \prop -> do
    -- an `if`, not `unless`: the branch must not even be built for a hole,
    -- since a downstream stage (a gated merge's `exactRow`) reads the value
    -- while constructing its effect (holey twins of a fold beside a status)
    if isHole a then pure unit else prop a
    pure mempty

-- | The **seeded echo wire**: `identity`'s pass-through plus the point,
-- | derived through `Choice` — the point sits on the `Left` branch, the
-- | wire on the `Right`, and both branches rejoin — so the pointed wire
-- | needs no second primitive. It is the initial-state supply a seeded
-- | value-level trace builds on: `debounced` primes its loop branch by
-- | composing one into the traced chain.
seeded :: forall m a. Applicative m => a -> PUI m a a
seeded a = dimap Right (either identity identity) (left (announce a))

-- | Discharge a chain's initial obligation by announcing its t=0 value.
-- | The seed is whatever the chain's first stage takes: a model row into
-- | the knot (`w # looped # with seed`, the app shape) or into a
-- | knotless flow (`# with invitation`, potluck), the seed's signature
-- | stating the row. Never `{}`: `body`
-- | feeds the terminal record once itself, so a load action standing
-- | before the knot runs on that feed with no seed written. Its own input
-- | is ignored, so it sits at any row; a seed that is a hole is never
-- | announced (`announce`, guardrails L18). A pipeline's residual input row
-- | *is* its initial-state obligation, discharged by `with` down to `{}`,
-- | the one self-pointed record, which `body` demands.
with :: forall a m b r. Applicative m => a -> PUI m a b -> PUI m { | r } b
with a w = lcmap (const {}) (announce a >>> w)

-- | **Self-reference as carrier structure** — the point's sibling: the
-- | point says *a beginning exists* (one registration moment), the knot
-- | says *feedback exists* (an emission can re-enter its own input). The
-- | two things a timeless carrier lacks: feeding a function its own output
-- | is `fix`, a fixpoint computation, not a wire.
-- |
-- | The `×`-diagonal **self-trace** at record rows: feed a UI component its
-- | own emissions, re-entrancy-guarded — leaf UI components echo on
-- | `toUser`, and the guard swallows the echoes the re-feed provokes (the
-- | idempotence law, operationally). A primitive because no ecosystem
-- | class reaches it: on this knowledge-gated carrier `Costrong`'s
-- | `unfirst` cannot self-feed (no `c` before the first emission, no
-- | emission before the first input — the gate deadlocks), so the
-- | self-feeding special case is carrier structure, not a derivation.
-- | Row-shaped in the word itself: the looped value is an entity (a model
-- | row), stated by the seed's signature, not on the knot (`w # looped #
-- | with freshCount`, `freshCount :: { counted :: Int }`). It is the app's
-- | knot, and the only knot: a loop through the four shapes is cut at its
-- | record junction, where the model row holds the state, and an event re-enters through its fold. (The variant side would need
-- | no primitive — `Cochoice`'s `unleft` holds raw here, the carrier being
-- | traced over `+` and pointed-traced over `×` — and a one-field `×`-knot
-- | would be seeded through `PointedCostrong`; neither has a word,
-- | `Data.Profunctor.Row`, *What is deliberately not here*.)
-- |
-- | Laws — the trace axioms restricted to the diagonal, stated up to the
-- | observational equivalence of doc/observational-semantics.md (boundary
-- | channels; inner feeds compared up to consecutive duplication — the
-- | quotient feed-idempotence licenses):
-- |
-- | ```
-- | looped identity         = identity                        (yanking)
-- | looped (dimap f f⁻¹ g)  = dimap f f⁻¹ (looped g)          (conjugation — dinaturality at an iso f)
-- | looped (looped g)       ≈ looped g                        (idempotence: the guard; nesting only duplicates feeds)
-- | ```
-- |
-- | Conjugation needs the inverse pair: `dimap f f` in both positions is
-- | false for every non-involutive iso (counterexample in test/Main.purs).
-- | The triple is necessary, not sufficient — `looped = identity`
-- | satisfies all three — so what the word *means* is a fourth,
-- | behavioral law (temporal, like the point's): **re-entry** — each
-- | emission of the wrapped UI component is fed back to it exactly once,
-- | before propagating; the echoes that re-feed provokes are swallowed.
-- | Two of its three facts are row consequences: the re-entry guard is the
-- | Repetition law of a record input made operational, and the knot does
-- | not deadlock where positional `unfirst` does because a record input
-- | is **shared** — the loop-back and the outside are two feeders of one
-- | inclusive input row. What is not a row consequence is the cycle
-- | itself: an emission becoming a feed. The knot hides nothing and takes
-- | no seed of its own: it exposes the whole row and is primed by its
-- | first feed (`with seed`) — a looped state is a model field, never a
-- | hidden channel. It was a class, `Looping`, with this one instance,
-- | until the class was dropped (CHANGELOG, 2026-10-14).
-- |
-- | Wrapped around a record merge it
-- | supplies the sibling cross-feed the gated merge deliberately omits —
-- | every operand sees every emission re-broadcast, and per-operand
-- | *retention* falls out of the merge gates (each gate holds its side's
-- | last contribution). For **whole-row editor stages** the re-broadcast
-- | is what keeps exactness honest: each editor's `focusField @l` lift
-- | re-attaches the background it retained at its last feed, and the loop
-- | re-feeds every stage on every emission, so no editor can emit a stale
-- | sibling for longer than the turn in flight — which is why editor
-- | ensembles live inside `looped`, and a loop-free
-- | flow wraps its editor window in `looped` (order-form).
looped :: forall r m. Functor m => PUI m { | r } { | r } -> PUI m { | r } { | r }
looped (PUI p) = PUI $ p <#> \reg prop -> do
    busyRef <- Ref.new false
    -- `Just pending` while the body is being wired: downstream registers
    -- first (`compose`), so an emission fired during registration — a
    -- `fold`'s seed, announced from the loop's last stage — has no input
    -- leg to re-enter yet. It is held and re-fed once the body is wired.
    wiringRef <- Ref.new (Just [])
    legRef <- Ref.new Nothing
    let
      refeed u = do
        busy <- Ref.read busyRef
        if busy
          then tr "looped: echo swallowed (re-entrant)" u
          else do
            tr "looped: re-feeding emission" u
            Ref.write true busyRef
            Ref.read legRef >>= traverse_ (_ $ u)
            Ref.write false busyRef
            prop u
    leg <- reg \u -> do
      wiring <- Ref.read wiringRef
      case wiring of
        Just pending -> do
          tr "looped: emission while wiring, held until the body is wired" u
          Ref.write (Just (pending <> [ u ])) wiringRef
        Nothing -> refeed u
    Ref.write (Just leg) legRef
    pending <- Ref.read wiringRef
    Ref.write Nothing wiringRef
    for_ (fromMaybe [] pending) refeed
    pure leg

-- The four row merges, one instance per shape, in the order of the grid in
-- Data.Profunctor.Row ("The laws"): ×→×, ×→+, +→+, +→×. Only the first
-- gates — the machinery that follows the block, shared with the container
-- action; the other three are routing (+→× the copairing).

instance Apply m => RecordToRecord (PUI m) where
  recordToRecord p1@(PUI q1) p2@(PUI q2) = PUI ado
    reg1 <- q1
    reg2 <- q2
    in \prop -> do
      gate <- newRecordGate "×→×" labels1 labels2
      -- broadcast in, gate out, one feed one step — the shape's projection
      -- and preservation laws; the carrier's part is `steppedFeed`, the
      -- batched broadcast
      Tuple leg1 leg2 <- gatedRecordOutputs gate exactRow exactRow reg1 reg2 prop
      pure \new -> steppedRecordFeed Shared gate do
        leg1 new
        leg2 new
    where
    labels1 = labelsOf p1
    labels2 = labelsOf p2

instance Applicative m => RecordToVariant (PUI m) where
  -- broadcast in, passage out — the shape's projection law. A variant
  -- output has nothing to gate, so the merge is a bare pass-through and
  -- the one-thing-per-feed obligation falls on the operands (the answer
  -- law at ×→+, arming); hence `Applicative m`: no state here.
  recordToVariant (PUI p1) (PUI p2) = PUI ado
    reg1 <- p1
    reg2 <- p2
    in \prop -> do
      leg1 <- reg1 prop
      leg2 <- reg2 prop
      pure \new -> leg1 new *> leg2 new

-- dispatch in, passage out — the shape's projection law: no
-- broadcast, so nothing to step; a variant out, so nothing to gate. Hence
-- `Applicative m`: the one merge with no state at all.
instance Applicative m => VariantToVariant (PUI m) where
  variantToVariant (PUI p1) (PUI p2) = PUI ado
    reg1 <- p1
    reg2 <- p2
    in \prop -> do
      leg1 <- reg1 prop
      leg2 <- reg2 prop
      pure \v -> do
        for_ (contract v :: Maybe _) leg1
        for_ (contract v :: Maybe _) leg2

instance Applicative m => VariantToRecord (PUI m) where
  -- the copairing: an occurrence is dispatched to the one operand owning
  -- its case, and whatever either operand releases is the whole row, so it
  -- is forwarded as it comes — no gate, nothing retained
  variantToRecord (PUI p1) (PUI p2) = PUI ado
    reg1 <- p1
    reg2 <- p2
    in \prop -> do
      leg1 <- reg1 prop
      leg2 <- reg2 prop
      pure \v -> do
        for_ (contract v :: Maybe _) leg1
        for_ (contract v :: Maybe _) leg2

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

-- | The registration of a record merge's operands at its gate. Each operand
-- | emission, trimmed to its declared row by `exact1`/`exact2` (the type
-- | says exactly what the machine may see), contributes its fields under
-- | their labels; a release is assembled back into the merged row, `Union
-- | o1 o2 o` witnessing that the assembled fields are exactly `o`'s.
gatedRecordOutputs
  :: forall i1 i2 e1 e2 r1 r2 r
   . RecordGate
  -> (e1 -> { | r1 })
  -> (e2 -> { | r2 })
  -> Registration i1 e1
  -> Registration i2 e2
  -> ({ | r } -> Effect Unit)
  -> Effect (Tuple (i1 -> Effect Unit) (i2 -> Effect Unit))
gatedRecordOutputs rg exact1 exact2 reg1 reg2 prop = do
  -- the release reads the downstream continuation from the gate, so it is
  -- written before the operands subscribe: a registration-time announcement
  -- releases into a listening channel
  Ref.write (Just (prop <<< assembled)) rg.gate.prop
  -- a side owning zero fields contributes nothing: its only possible
  -- emission is the informationless {}, which fills no participant, so it
  -- neither opens the gate nor re-fires it — `identity @{}`, a silent
  -- display and an announcing one are indistinguishable as operands
  leg1 <- reg1 \partial -> contributed rg (fieldsOf rg.labels1 (exact1 partial))
  leg2 <- reg2 \partial -> contributed rg (fieldsOf rg.labels2 (exact2 partial))
  pure $ Tuple leg1 leg2
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
instance Functor m => Resolving (PUI m) where
  resolve = resolveFor { ms: 300.0 }

-- | `resolve` with an explicit quiescence window — see the `Resolving`
-- | instance. `Done` needs no state and fires (after the window) even
-- | unprimed; only the `Loop` branch is gated on a first `c`.
resolveFor :: forall m a b c. Functor m => { ms :: Number } -> PUI m a b -> PUI m (Tuple a c) (Either b c)
resolveFor millis (PUI p) = PUI $ p <#> \reg prop -> do
  cRef <- Ref.new Nothing
  mFiberRef <- Ref.new Nothing
  guard <- gateGuard
  leg <- reg \b -> do
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
    Ref.read cRef >>= case _ of
      Nothing -> do
        guard.blocked "Resolving.resolve: loop-branch emissions dropped for 3s — no input has primed the retained state (only quiescence resolutions pass). Feed the stage a first value." []
        tr "Resolving.resolve: loop branch withheld (state unprimed)" b
      Just c -> prop $ Right c
  pure \(Tuple a c) -> do
    guard.fed
    Ref.write (Just c) cRef
    leg a

-- | The Mealy step: a fresh `Left a` feeds the inner UI component, a `Right c`
-- | (re)places the retained state. When the inner UI component emits `b`, the
-- | output pairs it with the retained `c` — and is **withheld until a `c`
-- | has arrived** (a `Tuple b c` with unknown `c` would be a fabrication),
-- | mirroring the knowledge-gated record merges.
instance Functor m => Retaining (PUI m) where
  retain (PUI p) = PUI $ p <#> \reg prop -> do
    cRef <- Ref.new Nothing
    guard <- gateGuard
    leg <- reg \b -> Ref.read cRef >>= case _ of
      Nothing -> do
        guard.blocked "Retaining.retain: emissions dropped for 3s — the retained state was never fed (no state-case input arrived), so the gate cannot complete a Tuple. Prime the state channel: seed the state case (`seeded`); an application loop closes at its record junction instead (`looped`)." []
        tr "Retaining.retain: emission withheld (state unprimed)" b
      Just c -> prop $ Tuple b c
    pure case _ of
      Left a -> leg a
      Right c -> do
        guard.fed
        Ref.write (Just c) cRef


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
-- emitter *is* silent — holds for emitters that originate occurrences
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

-- | Whether a value is a **hole** — logic not written yet (guardrails L18):
-- | `PUI.Web.hole` answers `true` to this one field and throws on every
-- | other touch, and no other value has it. A word that consumes a logic
-- | value when built rather than on data — a seed it announces, a period
-- | it schedules — skips a hole, so a view runs before its logic exists.
-- | Pure: a field read. A development diagnostic like `tr` and
-- | `gateGuard`, and so the carrier's rather than the algebra's — it
-- | names a field the carrier's JavaScript invents, which no module under
-- | `Data.Profunctor` may know of (moved from `Data.Profunctor.Seeding`,
-- | CHANGELOG 2026-10-14).
isHole :: forall a. a -> Boolean
isHole a = (unsafeCoerce a :: { "__bambikHole" :: Boolean })."__bambikHole" == true

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

-- | The **tick source**: the `×→+` leaf owning case `l`
-- | (`ticks @"Clock ticked" tickPeriod`, timer) — every `interval`, the
-- | row it was last fed leaves as case `l`; replay is `Strong`'s
-- | retention, the fed row riding the state channel as under `clicked`.
-- | Feeds are retained, never answered; nothing ticks before the first feed
-- | or while the period is a hole (guardrails L18). A beat in a suspended
-- | scope (a detached pane) is skipped, and a cancelled scope ends the
-- | loop. A tick that needs telling is told by the status that folds it
-- | in the fold block, so the source opens with none.
ticks
  :: forall @l m r b v
   . Scoped m
  => IsSymbol l
  => Cons l { | r } b v
  => { ms :: Number }
  -> PUI m { | r } [ | v ]
ticks interval = fromHandles do
  sc <- scope
  lastRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \r -> Ref.write (Just r) lastRef
    , fromUser: \prop -> if isHole interval then pure unit else do
        let
          loop = do
            delay (Milliseconds interval.ms)
            cancelled <- liftEffect $ scopeCancelled sc
            unless cancelled do
              liftEffect do
                active <- scopeActive sc
                fed <- Ref.read lastRef
                when active $ for_ fed \r -> prop (inj (Proxy @l) r)
              loop
        launchAff_ loop
    }


-- | A **source**: a fact the platform knows and the app cannot seed — where
-- | the device is, whether it is online, what time it is, which colour
-- | scheme is preferred. A source owns one field, `l`, reads nothing of the
-- | row it is fed, and is **pointed**: it announces its first value at
-- | registration (`now`), so a merge of sources is a row the gate releases
-- | at once and keeps current. Every feed is answered with the last value
-- | known (Answer at `×→×`), and each change the platform reports is an
-- | emission outside any feed — the editor whose user is the platform. A
-- | fact the platform may never supply is a variant with its named cases
-- | (`geolocated`'s `located`/`unknown`/`denied`), never a `Maybe`, so the
-- | gate never starves on it. The watch is quiet while the scope is
-- | suspended (a detached pane) and ends when it is cancelled. Several
-- | sources are merged, `RecordToRecord.do`, each owning its field
-- | (here-and-now, incident-report) — the one application use of the
-- | `×→×` merge, since every editor owns the whole row.
source
  :: forall @l m a r r1
   . Scoped m
  => IsSymbol l
  => Cons l a () r1
  => Effect a
  -> ((a -> Effect Unit) -> Effect (Effect Unit))
  -> PUI m { | r } { | r1 }
source now watch = fromHandles do
  sc <- scope
  first <- liftEffect now
  lastRef <- liftEffect $ Ref.new first
  propRef <- liftEffect $ Ref.new Nothing
  let row a = Record.insert (Proxy @l) a {}
  pure
    { toUser: \_ -> do
        a <- Ref.read lastRef
        Ref.read propRef >>= traverse_ (_ $ row a)
    , fromUser: \prop -> do
        Ref.write (Just prop) propRef
        Ref.read lastRef >>= prop <<< row
        stop <- watch \a -> do
          Ref.write a lastRef
          active <- scopeActive sc
          when active $ prop (row a)
        onCancel sc stop
    }

-- | `joined` at the empty payload: the click source's occurrence carries
-- | nothing, so the event is dropped, `f` is read off the row last fed and
-- | the case is renamed from the source's to `l`. Replay is `Strong`'s
-- | retention and nothing else — the primed `Strong` law: an occurrence
-- | before any feed is withheld, a feed never emits. With `occurrencesOn`
-- | this is how a vocabulary builds a pick that carries a field of what
-- | it showed (`listOf @l @k`'s row, MDC3's `tile @l @k`): `replayed @l
-- | (Record.get (Proxy @k)) (occurrencesOn Click content)`. Plumbing, not
-- | vocabulary — an application's emitters are its vocabulary's.
replayed :: forall @l m r k b v. Functor m => IsSymbol l => Cons l k b v => ({ | r } -> k) -> PUI m { | r } [ occurred :: {} ] -> PUI m { | r } [ | v ]
replayed f src = Adopters.joined @"occurred" src # rmap (on (Proxy @"occurred") (inj (Proxy @l) <<< f <<< _.model) case_)

-- | What a host tells about the user's hands on what it built, for the
-- | occurrence sources that read nothing else: a **gesture** on the element
-- | built last (`occurrencesOn`), and the **focus** of the container being
-- | built (`departures`). Each method is read at build time and hands back
-- | the registration, so the place it speaks of is fixed where the source
-- | is written. `Web` answers on the DOM; the `Effect` probe has no user
-- | and never fires.
class MonadEffect m <= Interacting m where
  -- | Register an action run on each `gesture` on the element built last.
  gestured :: Gesture -> m (Effect Unit -> Effect Unit)
  -- | The container being built: move the focus inside it (unless it is
  -- | there already), and register an action run each time it leaves.
  focused :: m { focusInside :: Effect Unit, onDeparture :: Effect Unit -> Effect Unit }

-- | The gestures an occurrence source listens for.
data Gesture = Click | DoubleClick

instance Interacting Effect where
  gestured _ = pure \_ -> pure unit
  focused = pure { focusInside: pure unit, onDeparture: \_ -> pure unit }

-- | The occurrence source every emitter is built from: each `gesture` on
-- | the last-built element (the content's own node) — a `Click` for every
-- | emitter, a `DoubleClick` for MDC3's `doubleClickTile` — leaves as an
-- | occurrence carrying nothing; the content is fed the row and its output
-- | written off. The `× → +` leaf's occurrence half — at the closed empty
-- | row, `occurrencesOn Click chrome :: PUI m {} [ occurred :: {} ]` is
-- | the point's dual, an occurrence out of the terminal record (`ticks` is
-- | its timer sibling). One fixed case: the business label is the
-- | vocabulary leaf's to state, and `replayed @l` relabels while it
-- | attaches the row — replay is `Strong`'s retention, so no source keeps
-- | a copy of the row it was shown. Plumbing, like `replayed`.
occurrencesOn :: forall m a b. Interacting m => Gesture -> PUI m a b -> PUI m a [ occurred :: {} ]
occurrencesOn gesture w = fromHandles do
  w' <- handles w
  listen <- gestured gesture
  pure
    { toUser: w'.toUser
    , fromUser: \prop -> do
        -- content is display-only: give its wiring a sink so echoes flow
        w'.fromUser \_ -> pure unit
        listen $ prop (inj (Proxy @"occurred") {})
    }

-- | The **departure** source of an in-place editor's container: an
-- | occurrence each time the focus leaves the container — a click
-- | elsewhere, Tab, or Enter, which leaves the element it is pressed in —
-- | and, on every feed, the focus moved to the container's first element
-- | unless it is already inside, so an editor shown by a pane is ready to
-- | type in. Nothing leaves inside a feed. Placed after the editor in its
-- | container, it is the commit of what the editor released:
-- | `span $ input @k "text" >>> replayed @l identity departures`
-- | (`PUI.Web.HTML.inPlaceInput`). Plumbing.
departures :: forall m a. Interacting m => PUI m a [ occurred :: {} ]
departures = fromHandles do
  container <- focused
  pure
    { toUser: \_ -> container.focusInside
    , fromUser: \prop -> container.onDeparture (prop (inj (Proxy @"occurred") {}))
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
-- | `dimap`-only adopters (`atField`/`atCase`, `toField`/`toCase`, the
-- | floor's) and the focus `focusField`.
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

-- | An action **owns its button's case**: the `+→+` leaf of case `l`
-- | (`action @"Charge card" chargeFlaky`, payment), fed the row the button
-- | replayed, running the `Aff` on it, its outcome leaving as a case of
-- | the row `t` the `Aff`'s signature names. Like a status it narrows its
-- | input to its one case (`Cons l a () v`); its output is **exactly** its
-- | outcomes, like `load`'s, so it widens nothing and the view writes no
-- | row for it (guardrails L18): what tells the outcomes closes `t` —
-- | the statuses or capture after it, which own their cases
-- | (payment, weather), or, standing among emitters in a
-- | `RecordToVariant.do`, the `joined @l` (one outcome) or `joined2 @l1
-- | @l2` (two) it is joined by, which close its source to the labels
-- | they name and widen it into the block's row (order-form, ticker).
-- | With its logic deleted the hole is typed by those followers, so the
-- | view still compiles and the hint carries the outcome labels.
-- |
-- | It is **opened by its progress indicator**: the slot is a `+→×`
-- | citizen of the run's two lifecycle occurrences, `started` and
-- | `ended` (`indeterminateCircularProgress # action @"Charge card" …`), and of
-- | nothing else — a status that tells an outcome is never the slot, and
-- | the types forbid it. The outcome is folded after the action by the
-- | statuses that tell it (`snackbar @"Card charged" identity`, payment),
-- | or by `capture @l` where nothing needs telling (weather); an outcome
-- | whose fold keeps the model is `joined` with its button, so the fold
-- | has the model in hand (order-form's submit). No
-- | application owns a "busy": the indicator shows between `started` and
-- | `ended` and, like every status, owes the channel nothing. The action
-- | a mount runs, fed `body`'s one `{}` rather than a case, is `load`.
-- |
-- | A failing action is **reported, not swallowed**: `ended` is dispatched
-- | whichever way the `Aff` ends — so a throw cannot strand the spinner —
-- | and the error reaches the diagnostics sink by name. Nothing is posted
-- | onward, since there is no output to post. An action whose function is a
-- | hole never runs (guardrails L18): a view's own literal seed may reach
-- | it before its logic exists. An outcome that settles in a suspended or
-- | cancelled scope — the pane that held the action detached meanwhile — is
-- | dropped: the case the model left gets no occurrence from it, so a
-- | detached action is silent like a detached button; the slot is still
-- | told `ended`, so a re-attached indicator is not left running.
action
  :: forall @l t a v v1 m
   . IsSymbol l
  => Cons l a () v
  => Scoped m
  => Contractable ( started :: {}, ended :: {} ) v1
  => (a -> Aff [ | t ])
  -> PUI m [ | v1 ] {}
  -> PUI m [ | v ] [ | t ]
action arr slot = atCase @l (running (\i -> if isHole arr then Nothing else Just (arr i)) slot)

-- | The action a **feed** runs — `×→+`, the row fed is the `Aff`'s
-- | argument and its outcome leaves as the case the `Aff` names: the
-- | **load** standing before the knot, run by `body`'s one feed of `{}`
-- | (`indeterminateLinearProgress # load loadOrder; snackbar @"Order
-- | loaded" identity`, order-form), its outcome the model the knot
-- | loops. Its outcome row `t` is the `Aff`'s signature: unlike `action`
-- | it widens nothing, so the case the capture or status after it takes
-- | needs no line of its own (guardrails L18). Opened by its progress indicator like `action`,
-- | reported and scoped the same way; typed at exactly its outcomes.
load
  :: forall t r v1 m
   . Scoped m
  => Contractable ( started :: {}, ended :: {} ) v1
  => ({ | r } -> Aff [ | t ])
  -> PUI m [ | v1 ] {}
  -> PUI m { | r } [ | t ]
load arr = running \i -> if isHole arr then Nothing else Just (arr i)

-- The run behind `action`: `Nothing` is a hole, which
-- never runs (guardrails L18); otherwise `started` and `ended` are told to
-- the slot around the `Aff`, and its outcome is posted onward.
running
  :: forall t a v1 m
   . Scoped m
  => Contractable ( started :: {}, ended :: {} ) v1
  => (a -> Maybe (Aff [ | t ]))
  -> PUI m [ | v1 ] {}
  -> PUI m a [ | t ]
running run w = action'
  (\i pro post active -> case run i of
    Nothing -> pure unit
    Just aff -> do
      let tell v = for_ (contract (v :: [ started :: {}, ended :: {} ]) :: Maybe _) pro
      liftEffect $ tell (inj (Proxy @"started") {})
      result <- attempt aff
      liftEffect $ tell (inj (Proxy @"ended") {})
      case result of
        Left err -> liftEffect $ warn $ "action: the Aff failed and nothing was emitted — " <> message err
        Right o -> liftEffect do
          on <- active
          if on
            then post o
            else tr "action: outcome dropped, the scope is suspended or cancelled" o)
  w

-- | One case **folded into the record** (`+→×`): the faceless leaf owning
-- | case `l`, releasing `f` of its payload as the whole next model —
-- | `fold @"Count" increment` (counter), `fold @"Todo toggled" toggleTodo`
-- | (todo-list). A loop's folds are one per event case, merged by
-- | `VariantToRecord.do`, each releasing the whole next model. Memoryless:
-- | a replaying emitter's payload is the row it was fed, an event with
-- | something of its own arrives `joined` with that row as
-- | `{ event, model }` (`kept` is the fold that keeps it), and an effect
-- | returns the model (`capture @l`), so every handler has the model in
-- | hand and the loop's memory stays at the emitters and in the knot. A
-- | fold whose event needs telling is a **status**, which takes its fold
-- | the same way (`snackbar @"Card charged" identity`) and tells its
-- | case, the label its whole copy; this is that fold with no face,
-- | `folding @l f` over `unshown @l`.
-- |
-- | A status is **named as what it tells**: its case label is the words on
-- | the screen, so it stands after an action whose outcome row names its
-- | outcomes (`"Card charged"`, `"Order placed"`), and a command straight
-- | from an emitter — whose case is the button's caption — folds here,
-- | silently, its result already on screen. Whatever the user must keep
-- | (a reference, an order number) is a model field shown by a display,
-- | never copy flashed by a status; an outcome's reasons are its cases,
-- | each told by a sibling status.
fold
  :: forall @l m a v r
   . Applicative m
  => IsSymbol l
  => Cons l a () v
  => (a -> { | r })
  -> PUI m [ | v ] { | r }
fold f = folding @l f (unshown @l)

-- | One case folded into the record by **resetting** it to `x`
-- | (`reset @"New game" openingPosition`, tic-tac-toe): the row released is
-- | `x` whole, whatever the payload. `reset @l x = fold @l (const x)`; a
-- | status resets the same way, `snackbar @"Order placed" (const
-- | usualOrder)` (espresso-bar).
reset
  :: forall @l m a v r
   . Applicative m
  => IsSymbol l
  => Cons l a () v
  => { | r }
  -> PUI m [ | v ] { | r }
reset x = fold @l (const x)

-- | One case folded into the record by **capturing** its payload as the
-- | model whole (`capture @"Forecast fetched"`, weather): the case an effect
-- | returns the model under — a load, a fetch, a server's answer — where
-- | nothing needs telling. `capture @l = fold @l identity`; a status
-- | captures with `identity` (`snackbar @"Order loaded" identity`,
-- | order-form).
capture
  :: forall @l m v r
   . Applicative m
  => IsSymbol l
  => Cons l { | r } () v
  => PUI m [ | v ] { | r }
capture = fold @l identity

-- | The fold behind every `+→×` leaf — plumbing, not vocabulary: a status
-- | face owning case `l` and the fold of its payload, in one leaf. The
-- | vocabularies build their statuses with it (`snackbar @l f =
-- | snackbarFace (reflectSymbol (Proxy @l)) # atCase @l # folding @l f`),
-- | and `fold @l f` is it over the faceless `unshown @l`. The status is fed
-- | the **event**, never the fold's result — it shows its case's label, so
-- | what a status says is the name of what happened — and `f` of the
-- | payload is the row released.
-- |
-- | **Derived**: the wire and the status under the `×→×` merge, fed the
-- | occurrence's outcome and the occurrence in one two-field record — the
-- | status contributes no field and is inert to the gate, the wire answers
-- | every feed with the row, released once per feed. (A carrier-free
-- | `fold @l f` once lived in `Data.Profunctor.Row.VariantToRecord`, the
-- | statuses standing beside the folds as sibling operands; CHANGELOG,
-- | 2026-10-07.)
folding
  :: forall @l m a v r
   . Applicative m
  => IsSymbol l
  => Cons l a () v
  => (a -> { | r })
  -> PUI m [ | v ] {}
  -> PUI m [ | v ] { | r }
folding f status = dimap (\v -> { event: v, model: on (Proxy @l) f case_ v }) _.model $ recordToRecord
  (dimap _.model { model: _ } identity :: PUI m { event :: [ | v ], model :: { | r } } { model :: { | r } })
  (lcmap _.event status :: PUI m { event :: [ | v ], model :: { | r } } {})

action' :: forall a b s t m. Scoped m => (s -> (a -> Effect Unit) -> (t -> Effect Unit) -> Effect Boolean -> Aff Unit) -> Optic (PUI m) s t a b
action' arr w = fromHandles do
  w' <- handles w
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
debounced :: forall m. Applicative m => { ms :: Number } -> Ocular (PUI m)
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
  -- | Build a pane's content parked, in a scope of its own that starts
  -- | suspended, and hand back how to show and park it: `ensureAttached`
  -- | places it and resumes its scope, `ensureDetached` takes it away and
  -- | suspends the scope. Both are idempotent. The panes (`inCase`,
  -- | `provided`) are this and nothing else of the host.
  attachable :: forall r. m r -> m { result :: r, ensureAttached :: Effect Unit, ensureDetached :: Effect Unit }

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
        inst <- inScope child (handles w)
        pure { feed: inst.toUser, subscribe: inst.fromUser, node: unit, cancel: cancelScope child }
    , detach: \_ -> pure unit
    , restack: \_ -> pure unit
    }
  attachable build = do
    outer <- scope
    child <- newScope (Just outer) Suspended
    result <- inScope child build
    pure { result, ensureAttached: resumeScope child, ensureDetached: suspendScope child }

-- | Keyed, retaining collection on any hosting carrier (laws in
-- | `Data.Profunctor.Acting`'s header; the vocabulary form is `acted @l`).
instance Hosting m node => Acting (PUI m) where
  actedBy key w = fromHandles do
    hooks <- hosting w
    liftEffect $ actedWith key hooks

-- | The **pane**: a whole-row citizen — a display, an editor, or a pipeline
-- | of them — that *exists* only while the classifier yields case `l`:
-- | attached and fed the whole row on that case, detached on any other,
-- | the fed row released always. Where its content is a display, it shows
-- | the row only in that state (`text ticketLine # inCaseAt @"serving"
-- | @"display"`); where
-- | this pane's content emits the **row**, so the pane is a stage — the
-- | pane's channel and the wire's, side by side over one input and one
-- | output.
-- |
-- | It dissolves the identity fold: a field that exists only in one mode
-- | is *not* a payload to fold back into the row by hand — it is a
-- | whole-row editor whose existence is gated, and
-- | its `focusField @l` lift already re-attaches the rest of the row. The
-- | classifier is a business function typed at the stage's row, exactly as
-- | `pane`'s is; a variant the model stores is named by its label
-- | instead, `inCaseAt` (`# inCaseAt @"Delivery" @"selected"`, order-form). One release per feed either way: attached, the
-- | editor's own echo is the release; detached, the wire speaks for the
-- | absent editor. What the edit does to the rest of the row is a `settled`
-- | normalization on the same stage when it is a state invariant
-- | (meeting-booker's `seatsInRoom`, circle-drawer's `resizeSelected`).
-- | Like the other panes, it names only its case: the states it chooses
-- | among are the classifier's signature (guardrails L18).
inCase
  :: forall @l m node v r a b
   . Hosting m node => IsSymbol l => Cons l a b v
  => ({ | r } -> [ | v ]) -> PUI m { | r } { | r } -> PUI m { | r } { | r }
inCase f w = fromHandles do
  { result: editor, ensureAttached, ensureDetached } <- attachable $ handles w
  propRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \row -> do
        case prj (Proxy @l) (f row) of
          Nothing -> do
            ensureDetached
            -- the wire speaks only for the absent editor: attached, the
            -- editor's own echo is the release (one feed, one release)
            mProp <- Ref.read propRef
            for_ mProp \prop -> prop row
          -- attach before feeding, as `provided` does
          Just _ -> ensureAttached *> editor.toUser row
    , fromUser: \prop -> do
        Ref.write (Just prop) propRef
        editor.fromUser prop
    }

-- | `inCase` over a variant the model stores: the editor exists in case `c`
-- | of field `l`, both named by label (`# inCaseAt @"return flight" @"Flight type"`).
inCaseAt
  :: forall @c @l m node r a v b1 b2
   . Hosting m node => IsSymbol c => IsSymbol l => Cons c a b1 v => Cons l [ | v ] b2 r
  => PUI m { | r } { | r } -> PUI m { | r } { | r }
inCaseAt = inCase @c (Record.get (Proxy @l))

-- | Show the **emitter pane** while the model is in state `l` — visibility
-- | is **case adoption**: the argument is a business function classifying
-- | the situation into a variant, and the pane is attached and fed the row
-- | on case `l`, detached on every other case. A quiz whose run is either `asking` or `finished` shows its
-- | answer list as `listOf … # provided @"asking" quizPhase`; a stopwatch
-- | shows Start only while `halted`.
-- |
-- | The content is an emitter (`×→+`), and that is what makes the pane
-- | lawful: a feed arms an emitter and fires nothing, so a detached one
-- | firing nothing is the same answer — an emitter removed by `provided`
-- | *is* silent. For a button that is the document's doing (a parked
-- | node gets no clicks); for a `ticks` or an `action` inside, whose
-- | occurrences have no cause on the channel, it is the pane's **scope**:
-- | the content is built in a scope the pane suspends while detached, so a
-- | tick skips its beat and an outcome that settles meanwhile is dropped
-- | (`PUI`, "Scope"). The pane family is indexed by the shape of what the
-- | pane holds, each member lawful at its shape: `provided` for emitters,
-- | `inCase` for displays and editors (the wire while detached).
-- |
-- | A `Maybe`-gated pane is the same
-- | thing with its two cases unnamed, so there is no `Maybe` form: a state a
-- | pane depends on is a variant with **named** cases — `estimated`/`unknown`
-- | for a distance, `reading`/`browsing` for an inbox, `chosen`/`unchosen`
-- | for a selection — which is what makes the view line say which state it
-- | renders. Where several states are **mutually exclusive**, one classifier
-- | states it (`# provided @"taken" usernameStatus`) and each pane
-- | adopts its own case, so two panes can never both be on screen — which
-- | separate "should this be visible?" tests can always accidentally allow.
-- |
-- | The pane is fed the whole row, like every stage, and it is removed
-- | from the page while the model sits elsewhere: the rule for *what is on
-- | screen when* lives in business code where it can be tested, and a pane
-- | that is absent contributes nothing — so anything downstream waiting on
-- | it waits, rather than showing a stale or invented value.
-- |
-- | The pane names only its case (`# provided @"reading" messageView`):
-- | every state it chooses among is the classifier's signature, in the view
-- | model module, and the view restates none of them (guardrails L18).
-- | A variant the model stores is named by its label instead, `providedAt`.
provided :: forall @l m node v r a b v1. Hosting m node => IsSymbol l => Cons l a b v => ({ | r } -> [ | v ]) -> PUI m { | r } [ | v1 ] -> PUI m { | r } [ | v1 ]
provided = attachedOn @l

-- | `provided` over a variant the model stores: the emitter exists in case
-- | `c` of field `l`, both named by label (`# providedAt @"halted" @"phase"`).
providedAt :: forall @c @l m node r a v v1 b1 b2. Hosting m node => IsSymbol c => IsSymbol l => Cons c a b1 v => Cons l [ | v ] b2 r => PUI m { | r } [ | v1 ] -> PUI m { | r } [ | v1 ]
providedAt = provided @c (Record.get (Proxy @l))

-- The pane mechanism every pane shares, at any content output: attach and
-- feed the row on case `l`, detach on every other case. Public only
-- through its lawful restrictions — `provided` at emitter content, where a
-- detached pane's silence is `×→+`'s Answer (`inCase` answers for a
-- detached content itself).
attachedOn :: forall @l @v m node r a b c. Hosting m node => IsSymbol l => Cons l a b v => ({ | r } -> [ | v ]) -> PUI m { | r } c -> PUI m { | r } c
attachedOn f w = fromHandles do
  {result: { toUser, fromUser}, ensureAttached, ensureDetached} <- attachable $ handles w
  pure
    { toUser: \fed -> case prj (Proxy @l) (f fed) of
      Nothing -> ensureDetached
      Just _ -> do
        -- attach before feeding: a UI component that measures itself on toUser (the
        -- MDC slider positions its thumb from the track width) needs to be in
        -- the document first, or it lays out against a zero-width detached node
        ensureAttached
        toUser fed
    , fromUser
    }

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
foreach :: forall @l r m node k b a. Hosting m node => IsSymbol l => Cons l k b r => (a -> Array { | r }) -> PUI m { | r } { | r } -> PUI m a a
foreach f w = withStructuralOrd @k $ fromHandles do
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
anyOf :: forall @l r m node k b a v. Hosting m node => IsSymbol l => Cons l k b r => (a -> Array { | r }) -> PUI m { | r } [ | v ] -> PUI m a [ | v ]
anyOf f w = withStructuralOrd @k (lcmap (\r -> if isHole f then [] else let xs = f r in if isHole xs then [] else xs) $ fromHandles do
  hooks <- hosting w
  liftEffect $ collapsedWith (Record.get (Proxy @l)) hooks)

-- | `anyOf` over an array the model stores: keyed by field `l` of each
-- | element, the array field `t` of the fed row, both named by label
-- | (`# anyOfAt @"key" @"cells"`, cells). An array derived from the row is
-- | a projection, `anyOf`.
anyOfAt :: forall @l @t m node k b1 b2 r1 r v. Hosting m node => IsSymbol l => IsSymbol t => Cons l k b1 r1 => Cons t (Array { | r1 }) b2 r => PUI m { | r1 } [ | v ] -> PUI m { | r } [ | v ]
anyOfAt = anyOf @l (Record.get (Proxy @t))

-- | The **editing collection** — `anyOf` over editors: the same keyed and
-- | retaining reconciliation over elements that are `×→×` citizens (a row
-- | holding a toggle, a field), a user's edit of an element leaving as
-- | case `l` carrying the element's row, while an element's answer to the
-- | list's own feed — every editor echoes what it is shown — is no event
-- | and is dropped inside that feed. So the list is `×→+` by behaviour as
-- | by shape (Answer: a feed is answered by no emission) without leaning
-- | on the knot's re-entrancy guard, and the element stays the
-- | checkbox-like leaf it is: `( listItem $ Semigroupoid.do { …; iconToggle
-- | @"Favorite" … } ) # anyEdit @"Favorite changed" @"title"
-- | visibleMovies # joined @"Favorite changed"` (movie-browser), the fold
-- | writing the edit back into the stored collection by key. The
-- | projection's array is a view, so nothing gathers — a stored array's
-- | editors are `acted`.
anyEdit
  :: forall @l @k r m node key b1 b2 a v
   . Hosting m node
  => IsSymbol l
  => IsSymbol k
  => Cons k key b1 r
  => Cons l { | r } b2 v
  => (a -> Array { | r })
  -> PUI m { | r } { | r }
  -> PUI m a [ | v ]
anyEdit f editor = anyOf @k f $ fromHandles do
  w' <- handles editor
  feeding <- liftEffect $ Ref.new false
  pure
    { toUser: \i -> do
        Ref.write true feeding
        w'.toUser i
        Ref.write false feeding
    , fromUser: \prop -> w'.fromUser \o -> do
        answering <- Ref.read feeding
        if answering
          then tr "anyEdit: an element's answer to the feed is no edit" o
          else prop (inj (Proxy @l) o)
    }

-- | The **keyed dispatch** — the +→+ member: one runtime case at a time.
-- | A fed `{ key, value }` reaches exactly the instance whose key matches —
-- | instantiated on first appearance (an unknown key is a new case, not an
-- | error) — and that instance's emissions leave tagged with its key. The
-- | targeted-update direction: no whole-array re-feed, O(1) per case, the
-- | shape for streams/pushes that arrive one entity at a time. No key
-- | function: the runtime variant input carries its tag, as a variant case
-- | carries its label.
dispatched :: forall k a m node i b. Hosting m node => (i -> { key :: k, value :: a }) -> PUI m a b -> PUI m i { key :: k, value :: b }
dispatched f w = withStructuralOrd @k (fromHandles do
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
accumulated :: forall k a m node i. Hosting m node => (i -> { key :: k, value :: a }) -> PUI m a a -> PUI m i (Array a)
accumulated f w = withStructuralOrd @k (fromHandles do
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
