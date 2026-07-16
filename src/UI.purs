module UI
  ( Action
  , PropagationError
  , PropagationStatus
  , UI(..)
  , action
  , action'
  , affAdapter
  , announce
  , constant
  , debounced
  , debounced'
  , effAdapter
  , every
  , looped
  , resolveFor
  , seeded
  , silence
  , spied
  , updates
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Lens (Optic)
import Data.Lens.Extra.Types (Ocular)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Cochoice (class Cochoice)
import Data.Profunctor.Costrong (class Costrong)
import Data.Profunctor.Row.RecordToRecord (class RecordToRecord)
import Data.Profunctor.Row.RecordToVariant (class RecordToVariant, class Resolving, class Coresolving)
import Data.Profunctor.Row (exactRow, widenRecordInput, widenVariantOutput)
import Data.Profunctor.Row.VariantToRecord (class VariantToRecord, class Retaining, class Coretaining)
import Data.Profunctor.Row.VariantToVariant (class VariantToVariant)
import Data.Profunctor.Strong (class Strong)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Variant (contract)
import Debug (class DebugWarning, spy)
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (Aff, delay, error, forkAff, killFiber, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Record (union) as Record

-- could it be: newtype UI m i o = UI ((o -> Effect Unit) -> m (i -> Effect Unit))
newtype UI m i o = UI (m
  { toUser :: i -> Effect Unit
  , fromUser :: (o -> Effect PropagationStatus) -> Effect Unit
  })

-- TODO: rename ValidationStatus/UserInputStatus?
type PropagationStatus = Maybe PropagationError

type PropagationError = String

derive instance Newtype (UI m i o) _

instance Functor m => Profunctor (UI m) where
  dimap pre post p = wrap ado
    p' <- unwrap p
    in
      { toUser: pre >>> p'.toUser
      , fromUser: \prop -> p'.fromUser (prop <<< post)
      }

-- Stateful instances below share one gating principle (the same one the
-- record merges follow): state a widget hasn't received yet cannot be
-- fabricated, so emissions needing it are withheld (`pure Nothing`) until
-- the state channel has been fed.
instance Functor m => Strong (UI m) where
  first p = wrap ado
    let lastab = unsafePerformEffect $ Ref.new Nothing
    p' <- unwrap p
    in
      { toUser: \ab -> do
          Ref.write (Just ab) lastab
          p'.toUser $ fst ab
      , fromUser: \prop ->
          p'.fromUser \b -> do
            mab <- Ref.read lastab
            case mab of
              Nothing -> pure Nothing
              Just prevab -> prop (Tuple b (snd prevab))
      }
  second p = wrap ado
    let lastab = unsafePerformEffect $ Ref.new Nothing
    p' <- unwrap p
    in
      { toUser: \ab -> do
          Ref.write (Just ab) lastab
          p'.toUser $ snd ab
      , fromUser: \prop ->
          p'.fromUser \b -> do
            mab <- Ref.read lastab
            case mab of
              Nothing -> pure Nothing
              Just prevab -> prop (Tuple (fst prevab) b)
      }

instance Functor m => Choice (UI m) where
  left p = wrap ado
    let mPropRef = unsafePerformEffect $ Ref.new Nothing
    p' <- unwrap p
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
    let mPropRef = unsafePerformEffect $ Ref.new Nothing
    p' <- unwrap p
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
instance Functor m => Costrong (UI m) where
  unfirst p = wrap ado
    let cRef = unsafePerformEffect $ Ref.new Nothing
    p' <- unwrap p
    in
      { toUser: \a -> do
          mc <- Ref.read cRef
          for_ mc \c -> p'.toUser $ Tuple a c
      , fromUser: \prop ->
          p'.fromUser \(Tuple b c) -> do
            Ref.write (Just c) cRef
            prop b
      }
  unsecond p = wrap ado
    let aRef = unsafePerformEffect $ Ref.new Nothing
    p' <- unwrap p
    in
      { toUser: \b -> do
          ma <- Ref.read aRef
          for_ ma \a -> p'.toUser $ Tuple a b
      , fromUser: \prop ->
          p'.fromUser \(Tuple a c) -> do
            Ref.write (Just a) aRef
            prop c
      }

-- | The `+`-diagonal **trace** (dual of `Choice`): a looped-branch emission
-- | re-enters the widget as input — feedback of **control**, i.e. iteration —
-- | until an exit-branch emission passes through. The re-entry is a `toUser`,
-- | so in `UI` the loop is an *event* loop: it advances on the widget's next
-- | emission (variant-output widgets do not echo, so the leaf protocol cannot
-- | provoke a synchronous spin). Retraction law: `unleft (left g) ≅ g`.
instance Functor m => Cochoice (UI m) where
  unleft p = wrap $ unwrap p <#> \p' ->
    { toUser: \a -> p'.toUser $ Left a
    , fromUser: \prop -> p'.fromUser case _ of
        Left b -> prop b
        Right c -> do
          p'.toUser $ Right c
          pure Nothing
    }
  unright p = wrap $ unwrap p <#> \p' ->
    { toUser: \a -> p'.toUser $ Right a
    , fromUser: \prop -> p'.fromUser case _ of
        Right b -> prop b
        Left c -> do
          p'.toUser $ Left c
          pure Nothing
    }

-- | The `× → +` **co-strength** (retraction of `Resolving`): a `Right c`
-- | emission is retained as the fold state and — **eagerly** — re-fed to
-- | the widget joined with the last input (guarded), so the widget
-- | re-renders at every fold step; a `Left b` exits. Gated like `Costrong`
-- | (a first `c` must arrive before inputs pass — `announce` an initial
-- | state to prime it); `coresolve (resolve g) ≅ g` once primed.
instance Functor m => Coresolving (UI m) where
  coresolve p = wrap ado
    let aRef = unsafePerformEffect $ Ref.new Nothing
    let cRef = unsafePerformEffect $ Ref.new Nothing
    let busyRef = unsafePerformEffect $ Ref.new false
    p' <- unwrap p
    in
      { toUser: \a -> do
          Ref.write (Just a) aRef
          mc <- Ref.read cRef
          for_ mc \c -> p'.toUser $ Tuple a c
      , fromUser: \prop -> p'.fromUser case _ of
          Left b -> prop b
          Right c -> do
            Ref.write (Just c) cRef
            busy <- Ref.read busyRef
            unless busy do
              Ref.write true busyRef
              ma <- Ref.read aRef
              for_ ma \a -> p'.toUser $ Tuple a c
              Ref.write false busyRef
            pure Nothing
      }

-- | The `+ → ×` **co-strength** (retraction of `Retaining`): every emission
-- | `Tuple b c` yields `b` and immediately re-enters the widget as a
-- | `Right c` resume — a **productive unfold**/generator.
-- | `coretain (retain g) ≅ g` once the state channel is primed.
instance Functor m => Coretaining (UI m) where
  coretain p = wrap $ unwrap p <#> \p' ->
    -- the resume re-entry is guarded: a record-output widget echoes on
    -- `toUser`, and an unguarded re-feed would loop on its own echo
    let busyRef = unsafePerformEffect $ Ref.new false
    in
      { toUser: \a -> p'.toUser $ Left a
      , fromUser: \prop -> p'.fromUser \(Tuple b c) -> do
          status <- prop b
          busy <- Ref.read busyRef
          unless busy do
            Ref.write true busyRef
            p'.toUser $ Right c
            Ref.write false busyRef
          pure status
      }

instance Apply m => Semigroupoid (UI m) where
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
            p2'.toUser x
            pure Nothing
      }

-- | `identity` forwards its input straight to its output: a wire. The unit
-- | of `compose` (up to effect timing), the element the diagonal unary laws
-- | pin, and — at `{}` — an alternative `RecordToRecord.pempty`.
instance Applicative m => Category (UI m) where
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
silence :: forall m i o. Applicative m => UI m i o
silence = wrap $ pure
  { toUser: mempty
  , fromUser: mempty
  }

-- | The **announcing constant**: silent except for one emission of `o` at
-- | registration — the value-level generalization of the record units'
-- | `{}` announcement (`Web.staticText`'s protocol, with a payload). As a
-- | merge operand it seeds fields or cases; in front of a knowledge-gated
-- | trace (`folding`, `feedback`) it primes the state channel — the fold
-- | announces its initial state the way `pempty` announces its
-- | informationless `{}`.
announce :: forall m o. Applicative m => o -> UI m {} o
announce o = wrap $ pure
  { toUser: mempty
  , fromUser: \prop -> void $ prop o
  }

-- | The **seeded echo wire**: `identity`'s pass-through plus one emission
-- | of the seed at registration. As the first stage of a knowledge-gated
-- | trace's inner (`feedback`, `unfolding`), the seed emission flows into
-- | the following stages, they render and emit, and the trace's state
-- | channel is primed before any input arrives — `announce`'s job, at a
-- | pass-through type.
seeded :: forall m a. Applicative m => a -> UI m a a
seeded a = wrap $ pure unit <#> \_ ->
  -- ref per unwrap, like `identity` (a shared `let` would wire all
  -- instantiations together)
  let mPropRef = unsafePerformEffect $ Ref.new Nothing
  in
    { toUser: \ch -> do
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> void $ prop ch
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        void $ prop a
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
updates :: forall m s e. Functor m => (e -> s -> s) -> UI m s e -> UI m s s
updates handler events = wrap $ unwrap events <#> \evts ->
  let sRef = unsafePerformEffect $ Ref.new Nothing
      mPropRef = unsafePerformEffect $ Ref.new Nothing
  in
    { toUser: \s -> do
        Ref.write (Just s) sRef
        evts.toUser s
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> void $ prop s
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        evts.fromUser \e -> do
          ms <- Ref.read sRef
          case ms of
            Nothing -> pure Nothing
            Just s -> do
              let s' = handler e s
              Ref.write (Just s') sRef
              prop s'
    }

-- | The **heartbeat wire**: `identity`'s pass-through plus a periodic step.
-- | Retains the last value flowing through; every `interval`, applies
-- | `step` to it — `Just` advances (retained and emitted), `Nothing`
-- | pauses until fresh input arrives. Inside a `looped` chain this is a
-- | tick source: the 7GUIs Timer is `every (Milliseconds 100.0) tick`.
-- | The loop runs for the widget's whole life (no cancellation — a
-- | prototype limitation shared with `action'`).
every :: forall m a. Applicative m => Milliseconds -> (a -> Maybe a) -> UI m a a
every interval step = wrap $ pure unit <#> \_ ->
  let lastRef = unsafePerformEffect $ Ref.new Nothing
      mPropRef = unsafePerformEffect $ Ref.new Nothing
  in
    { toUser: \a -> do
        Ref.write (Just a) lastRef
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> void $ prop a
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        let
          loop = do
            delay interval
            liftEffect do
              ma <- Ref.read lastRef
              for_ (ma >>= step) \a' -> do
                Ref.write (Just a') lastRef
                void $ prop a'
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
looped :: forall m a. Functor m => UI m a a -> UI m a a
looped p = wrap $ unwrap p <#> \p' ->
  let busyRef = unsafePerformEffect $ Ref.new false
  in
    { toUser: p'.toUser
    , fromUser: \prop ->
        p'.fromUser \u -> do
          busy <- Ref.read busyRef
          if busy
            then pure Nothing
            else do
              Ref.write true busyRef
              p'.toUser u
              Ref.write false busyRef
              prop u
    }

instance Applicative m => RecordToRecord (UI m) where
  -- the unit announces its informationless {} once, so the merge gates below
  -- never starve against it
  pempty = wrap $ pure
    { toUser: mempty
    , fromUser: \prop -> void $ prop {}
    }
  recordToRecord p1 p2 = wrap ado
    let p1Last = unsafePerformEffect $ Ref.new Nothing
    let p2Last = unsafePerformEffect $ Ref.new Nothing
    p1' <- unwrap (widenRecordInput p1)
    p2' <- unwrap (widenRecordInput p2)
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
              Nothing -> pure Nothing
              Just p2val -> prop $ Record.union exact p2val
          p2'.fromUser \partial -> do
            let exact = exactRow partial
            let _ = unsafePerformEffect $ Ref.write (Just exact) p2Last
            let mp1 = unsafePerformEffect $ Ref.read p1Last
            case mp1 of
              Nothing -> pure Nothing
              Just p1val -> prop $ Record.union p1val exact
      }

instance Applicative m => RecordToVariant (UI m) where
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
instance Functor m => Resolving (UI m) where
  resolve = resolveFor (Milliseconds 300.0)

-- | `resolve` with an explicit quiescence window — see the `Resolving`
-- | instance. `Done` needs no state and fires (after the window) even
-- | unprimed; only the `Loop` branch is gated on a first `c`.
resolveFor :: forall m a b c. Functor m => Milliseconds -> UI m a b -> UI m (Tuple a c) (Either b c)
resolveFor millis p = wrap ado
  let cRef = unsafePerformEffect $ Ref.new Nothing
  let mFiberRef = unsafePerformEffect $ Ref.new Nothing
  p' <- unwrap p
  in
    { toUser: \(Tuple a c) -> do
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
              delay millis
              liftEffect $ void $ prop $ Left b
            liftEffect $ Ref.write (Just newFiber) mFiberRef
          -- loop immediately with the retained state
          mc <- Ref.read cRef
          case mc of
            Nothing -> pure Nothing
            Just c -> prop $ Right c
    }

-- | The Mealy step: a fresh `Left a` feeds the inner widget, a `Right c`
-- | (re)places the retained state. When the inner widget emits `b`, the
-- | output pairs it with the retained `c` — and is **withheld until a `c`
-- | has arrived** (a `Tuple b c` with unknown `c` would be a fabrication),
-- | mirroring the knowledge-gated record merges.
instance Functor m => Retaining (UI m) where
  retain p = wrap ado
    let cRef = unsafePerformEffect $ Ref.new Nothing
    p' <- unwrap p
    in
      { toUser: case _ of
          Left a -> p'.toUser a
          Right c -> Ref.write (Just c) cRef
      , fromUser: \prop ->
          p'.fromUser \b -> do
            mc <- Ref.read cRef
            case mc of
              Nothing -> pure Nothing
              Just c -> prop $ Tuple b c
      }

instance Applicative m => VariantToRecord (UI m) where
  pempty = wrap $ pure
    { toUser: mempty
    , fromUser: \prop -> void $ prop {}
    }
  variantToRecord p1 p2 = wrap ado
    -- gate like `recordToRecord`: hold propagation until both sides' fields
    -- are known (each operand emits its complete sub-record)
    let p1Last = unsafePerformEffect $ Ref.new Nothing
    let p2Last = unsafePerformEffect $ Ref.new Nothing
    p1' <- unwrap p1
    p2' <- unwrap p2
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
              Nothing -> pure Nothing
              Just p2val -> prop $ Record.union exact p2val
          p2'.fromUser \partial -> do
            let exact = exactRow partial
            let _ = unsafePerformEffect $ Ref.write (Just exact) p2Last
            let mp1 = unsafePerformEffect $ Ref.read p1Last
            case mp1 of
              Nothing -> pure Nothing
              Just p1val -> prop $ Record.union p1val exact
      }

instance Applicative m => VariantToVariant (UI m) where
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
constant :: forall a s t m. Functor m => a -> Optic (UI m) s t a Void
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

type Action s t a b = forall m. Functor m => Optic (UI m) s t a b

-- | The progress slot is row-shaped like every component interface: the
-- | widget is a `{ busy :: Boolean } → {}` display citizen.
action :: forall s t. (s -> Aff t) -> Action s t { busy :: Boolean } {}
action arr = action' \i pro post -> do
  liftEffect $ pro { busy: true }
  o <- arr i
  liftEffect $ pro { busy: false }
  liftEffect $ post o

action' :: forall a b i o m. Functor m => (i -> (a -> Effect Unit) -> (o -> Effect Unit) -> Aff Unit) -> Optic (UI m) i o a b
action' arr w = wrap ado
  let oVar = unsafePerformEffect $ liftEffect AVar.empty
  w' <- unwrap w
  in
    { toUser: \i -> launchAff_ $ arr i (\a -> void $ w'.toUser a) (\o -> void $ AVar.put o oVar mempty)
    , fromUser: \prop ->
      let waitAndPropagate = void $ AVar.take oVar case _ of
            Left error -> pure unit -- TODO handle error
            Right o -> do
              void $ prop o -- TODO really?
              waitAndPropagate
      in waitAndPropagate
    }

-- notice: this is not really optics, operates for given m
-- TODO add release parameter?
-- TODO is this needed?
effAdapter :: forall m a b s t. Apply m => m { pre :: s -> Effect a, post ::  b -> Effect t} -> Optic (UI m) s t a b
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
affAdapter :: forall m a b s t. Apply m => m { pre :: s -> Aff a, post ::  b -> Aff t} -> Optic (UI m) s t a b
affAdapter f w = wrap ado
  { toUser, fromUser } <- unwrap w
  { pre, post } <- f
  let mInputFiberRef = unsafePerformEffect $ Ref.new Nothing
  let mOutputFiberRef = unsafePerformEffect $ Ref.new Nothing
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
        pure Nothing
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
debounced' :: forall m. Applicative m => Milliseconds -> Ocular (UI m)
debounced' millis = affAdapter $ pure
  { pre: \i -> delay millis *> pure i
  , post: pure
  }

debounced :: forall m. Applicative m => Ocular (UI m)
debounced = debounced' (Milliseconds 300.0)

spied :: forall m. Functor m => DebugWarning => String -> Ocular (UI m)
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
    spy' text a = spy ("Spied UI \"" <> name <> "\" " <> text <> " new value") a
