module UI
  ( Action
  , New(..)
  , PropagationError
  , PropagationStatus
  , UI(..)
  , action
  , action'
  , affAdapter
  , constant
  , debounced
  , debounced'
  , effAdapter
  , looped
  , silence
  , spied
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Traversable (traverse)
import Data.Lens (Optic)
import Data.Lens.Extra.Types (Ocular)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (class Profunctor, lcmap)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Cochoice (class Cochoice)
import Data.Profunctor.Costrong (class Costrong)
import Data.Profunctor.Row.RecordToRecord (class RecordToRecord)
import Data.Profunctor.Row.RecordToVariant (class RecordToVariant, class Resolving, class Coresolving)
import Data.Profunctor.Row (widenRecordInput, widenVariantOutput)
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
  { toUser :: New i -> Effect Unit
  , fromUser :: (New o -> Effect PropagationStatus) -> Effect Unit
  })

-- TODO: rename ValidationStatus/UserInputStatus?
type PropagationStatus = Maybe PropagationError

type PropagationError = String

derive instance Newtype (UI m i o) _

data New a = New a Boolean -- Boolean value denotes whether the value is likely to be replaced soon (e.g. via text user input)

derive instance Functor New

instance Functor m => Profunctor (UI m) where
  dimap pre post p = wrap ado
    p' <- unwrap p
    in
      { toUser: map pre >>> p'.toUser
      , fromUser: lcmap (map post) >>> p'.fromUser
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
      { toUser: \(New ab cont) -> do
          Ref.write (Just ab) lastab
          p'.toUser $ New (fst ab) cont
      , fromUser: \prop ->
          p'.fromUser \u -> do
            mab <- Ref.read lastab
            case mab of
              Nothing -> pure Nothing
              Just prevab -> prop (map (flip Tuple (snd prevab)) u)
      }
  second p = wrap ado
    let lastab = unsafePerformEffect $ Ref.new Nothing
    p' <- unwrap p
    in
      { toUser: \(New ab cont) -> do
          Ref.write (Just ab) lastab
          p'.toUser $ New (snd ab) cont
      , fromUser: \prop ->
          p'.fromUser \u -> do
            mab <- Ref.read lastab
            case mab of
              Nothing -> pure Nothing
              Just prevab -> prop (map (Tuple (fst prevab)) u)
      }

instance Functor m => Choice (UI m) where
  left p = wrap ado
    let mPropRef = unsafePerformEffect $ Ref.new Nothing
    p' <- unwrap p
    in
      { toUser: case _ of
        New (Right c) cont -> do
          mProp <- Ref.read mPropRef
          for_ mProp \prop -> prop (New (Right c) cont)
        New (Left a) cont -> p'.toUser $ New a cont
      , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        p'.fromUser \u -> prop (Left <$> u)
      }
  right p = wrap ado
    let mPropRef = unsafePerformEffect $ Ref.new Nothing
    p' <- unwrap p
    in
      { toUser: case _ of
        New (Left c) cont -> do
          mProp <- Ref.read mPropRef
          for_ mProp \prop -> prop (New (Left c) cont)
        New (Right a) cont -> p'.toUser $ New a cont
      , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        p'.fromUser \u -> prop (Right <$> u)
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
      { toUser: \(New a cont) -> do
          mc <- Ref.read cRef
          for_ mc \c -> p'.toUser $ New (Tuple a c) cont
      , fromUser: \prop ->
          p'.fromUser \(New (Tuple b c) cont) -> do
            Ref.write (Just c) cRef
            prop $ New b cont
      }
  unsecond p = wrap ado
    let aRef = unsafePerformEffect $ Ref.new Nothing
    p' <- unwrap p
    in
      { toUser: \(New b cont) -> do
          ma <- Ref.read aRef
          for_ ma \a -> p'.toUser $ New (Tuple a b) cont
      , fromUser: \prop ->
          p'.fromUser \(New (Tuple a c) cont) -> do
            Ref.write (Just a) aRef
            prop $ New c cont
      }

-- | The `+`-diagonal **trace** (dual of `Choice`): a looped-branch emission
-- | re-enters the widget as input — feedback of **control**, i.e. iteration —
-- | until an exit-branch emission passes through. The re-entry is a `toUser`,
-- | so in `UI` the loop is an *event* loop: it advances on the widget's next
-- | emission (variant-output widgets do not echo, so the leaf protocol cannot
-- | provoke a synchronous spin). Retraction law: `unleft (left g) ≅ g`.
instance Functor m => Cochoice (UI m) where
  unleft p = wrap $ unwrap p <#> \p' ->
    { toUser: \(New a cont) -> p'.toUser $ New (Left a) cont
    , fromUser: \prop -> p'.fromUser \(New bc cont) -> case bc of
        Left b -> prop $ New b cont
        Right c -> do
          p'.toUser $ New (Right c) cont
          pure Nothing
    }
  unright p = wrap $ unwrap p <#> \p' ->
    { toUser: \(New a cont) -> p'.toUser $ New (Right a) cont
    , fromUser: \prop -> p'.fromUser \(New cb cont) -> case cb of
        Right b -> prop $ New b cont
        Left c -> do
          p'.toUser $ New (Left c) cont
          pure Nothing
    }

-- | The `× → +` **co-strength** (retraction of `Resolving`): a `Right c`
-- | emission is retained as the state paired with subsequent inputs, a
-- | `Left b` exits — a **terminating fold**. Gated like `Costrong` (a first
-- | `c` must arrive before inputs pass); `coresolve (resolve g) ≅ g` once
-- | primed.
instance Functor m => Coresolving (UI m) where
  coresolve p = wrap ado
    let cRef = unsafePerformEffect $ Ref.new Nothing
    p' <- unwrap p
    in
      { toUser: \(New a cont) -> do
          mc <- Ref.read cRef
          for_ mc \c -> p'.toUser $ New (Tuple a c) cont
      , fromUser: \prop -> p'.fromUser \(New bc cont) -> case bc of
          Left b -> prop $ New b cont
          Right c -> do
            Ref.write (Just c) cRef
            pure Nothing
      }

-- | The `+ → ×` **co-strength** (retraction of `Retaining`): every emission
-- | `Tuple b c` yields `b` and immediately re-enters the widget as a
-- | `Right c` resume — a **productive unfold**/generator.
-- | `coretain (retain g) ≅ g` once the state channel is primed.
instance Functor m => Coretaining (UI m) where
  coretain p = wrap $ unwrap p <#> \p' ->
    { toUser: \(New a cont) -> p'.toUser $ New (Left a) cont
    , fromUser: \prop -> p'.fromUser \(New (Tuple b c) cont) -> do
        status <- prop $ New b cont
        p'.toUser $ New (Right c) cont
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
          p1'.fromUser \x -> do
            p2'.toUser x
            pure Nothing
          p2'.fromUser prop
      }

-- | `identity` forwards its input straight to its output: a wire. The unit
-- | of `compose` (up to effect timing), the element the diagonal unary laws
-- | pin, and — at `{}` — an alternative `RecordToRecord.pempty`.
instance Applicative m => Category (UI m) where
  identity = wrap ado
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
    , fromUser: \prop -> void $ prop $ New {} false
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
          p1'.fromUser \(New partial cont) -> do
            let _ = unsafePerformEffect $ Ref.write (Just partial) p1Last
            let mp2 = unsafePerformEffect $ Ref.read p2Last
            case mp2 of
              Nothing -> pure Nothing
              Just p2val -> prop $ New (Record.union partial p2val) cont
          p2'.fromUser \(New partial cont) -> do
            let _ = unsafePerformEffect $ Ref.write (Just partial) p2Last
            let mp1 = unsafePerformEffect $ Ref.read p1Last
            case mp1 of
              Nothing -> pure Nothing
              Just p1val -> prop $ New (Record.union p1val partial) cont
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

-- | The loop step: the input `Tuple a c` shows `a` to the inner widget and
-- | retains `c`. When the inner widget emits, the **continuity flag decides
-- | the branch**: a transient emission (`cont = true`, e.g. mid-typing) takes
-- | the `Loop` branch, escaping with the retained `c`; a final emission
-- | (`cont = false`, e.g. a button click) is `Done` with the emitted `b`.
-- | Until a `c` has arrived there is nothing valid to loop with, so transient
-- | emissions are withheld.
instance Functor m => Resolving (UI m) where
  resolve p = wrap ado
    let cRef = unsafePerformEffect $ Ref.new Nothing
    p' <- unwrap p
    in
      { toUser: \(New (Tuple a c) cont) -> do
          Ref.write (Just c) cRef
          p'.toUser $ New a cont
      , fromUser: \prop ->
          p'.fromUser \(New b cont) ->
            if cont
              then do
                mc <- Ref.read cRef
                case mc of
                  Nothing -> pure Nothing
                  Just c -> prop $ New (Right c) cont
              else prop $ New (Left b) cont
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
          New (Left a) cont -> p'.toUser $ New a cont
          New (Right c) _ -> Ref.write (Just c) cRef
      , fromUser: \prop ->
          p'.fromUser \(New b cont) -> do
            mc <- Ref.read cRef
            case mc of
              Nothing -> pure Nothing
              Just c -> prop $ New (Tuple b c) cont
      }

instance Applicative m => VariantToRecord (UI m) where
  pempty = wrap $ pure
    { toUser: mempty
    , fromUser: \prop -> void $ prop $ New {} false
    }
  variantToRecord p1 p2 = wrap ado
    -- gate like `recordToRecord`: hold propagation until both sides' fields
    -- are known (each operand emits its complete sub-record)
    let p1Last = unsafePerformEffect $ Ref.new Nothing
    let p2Last = unsafePerformEffect $ Ref.new Nothing
    p1' <- unwrap p1
    p2' <- unwrap p2
    in
      { toUser: \(New v cont) -> do
          for_ (contract v :: Maybe _) \v1 -> p1'.toUser $ New v1 cont
          for_ (contract v :: Maybe _) \v2 -> p2'.toUser $ New v2 cont
      , fromUser: \prop -> do
          p1'.fromUser \(New partial cont) -> do
            let _ = unsafePerformEffect $ Ref.write (Just partial) p1Last
            let mp2 = unsafePerformEffect $ Ref.read p2Last
            case mp2 of
              Nothing -> pure Nothing
              Just p2val -> prop $ New (Record.union partial p2val) cont
          p2'.fromUser \(New partial cont) -> do
            let _ = unsafePerformEffect $ Ref.write (Just partial) p2Last
            let mp1 = unsafePerformEffect $ Ref.read p1Last
            case mp1 of
              Nothing -> pure Nothing
              Just p1val -> prop $ New (Record.union p1val partial) cont
      }

instance Applicative m => VariantToVariant (UI m) where
  pempty = silence
  variantToVariant p1 p2 = wrap ado
    p1' <- unwrap (widenVariantOutput p1)
    p2' <- unwrap (widenVariantOutput p2)
    in
      { toUser: \(New v cont) -> do
          for_ (contract v :: Maybe _) \v1 -> p1'.toUser $ New v1 cont
          for_ (contract v :: Maybe _) \v2 -> p2'.toUser $ New v2 cont
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
        w'.toUser $ New a false
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
    { toUser: case _ of
      New i cont -> launchAff_ $ arr i (\a -> void $ w'.toUser $ New a cont) (\o -> void $ AVar.put o oVar mempty)
    , fromUser: \prop ->
      let waitAndPropagate = void $ AVar.take oVar case _ of
            Left error -> pure unit -- TODO handle error
            Right o -> do
              -- w'.toUser $ New [] Nothing false
              void $ prop $ New o false -- TODO really?
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
    { toUser: case _ of
      New s cont -> do
        a <- pre s
        toUser $ New a cont
    , fromUser: \prop -> do
      fromUser case _ of
        New b cont -> do
          t <- post b
          prop $ New t cont
    }

-- TODO is this needed?
affAdapter :: forall m a b s t. Apply m => m { pre :: New s -> Aff a, post ::  New b -> Aff t} -> Optic (UI m) s t a b
affAdapter f w = wrap ado
  { toUser, fromUser } <- unwrap w
  { pre, post } <- f
  let mInputFiberRef = unsafePerformEffect $ Ref.new Nothing
  let mOutputFiberRef = unsafePerformEffect $ Ref.new Nothing
  in
    { toUser: case _ of
      news@(New _ cont) -> launchAff_ do
        mFiber <- liftEffect $ Ref.read mInputFiberRef
        for_ mFiber $ killFiber (error "Obsolete input")
        newFiber <- forkAff do
          a <- pre news
          liftEffect $ toUser $ New a cont
        liftEffect $ Ref.write (Just newFiber) mInputFiberRef
    , fromUser: \prop -> do
      fromUser case _ of
        newb@(New _ cont) -> do
          launchAff_ do
            mFiber <- liftEffect $ Ref.read mOutputFiberRef
            for_ mFiber $ killFiber (error "Obsolete output")
            newFiber <- forkAff do
              t <- post newb
              liftEffect $ prop $ New t cont
            liftEffect $ Ref.write (Just newFiber) mOutputFiberRef
          pure Nothing
    }

-- Oculars

debounced' :: forall m. Applicative m => Milliseconds -> Ocular (UI m)
debounced' millis = affAdapter $ pure
  { pre: case _ of
    (New i true) -> delay millis *> pure i
    (New i false) -> pure i
  , post: \(New i _) -> pure i
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
    spy' :: forall a. String -> New a -> a
    spy' text (New a cont) = spy ("Spied UI \"" <> name <> "\" " <> text <> " new value with continuity " <> show cont) a
