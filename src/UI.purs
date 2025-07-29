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
  , spied
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Lens (Optic)
import Data.Lens.Extra.Types (Ocular)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (class Profunctor, lcmap)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Endo (class Endo)
import Data.Profunctor.Strong (class Strong)
import Data.Profunctor.Sum (class Sum)
import Data.Profunctor.Zero (class Zero)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..), fst, snd)
import Debug (class DebugWarning, spy)
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (Aff, delay, error, forkAff, killFiber, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Unsafe.Coerce (unsafeCoerce)

newtype UI m i o = UI (m
  { toUser :: New i -> Effect Unit
  , fromUser :: (New o -> Effect PropagationStatus) -> Effect Unit
  })

-- TODO: rename ValidationStatus/UserInputStatus?
type PropagationStatus = Maybe PropagationError

type PropagationError = String

derive instance Newtype (UI m i o) _

data New a = New a Boolean

derive instance Functor New

instance Functor m => Profunctor (UI m) where
  dimap pre post p = wrap ado
    p' <- unwrap p
    in
      { toUser: map pre >>> p'.toUser
      , fromUser: lcmap (map post) >>> p'.fromUser
      }

instance Functor m => Strong (UI m) where
  first p = wrap ado
    let lastab = unsafePerformEffect $ Ref.new (unsafeCoerce unit)
    p' <- unwrap p
    in
      { toUser: case _ of
          New ab cont -> do
            let _ = unsafePerformEffect $ Ref.write ab lastab
            p'.toUser $ New (fst ab) cont
      , fromUser: \prop -> do
        p'.fromUser \u -> do
          let prevab = unsafePerformEffect $ Ref.read lastab
          prop (map (flip Tuple (snd prevab)) u)
      }
  second p = wrap ado
    let lastab = unsafePerformEffect $ Ref.new (unsafeCoerce unit)
    p' <- unwrap p
    in
      { toUser: case _ of
          New ab cont -> do
            let _ = unsafePerformEffect $ Ref.write ab lastab
            p'.toUser $ New (snd ab) cont
      , fromUser: \prop -> do
        p'.fromUser \u -> do
          let prevab = unsafePerformEffect $ Ref.read lastab
          prop (map (Tuple (fst prevab)) u)
      }

instance Functor m => Choice (UI m) where
  left p = wrap ado
    let propRef = unsafePerformEffect $ Ref.new (unsafeCoerce unit)
    p' <- unwrap p
    in
      { toUser: case _ of
        New (Right c) cont -> do
          let prop = unsafePerformEffect $ Ref.read propRef
          _ <- prop (New (Right c) cont)
          pure unit
        New (Left a) cont -> p'.toUser $ New a cont
      , fromUser: \prop -> do
        Ref.write prop propRef
        p'.fromUser \u -> prop (Left <$> u)
      }
  right p = wrap ado
    p' <- unwrap p
    let propRef = unsafePerformEffect $ Ref.new (unsafeCoerce unit)
    in
      { toUser: case _ of
        New (Left c) cont -> do
          let prop = unsafePerformEffect $ Ref.read propRef
          _ <- prop (New (Left c) cont)
          pure unit
        New (Right a) cont -> p'.toUser $ New a cont
      , fromUser: \prop -> do
        Ref.write prop propRef
        p'.fromUser \u -> prop (Right <$> u)
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

-- TODO: not sure abot this instance, it is not really a category?
instance Applicative m => Category (UI m) where
  identity = wrap  ado
    let propRef = unsafePerformEffect $ Ref.new (unsafeCoerce unit)
    in
      { toUser: \ch -> do
        let prop = unsafePerformEffect $ Ref.read propRef
        _ <- prop ch
        pure unit
      , fromUser: \prop -> do
        Ref.write prop propRef
      }

instance Apply m => Sum (UI m) where
  psum p1 p2 = wrap ado
    p1' <- unwrap p1
    p2' <- unwrap p2
    in
      { toUser: \ch -> p1'.toUser ch *> p2'.toUser ch
      , fromUser: \prop -> p1'.fromUser prop *> p2'.fromUser prop
      }

instance Applicative m => Zero (UI m) where
  pzero = wrap $ pure
    { toUser: mempty
    , fromUser: mempty
    }

instance Apply m => Endo (UI m) where
  pendo p1 p2 = wrap ado
    p1' <- unwrap p1
    p2' <- unwrap p2
    in
      { toUser: \ch -> do
        p1'.toUser ch
        p2'.toUser ch
      , fromUser: \prop -> do
        p1'.fromUser \u -> do
          p1'.toUser u
          p2'.toUser u
          prop u
        p2'.fromUser \u -> do
          p1'.toUser u
          p2'.toUser u
          prop u
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

action :: forall s t. (s -> Aff t) -> Action s t Boolean Void
action arr = action' \i pro post -> do
  liftEffect $ pro true
  o <- arr i
  liftEffect $ pro false
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
