module Widget
  ( Change(..)
  , Scope(..)
  , Widget(..)
  , WidgetOptics
  , WidgetOptics'
  , adapter
  , affAdapter
  , affArr
  , constructor
  , debounced
  , debounced'
  , effAdapter
  , effBracket
  , field
  , constant
  , iso
  , lens
  , prism
  , projection
  )
  where

import Prelude

import Control.Alt (class Alt)
import Control.Plus (class Plus)
import Data.Array (uncons, (:))
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Generic.Rep (class Generic)
import Data.Lens as Profunctor
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (class Profunctor, dimap, lcmap)
import Data.Profunctor.Choice (class Choice, left)
import Data.Profunctor.Strong (class Strong)
import Data.Show.Generic (genericShow)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (Aff, delay, error, forkAff, killFiber, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Prim.Row as Row
import Record (get, set)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

newtype Widget m i o = Widget (m
  { speak :: Maybe (Change i) -> Effect Unit
  , listen :: (Change o -> Effect Unit) -> Effect Unit
  })

derive instance Newtype (Widget m i o) _

data Change a
  = Update (Array Scope) a
  | Removal

derive instance Functor Change

instance Show (Change a) where
  show = case _ of
    Removal -> "Removal"
    Update scopes _ -> "Update " <> joinWith "." (show <$> scopes)

data Scope = Part String | Variant String

derive instance Generic Scope _

instance Show Scope where
  show = genericShow

derive instance Eq Scope

instance Functor m => Profunctor (Widget m) where
  dimap contraf cof p = wrap $ unwrap p <#> \p' ->
    { speak: (_ <<< map (map contraf)) p'.speak
    , listen: p'.listen <<< lcmap (map cof)
    }

instance Applicative m => Strong (Widget m) where
  first p = wrap ado
    let lastab = unsafePerformEffect $ Ref.new (unsafeCoerce unit)
    p' <- unwrap p
    in
      { speak: \mch -> do
        case mch of
          Nothing -> pure unit
          Just Removal -> p'.speak $ Just Removal
          Just (Update _ ab) -> do
            let _ = unsafePerformEffect $ Ref.write ab lastab
            p'.speak $ map (map fst) mch
      , listen: \prop -> do
        p'.listen \ch -> do
          let prevab = unsafePerformEffect $ Ref.read lastab
          prop (map (flip Tuple (snd prevab)) ch)
      }
  second p = wrap ado
    let lastab = unsafePerformEffect $ Ref.new (unsafeCoerce unit)
    p' <- unwrap p
    in
      { speak: \mch -> do
        case mch of
          Nothing -> pure unit
          Just Removal -> p'.speak $ Just Removal
          Just (Update _ ab) -> do
            let _ = unsafePerformEffect $ Ref.write ab lastab
            p'.speak $ map (map snd) mch
      , listen: \prop -> do
        p'.listen \ch -> do
          let prevab = unsafePerformEffect $ Ref.read lastab
          prop (map (Tuple (fst prevab)) ch)
      }

instance Applicative m => Choice (Widget m) where
  left p = wrap ado
    p' <- unwrap p
    in
      { speak: case _ of
        Nothing -> p'.speak Nothing
        Just Removal -> p'.speak $ Just Removal
        Just (Update _ (Right _)) -> p'.speak $ Just Removal
        Just (Update scope (Left a)) -> p'.speak $ Just $ Update scope a
      , listen: \prop -> do
        p'.listen \ch -> prop (Left <$> ch)
      }
  right p = wrap ado
    p' <- unwrap p
    in
      { speak: case _ of
        Nothing -> p'.speak Nothing
        Just Removal -> p'.speak $ Just Removal
        Just (Update _ (Left _)) -> p'.speak $ Just Removal
        Just (Update scope (Right a)) -> p'.speak $ Just $ Update scope a
      , listen: \prop -> do
        p'.listen \ch -> prop (Right <$> ch)
      }

instance Apply m => Semigroup (Widget m a a) where
  append p1 p2 = wrap ado
    p1' <- unwrap p1
    p2' <- unwrap p2
    in
      { speak: \ch -> p1'.speak ch *> p2'.speak ch
      , listen: \prop -> (p1'.listen \ch -> p2'.speak (Just ch) *> prop ch) *> (p2'.listen \ch -> p1'.speak (Just ch) *> prop ch)
      }
-- Notice: optic `Widget m c d -> Widget m a a` is also a Semigroup

instance Applicative m => Monoid (Widget m a a) where
  mempty = wrap $ pure
    { speak: mempty
    , listen: mempty
    }
-- Notice: optic `Widget m c d -> Widget m a a` is also a Monoid

instance MonadEffect m => Semigroupoid (Widget m) where
  compose p2 p1 = wrap do
    p1' <- unwrap p1
    p2' <- unwrap p2
    liftEffect $ p1'.listen \ch -> case ch of
      Removal -> log $ "removed input"
      _ -> p2'.speak $ Just ch -- TODO what if `ch == Remove`?
    pure
      { speak: p1'.speak
      , listen: p2'.listen
      }

instance MonadEffect m => Category (Widget m) where
  identity = wrap do
    chaAVar <- liftEffect AVar.empty
    pure
      { speak: case _ of
        Nothing -> pure unit
        Just cha -> void $ AVar.put cha chaAVar mempty
      , listen: \prop ->
        let waitAndPropagate = void $ AVar.take chaAVar case _ of
              Left error -> pure unit -- TODO handle error
              Right cha -> do
                prop cha
                waitAndPropagate
        in waitAndPropagate
      }

instance Functor m => Functor (Widget m a) where
  map f p = wrap $ unwrap p <#> \p' ->
    { speak: p'.speak
    , listen: p'.listen <<< lcmap (map f)
    }

instance Apply m => Alt (Widget m a) where
  alt p1 p2 = wrap ado
    p1' <- unwrap p1
    p2' <- unwrap p2
    in
      { speak: \ch -> p1'.speak ch *> p2'.speak ch
      , listen: \prop -> p1'.listen prop *> p2'.listen prop
      }

instance Applicative m => Plus (Widget m a) where
  empty = wrap $ pure
    { speak: const $ pure unit
    , listen: const $ pure unit
    }

-- optics

type WidgetOptics a b s t = forall m. MonadEffect m => Widget m a b -> Widget m s t
type WidgetOptics' a s = WidgetOptics a a s s

constant :: forall a b s t. a -> WidgetOptics a b s t
constant a w = wrap do
  w' <- unwrap w
  liftEffect $ w'.speak $ Just $ Update [] a
  pure
    { speak: const $ pure unit
    , listen: const $ pure unit
    }

adapter :: forall a b s t. String -> (s -> a) -> (b -> t) -> WidgetOptics a b s t
adapter name mapin mapout = dimap mapin mapout >>> scopemap (Variant name) -- TODO not sure about `Variant name`

iso :: forall a s. String -> (s -> a) -> (a -> s) -> WidgetOptics' a s
iso name mapin mapout = dimap mapin mapout >>> scopemap (Variant name)

projection :: forall a b s. (s -> a) -> WidgetOptics a b s b
projection f = dimap f identity

lens :: forall a b s t. String -> (s -> a) -> (s -> b -> t) -> WidgetOptics a b s t
lens name getter setter = Profunctor.lens getter setter >>> scopemap (Variant name)

-- TODO use Data.Lens.Record.prop
field :: forall @l s r a . IsSymbol l => Row.Cons l a r s => WidgetOptics' a (Record s)
field = scopemap (Part (reflectSymbol (Proxy @l))) >>> Profunctor.lens (get (Proxy @l)) (flip (set (Proxy @l)))

prism :: forall a b s t. String -> (b -> t) -> (s -> Either t a) -> WidgetOptics a b s t
prism name to from = Profunctor.prism to from >>> scopemap (Variant name)

constructor :: forall a s. String -> (a -> s) -> (s -> Maybe a) -> WidgetOptics' a s
constructor name construct deconstruct = scopemap (Part name) >>> left >>> dimap (\s -> maybe (Right s) Left (deconstruct s)) (either construct identity)

-- modifiers

effBracket :: forall m a b. Monad m => m
  { beforeInputChange :: Maybe (Change a) -> Effect Unit
  , afterInputChange :: Maybe (Change a) -> Effect Unit
  , beforeOutputChange :: Change b -> Effect Unit
  , afterOutputChange :: Change b -> Effect Unit
  } -> Widget m a b -> Widget m a b
effBracket f w = wrap do
  { speak, listen } <- unwrap w
  { beforeInputChange, afterInputChange, beforeOutputChange, afterOutputChange } <- f
  pure
    { speak: \ch -> beforeInputChange ch *> speak ch *> afterInputChange ch
    , listen: \prop -> do
      listen \ch -> beforeOutputChange ch *> prop ch *> afterOutputChange ch
    }

-- notice: this is not really optics, operates for given m
-- TODO add release parameter?
effAdapter :: forall m a b s t. Monad m => m { pre :: s -> Effect a, post ::  b -> Effect t} -> Widget m a b -> Widget m s t
effAdapter f w = wrap do
  { speak, listen } <- unwrap w
  { pre, post } <- f
  pure
    { speak: case _ of
      Nothing -> speak Nothing
      Just (Update _ s) -> do
        a <- pre s
        speak $ Just $ Update [] a
      _ -> pure unit -- TODO really?
    , listen: \prop -> do
      listen case _ of
        Update _ b -> do
          t <- post b
          prop $ Update [] t
        _ -> pure unit -- TODO really?
    }

affAdapter :: forall m a b s t. MonadEffect m => m { pre :: s -> Aff a, post ::  b -> Aff t} -> Widget m a b -> Widget m s t
affAdapter f w = wrap do
  { speak, listen } <- unwrap w
  { pre, post } <- f
  mInputFiberRef <- liftEffect $ Ref.new Nothing
  mOutputFiberRef <- liftEffect $ Ref.new Nothing
  pure
    { speak: case _ of
      Nothing -> speak Nothing
      Just (Update _ s) -> launchAff_ do
        mFiber <- liftEffect $ Ref.read mInputFiberRef
        for_ mFiber $ killFiber (error "Obsolete input")
        newFiber <- forkAff do
          a <- pre s
          liftEffect $ speak $ Just $ Update [] a
        liftEffect $ Ref.write (Just newFiber) mInputFiberRef
      _ -> pure unit -- TODO really?
    , listen: \prop -> do
      listen case _ of
        Update _ b -> launchAff_ do
          mFiber <- liftEffect $ Ref.read mOutputFiberRef
          for_ mFiber $ killFiber (error "Obsolete output")
          newFiber <- forkAff do
            t <- post b
            liftEffect $ prop $ Update [] t
          liftEffect $ Ref.write (Just newFiber) mOutputFiberRef
        _ -> pure unit -- TODO really?
    }

affArr :: forall m a b. MonadEffect m => (a -> Aff b) -> Widget m a b
affArr arr = identity # affAdapter (pure { pre: arr, post: pure })

effLens :: forall m a b s t. MonadEffect m => Widget m a b -> m { get :: s -> Effect a, set :: s -> b -> Effect t} -> Widget m s t
effLens w mlens = wrap do
  { speak, listen } <- unwrap w
  sref <- liftEffect $ Ref.new $ unsafeCoerce unit
  { get, set } <- mlens
  pure
    { speak: case _ of
      Nothing -> speak Nothing
      Just (Update _ s) -> do
        a <- get s
        Ref.write s sref
        speak $ Just $ Update [] a
      _ -> pure unit -- TODO really?
    , listen: \prop -> do
      listen case _ of
        Update _ b -> do
          s <- Ref.read sref
          t <- set s b
          prop $ Update [] t
        _ -> pure unit -- TODO really?
    }

effPrism :: forall m a b s t. MonadEffect m => Widget m a b -> m { to :: s -> Effect (Either t a), from :: b -> Effect t} -> Widget m s t
effPrism w mprism = wrap do
  { speak, listen } <- unwrap w
  sref <- liftEffect $ Ref.new $ unsafeCoerce unit
  { to, from } <- mprism
  pure
    { speak: case _ of
      Nothing -> speak Nothing
      Just (Update _ s) -> do
        tora <- to s
        case tora of
          Right a -> speak $ Just $ Update [] a
          Left t -> speak $ Just Removal
      Just Removal -> speak $ Just Removal
      _ -> pure unit -- TODO really?
    , listen: \prop -> do
      listen case _ of
        Update _ b -> do
          s <- Ref.read sref
          t <- from b
          prop $ Update [] t
        _ -> pure unit -- TODO really?
    }

debounced :: forall m a b. MonadEffect m => Milliseconds -> Widget m a b -> Widget m a b
debounced millis = affAdapter $ pure
  { pre: pure
  , post: \i -> delay millis *> pure i
  }

debounced' :: forall m a b. MonadEffect m => Widget m a b -> Widget m a b
debounced' = debounced (Milliseconds 500.0)

-- private

scopemap :: forall m a b. Applicative m => Scope -> Widget m a b -> Widget m a b
scopemap scope p = wrap ado
  { speak, listen } <- unwrap p
  in
    { speak: speak <<< zoomIn
    , listen: \prop -> do
      listen $ prop <<< zoomOut
    }
  where
    zoomOut :: Change b -> Change b
    zoomOut Removal = Removal
    zoomOut (Update scopes mb) = Update (scope : scopes) mb

    zoomIn :: Maybe (Change a) -> Maybe (Change a)
    zoomIn Nothing = Nothing
    zoomIn (Just Removal) = Just Removal
    zoomIn (Just (Update scopes ma)) = case uncons scopes of
      Just { head, tail } | head == scope -> Just $ Update tail ma
      Nothing -> Just $ Update [] ma
      Just { head: Variant _ } -> Just $ Update [] ma -- not matching head but head is twist
      _ -> case scope of
        Variant _ -> Just $ Update [] ma -- not matching head but scope is twist
        _ -> Nothing -- otherwise

