module Widget
  ( Change(..)
  , Changed(..)
  , Scope(..)
  , Widget(..)
  , WidgetOptics
  , WidgetOptics'
  , adapter
  , affAdapter
  , action
  , constant
  , constructor
  , debounced
  , debounced'
  , effAdapter
  , effBracket
  , field
  , iso
  , lens
  , prism
  , projection
  , spied
  )
  where

import Prelude

import Control.Alt (class Alt)
import Control.Plus (class Plus)
import Data.Array (null, uncons, (:))
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
  , listen :: (Changed o -> Effect Unit) -> Effect Unit
  })

derive instance Newtype (Widget m i o) _

data Changed a = Changed (Array Scope) a

instance Show a => Show (Changed a) where
  show (Changed scopes a) = "Changed " <> (if null scopes then "entire" else joinWith "." (show <$> scopes) <> " part of") <> " " <> show a

derive instance Functor Changed

data Change a
  = Update (Changed a)
  | Removal

derive instance Functor Change

instance Show a => Show (Change a) where
  show = case _ of
    Removal -> "Removal"
    Update ch -> show ch

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
      { speak: case _ of
          Nothing -> pure unit
          Just Removal -> p'.speak $ Just Removal
          Just (Update (Changed scope ab)) -> do
            let _ = unsafePerformEffect $ Ref.write ab lastab
            p'.speak $ Just $ Update $ Changed scope $ fst ab
      , listen: \prop -> do
        p'.listen \ch -> do
          let prevab = unsafePerformEffect $ Ref.read lastab
          prop (map (flip Tuple (snd prevab)) ch)
      }
  second p = wrap ado
    let lastab = unsafePerformEffect $ Ref.new (unsafeCoerce unit)
    p' <- unwrap p
    in
      { speak: case _ of
          Nothing -> pure unit
          Just Removal -> p'.speak $ Just Removal
          Just (Update (Changed scope ab)) -> do
            let _ = unsafePerformEffect $ Ref.write ab lastab
            p'.speak $ Just $ Update $ Changed scope $ snd ab
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
        Nothing -> pure unit
        Just Removal -> p'.speak $ Just Removal
        Just (Update (Changed _ (Right _))) -> p'.speak $ Just Removal
        Just (Update (Changed scope (Left a))) -> p'.speak $ Just $ Update $ Changed scope a
      , listen: \prop -> do
        p'.listen \ch -> prop (Left <$> ch)
      }
  right p = wrap ado
    p' <- unwrap p
    in
      { speak: case _ of
        Nothing -> pure unit
        Just Removal -> p'.speak $ Just Removal
        Just (Update (Changed _ (Left _))) -> p'.speak $ Just Removal
        Just (Update (Changed scope (Right a))) -> p'.speak $ Just $ Update $ Changed scope a
      , listen: \prop -> do
        p'.listen \ch -> prop (Right <$> ch)
      }

instance Apply m => Semigroup (Widget m a a) where
  append p1 p2 = wrap ado
    p1' <- unwrap p1
    p2' <- unwrap p2
    in
      { speak: \ch -> p1'.speak ch *> p2'.speak ch
      , listen: \prop -> (p1'.listen \ch -> p2'.speak (Just $ Update ch) *> prop ch) *> (p2'.listen \ch -> p1'.speak (Just $ Update ch) *> prop ch)
      }
-- Notice: optic `WidgetOptic m a b c c` is also a Semigroup

instance Applicative m => Monoid (Widget m a a) where
  mempty = wrap $ pure
    { speak: mempty
    , listen: mempty
    }
-- Notice: optic `WidgetOptic m a b c c` is also a Monoid

instance MonadEffect m => Semigroupoid (Widget m) where
  compose p2 p1 = wrap do
    p1' <- unwrap p1
    p2' <- unwrap p2
    liftEffect $ p1'.listen \ch -> p2'.speak $ Just $ Update ch -- TODO smell: well, it's not an update it's rather brand new value/event
    pure
      { speak: p1'.speak
      , listen: p2'.listen
      }

-- Notice: Widget is not a category
-- instance MonadEffect m => Category (Widget m) where
--   identity = wrap do
--     chaAVar <- liftEffect AVar.empty
--     pure
--       { speak: \mcha -> void $ AVar.put mcha chaAVar mempty
--       , listen: \prop ->
--         let waitAndPropagate = void $ AVar.take chaAVar case _ of
--               Left error -> pure unit -- TODO handle error
--               Right Nothing -> ... -- impossible
--               Right (Just Removal) -> ... -- impossible
--               Right (Just (Update changeda)) -> do
--                 prop changeda
--                 waitAndPropagate
--         in waitAndPropagate
--       }

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
  liftEffect $ w'.speak $ Just $ Update $ Changed [] a
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
  { beforeInput :: Maybe (Change a) -> Effect Unit
  , afterInput :: Maybe (Change a) -> Effect Unit
  , beforeOutput :: Changed b -> Effect Unit
  , afterOutput :: Changed b -> Effect Unit
  } -> Widget m a b -> Widget m a b
effBracket f w = wrap do
  { speak, listen } <- unwrap w
  { beforeInput, afterInput, beforeOutput, afterOutput } <- f
  pure
    { speak: \ch -> beforeInput ch *> speak ch *> afterInput ch
    , listen: \prop -> do
      listen \ch -> beforeOutput ch *> prop ch *> afterOutput ch
    }

spied :: forall m a . MonadEffect m => Show a => String -> Widget m a a -> Widget m a a
spied name = effBracket do
  pure
    { beforeInput: \a -> log $ "[Spy] " <> name <> " input: " <> show a
    , afterInput: mempty
    , beforeOutput: \a -> log $ "[Spy] " <> name <> " output: " <> show a
    , afterOutput: mempty
    }

-- notice: this is not really optics, operates for given m
-- TODO add release parameter?
effAdapter :: forall m a b s t. Monad m => m { pre :: s -> Effect a, post ::  b -> Effect t} -> Widget m a b -> Widget m s t
effAdapter f w = wrap do
  { speak, listen } <- unwrap w
  { pre, post } <- f
  pure
    { speak: case _ of
      Nothing -> pure unit
      Just Removal -> speak $ Just $ Removal
      Just (Update (Changed _ s)) -> do
        a <- pre s
        speak $ Just $ Update $ Changed [] a
    , listen: \prop -> do
      listen \(Changed _ b) -> do
        t <- post b
        prop $ Changed [] t
    }

action :: forall a i o. (i -> Aff o) -> WidgetOptics Boolean a i o
action arr w = wrap do
  oVar <- liftEffect AVar.empty
  w' <- unwrap w
  pure
    { speak: case _ of
      Nothing -> pure unit
      Just Removal -> pure unit -- TODO really?
      Just (Update (Changed _ i)) -> do
        w'.speak $ Just $ Update $ Changed [] true
        launchAff_ do
          o <- arr i
          liftEffect $ void $ AVar.put o oVar mempty
    , listen: \prop ->
      let waitAndPropagate = void $ AVar.take oVar case _ of
            Left error -> pure unit -- TODO handle error
            Right o -> do
              w'.speak $ Just $ Update $ Changed [] false
              prop $ Changed [] o
              waitAndPropagate
      in waitAndPropagate
    }

affAdapter :: forall m a b s t. MonadEffect m => m { pre :: s -> Aff a, post ::  b -> Aff t} -> Widget m a b -> Widget m s t
affAdapter f w = wrap do
  { speak, listen } <- unwrap w
  { pre, post } <- f
  mInputFiberRef <- liftEffect $ Ref.new Nothing
  mOutputFiberRef <- liftEffect $ Ref.new Nothing
  pure
    { speak: case _ of
      Nothing -> pure unit -- TODO really?
      Just (Update (Changed _ s)) -> launchAff_ do
        mFiber <- liftEffect $ Ref.read mInputFiberRef
        for_ mFiber $ killFiber (error "Obsolete input")
        newFiber <- forkAff do
          a <- pre s
          liftEffect $ speak $ Just $ Update $ Changed [] a
        liftEffect $ Ref.write (Just newFiber) mInputFiberRef
      _ -> pure unit -- TODO really?
    , listen: \prop -> do
      listen \(Changed _ b) -> launchAff_ do
        mFiber <- liftEffect $ Ref.read mOutputFiberRef
        for_ mFiber $ killFiber (error "Obsolete output")
        newFiber <- forkAff do
          t <- post b
          liftEffect $ prop $ Changed [] t
        liftEffect $ Ref.write (Just newFiber) mOutputFiberRef
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
    zoomOut :: Changed b -> Changed b
    zoomOut (Changed scopes mb) = Changed (scope : scopes) mb

    zoomIn :: Maybe (Change a) -> Maybe (Change a)
    zoomIn Nothing = Nothing
    zoomIn (Just Removal) = Just Removal
    zoomIn (Just (Update (Changed scopes ma))) = case uncons scopes of
      Just { head, tail } | head == scope -> Just $ Update $ Changed tail ma
      Nothing -> Just $ Update $ Changed [] ma
      Just { head: Variant _ } -> Just $ Update $ Changed [] ma -- not matching head but head is twist
      _ -> case scope of
        Variant _ -> Just $ Update $ Changed [] ma -- not matching head but scope is twist
        _ -> Nothing -- otherwise

