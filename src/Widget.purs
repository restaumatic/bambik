module Widget
  ( Change(..)
  , Propagation
  , Scope(..)
  , Widget(..)
  , WidgetOptics
  , WidgetOptics'
  , bracket
  , constructor
  , effect
  , field
  , fixed
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
import Data.Tuple (Tuple(..), fst, snd)
import Effect.AVar as AVar
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Prim.Row as Row
import Record (get, set)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

newtype Widget m i o = Widget (m
  { speak :: Propagation m i
  , listen :: Propagation m o -> m Unit
  })

-- things that can `ReaderT r m` can enable:
--   UI-independent:
--     platform
--     app metadata e.g verion
--     feature flags
--     logging
--     monitoring
--     analytics
--     authorization
--     current time
--     localization
--     user context 
--   UI-specific:
--     scheduleNotification :: NotificationOptions -> Effect Unit
--     updateNotification :: NotificationOptions -> Effect Unit
--     clearNotification :: NotificationId -> Effect Unit
--     onClickNotification :: (NotificationOptions -> Effect Unit) -> Effect Unit
--     device permissions - checking/fixing
--     sounds (or rather it should be widget?)
--     onPushNotification :: (PushMessage -> Effect Unit) -> Effect Unit
--     printing
--     media type
--     sounds

type Propagation m a = Change a -> m Unit

derive instance Newtype (Widget m i o) _

data Change a
  = Update (Array Scope) a
  | Removal
  | None

derive instance Functor Change

instance Show (Change a) where
  show = case _ of
    None -> "None"
    Removal -> "Removal"
    Update scopes _ -> "Update " <> joinWith "." (show <$> scopes)

data Scope = Part String | Variant String

derive instance Generic Scope _

instance Show Scope where
  show = genericShow

derive instance Eq Scope

instance Functor m => Profunctor (Widget m) where
  dimap contraf cof p = wrap $ unwrap p <#> \p' ->
    { speak: (_ <<< map contraf) p'.speak
    , listen: p'.listen <<< lcmap (map cof)
    }

instance Applicative m => Strong (Widget m) where
  first p = wrap ado
    let lastab = unsafePerformEffect $ Ref.new (unsafeCoerce unit)
    p' <- unwrap p
    in
      { speak: \ch -> do
        case ch of
          None -> pure unit
          Removal -> p'.speak Removal
          Update _ ab -> do
            let _ = unsafePerformEffect $ Ref.write ab lastab
            p'.speak $ map fst ch
      , listen: \prop -> do
        p'.listen \ch -> do
          let prevab = unsafePerformEffect $ Ref.read lastab
          prop (map (flip Tuple (snd prevab)) ch)
      }
  second p = wrap ado
    let lastab = unsafePerformEffect $ Ref.new (unsafeCoerce unit)
    p' <- unwrap p
    in
      { speak: \chab -> do
        case chab of
          None -> pure unit
          Removal -> p'.speak Removal
          Update _ ab -> do
            let _ = unsafePerformEffect $ Ref.write ab lastab
            p'.speak $ map snd chab
      , listen: \prop -> do
        p'.listen \ch -> do
          let prevab = unsafePerformEffect $ Ref.read lastab
          prop (map (Tuple (fst prevab)) ch)
      }

instance Applicative m => Choice (Widget m) where
  left p = wrap ado
    p' <- unwrap p
    in
      { speak: \ch -> do
        case ch of
          None -> pure unit
          Removal -> p'.speak Removal
          Update _ (Right _) -> p'.speak Removal
          Update _ (Left a) -> p'.speak $ ch $> a
      , listen: \prop -> do
        p'.listen \ch -> prop (Left <$> ch)
      }
  right p = wrap ado
    p' <- unwrap p
    in
      { speak: \ch -> do
        case ch of
          None -> pure unit
          Removal -> p'.speak Removal
          Update _ (Left _) -> p'.speak Removal
          Update _ (Right a) -> p'.speak $ ch $> a
      , listen: \prop -> do
        p'.listen \ch -> prop (Right <$> ch)
      }

instance Apply m => Semigroup (Widget m a a) where
  append p1 p2 = wrap ado
    p1' <- unwrap p1
    p2' <- unwrap p2
    in
      { speak: \ch -> p1'.speak ch *> p2'.speak ch
      , listen: \prop -> (p1'.listen \ch -> p2'.speak ch *> prop ch) *> (p2'.listen \ch -> p1'.speak ch *> prop ch)
      }
-- Notice: optic `Widget m c d -> Widget m a a` is also a Semigroup

instance Monad m => Semigroupoid (Widget m) where
  compose p2 p1 = wrap do
    p1' <- unwrap p1
    p2' <- unwrap p2
    p1'.listen \ch -> p2'.speak ch
    pure
      { speak: p1'.speak
      , listen: p2'.listen
      }
-- Notice: optic `Widget m c d -> Widget m a a` is also a Monoid

-- impossible:
-- instance Monad m => Category (Widget m) where
--   identity = wrap $ pure
--     { speak: unsafeThrow "impossible"
--     , listen: unsafeThrow "impossible"
--     }
-- but:
instance MonadEffect m => Category (Widget m) where
  identity = wrap do
    chaAVar <- liftEffect AVar.empty
    pure
      { speak: \cha -> liftEffect $ void $ AVar.put cha chaAVar mempty
      , listen: \prop ->
        let waitAndPropagate = void $ AVar.take chaAVar case _ of
              Left error -> pure unit -- TODO handle error
              Right cha -> waitAndPropagate -- TODO propagate
        in liftEffect waitAndPropagate
      }
-- is maybe possible?

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

type WidgetOptics a b s t = forall m. Monad m => Widget m a b -> Widget m s t
type WidgetOptics' a s = forall m. Monad m => Widget m a a -> Widget m s s

fixed :: forall a b s t. a -> WidgetOptics a b s t
fixed a w = wrap do
  w' <- unwrap w
  w'.speak (Update [] a)
  pure
    { speak: const $ pure unit
    , listen: const $ pure unit
    }

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
prism name construct deconstruct = Profunctor.prism construct deconstruct >>> scopemap (Variant name)

constructor :: forall a s. String -> (a -> s) -> (s -> Maybe a) -> WidgetOptics' a s
constructor name construct deconstruct = scopemap (Part name) >>> left >>> dimap (\s -> maybe (Right s) Left (deconstruct s)) (either construct identity)

-- modifiers

-- notice: this is not really optics, operates for given m
-- TODO add release parameter?
bracket :: forall a b c m. Applicative m => m c -> (c -> m Unit) -> (c -> m Unit) -> Widget m a b -> Widget m a b
bracket afterInit afterInward beforeOutward w = wrap ado
  w' <- unwrap w
  ctx <- afterInit
  in
    { speak: \cha -> w'.speak cha <* afterInward ctx
    , listen: \prop -> do
      w'.listen \chb -> beforeOutward ctx *> prop chb
    }

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
    zoomOut None = None
    zoomOut Removal = Removal
    zoomOut (Update scopes mb) = Update (scope : scopes) mb

    zoomIn :: Change a -> Change a
    zoomIn (Update scopes ma) = case uncons scopes of
      Just { head, tail } | head == scope -> Update tail ma
      Nothing -> Update [] ma
      Just { head: Variant _ } -> Update [] ma -- not matching head but head is twist
      _ -> case scope of
        Variant _ -> Update [] ma -- not matching head but scope is twist
        _ -> None -- otherwise
    zoomIn None = None
    zoomIn Removal = Removal

effect :: forall req res m. MonadEffect m => (req -> m res) -> Widget m req res
effect processRequest = Widget $ liftEffect do
  resAVar <- AVar.empty
  pure
    { speak: case _ of
      Update _ req -> do
        res <- processRequest req
        void $ liftEffect $ AVar.put res resAVar mempty
      _-> pure unit
    , listen: \prop ->
      let waitAndPropagate = void $ AVar.take resAVar case _ of
            Left error -> pure unit -- TODO handle error
            Right res -> do
              log $ show $ Update [] res
              -- TODO propagate $ Update [] res
              waitAndPropagate
       in liftEffect waitAndPropagate
    }
