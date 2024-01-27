module Widget
  ( Change(..)
  , WidgetOptics
  , WidgetOptics'
  , Widget(..)
  , Scope(..)
  , bracket
  , constructor
  , field
  , fixed
  , iso
  , lens
  , preview
  , prism
  , projection
  , view
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
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Prim.Row as Row
import Record (get, set)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

newtype Widget m i o = Widget (m
  { speak :: Change i -> m Unit
  , listen :: (Change o -> m Unit) -> m Unit
  })

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

preview :: forall m i o. Monad m => Widget m i o -> m Unit
preview p = do
  { speak, listen } <- unwrap p
  listen (const $ pure unit)
  speak None

view :: forall m i o. Monad m => Widget m i o -> i -> m Unit
view p i = do
  { speak, listen } <- unwrap p
  listen (const $ pure unit)
  speak (Update [] i)

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
      { speak: \chab -> do
        case chab of
          None -> pure unit
          Removal -> p'.speak Removal
          Update _ ab -> do
            let _ = unsafePerformEffect $ Ref.write ab lastab
            p'.speak $ map fst chab
      , listen: \propagationab -> do
        p'.listen \cha -> do
          let prevab = unsafePerformEffect $ Ref.read lastab
          propagationab (map (flip Tuple (snd prevab)) cha)
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
      , listen: \propagationab -> do
        p'.listen \cha -> do
          let prevab = unsafePerformEffect $ Ref.read lastab
          propagationab (map (Tuple (fst prevab)) cha)
      }

instance Applicative m => Choice (Widget m) where
  left p = wrap ado
    p' <- unwrap p
    in
      { speak: \chab -> do
        case chab of
          None -> pure unit
          Removal -> p'.speak Removal
          Update _ (Right _) -> p'.speak Removal
          Update _ (Left a) -> p'.speak $ chab $> a
      , listen: \propagationab -> do
        p'.listen \cha -> do propagationab (Left <$> cha)
      }
  right p = wrap ado
    p' <- unwrap p
    in
      { speak: \chab -> do
        case chab of
          None -> pure unit
          Removal -> p'.speak Removal
          Update _ (Left _) -> p'.speak Removal
          Update _ (Right a) -> p'.speak $ chab $> a
      , listen: \propagationab -> do
        p'.listen \cha -> propagationab (Right <$> cha)
      }

instance Apply m => Semigroup (Widget m a a) where
  append p1 p2 = wrap ado
    p1' <- unwrap p1
    p2' <- unwrap p2
    in
      { speak: \o -> ado
        p1'.speak o
        p2'.speak o
        in unit
      , listen: \propagation -> ado
        p1'.listen \o -> ado
          p2'.speak o
          propagation o
          in unit
        p2'.listen \o -> ado
          p1'.speak o
          propagation o
          in unit
        in unit
      }

instance Monad m => Semigroupoid (Widget m) where
  compose p2 p1 = wrap do
    p1' <- unwrap p1
    p2' <- unwrap p2
    p1'.listen \o -> p2'.speak o -- TODO what does it mean if o is Nothing?
    pure
      { speak: p1'.speak -- TODO call p2.speak Nothing?
      , listen: p2'.listen
      }

-- impossible:
-- instance Monad m => Category (Widget m) where
--   identity = wrap $ pure
--     { speak: unsafeThrow "impossible"
--     , listen: unsafeThrow "impossible"
--     }

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
      { speak: p1'.speak *> p2'.speak
      , listen: \propagation -> ado
        p1'.listen propagation
        p2'.listen propagation
        in unit
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
prism name construct deconstruct = Profunctor.prism construct deconstruct >>> scopemap (Variant name) -- TODO not sure about it

constructor :: forall a s. String -> (a -> s) -> (s -> Maybe a) -> WidgetOptics' a s
constructor name construct deconstruct = scopemap (Part name) >>> left >>> dimap (\s -> maybe (Right s) Left (deconstruct s)) (either construct identity)

-- TODO this is not really optics
bracket :: forall m c i o i' o'. Monad m => m c -> (c -> Change i' -> m (Change i)) -> (c -> Change o -> m (Change o')) -> Widget m i o -> Widget m i' o'
bracket afterInit afterInward beforeOutward w = wrap ado
  w' <- unwrap w
  ctx <- afterInit
  in
    { speak: \occur -> do
      occur' <- afterInward ctx occur
      w'.speak occur'
    , listen: \prop -> do
      w'.listen \occur -> do
        occur' <- beforeOutward ctx occur
        prop occur'
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
