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
import Data.Foldable (for_)
import Data.Generic.Rep (class Generic)
import Data.Lens as Profunctor
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (class Profunctor, dimap, lcmap)
import Data.Profunctor.Choice (class Choice, left)
import Data.Profunctor.Strong (class Strong, first)
import Data.Show.Generic (genericShow)
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

data Change a = Some (Array Scope) (Maybe a) | None

derive instance Functor Change

data Scope = Part String | Variant String

derive instance Generic Scope _

instance Show Scope where
  show = genericShow

derive instance Eq Scope

preview :: forall m i o. Monad m => Widget m i o -> m Unit
preview p = do
  { speak, listen } <- unwrap p
  listen (const $ pure unit)
  speak (Some [] Nothing)

view :: forall m i o. Monad m => Widget m i o -> i -> m Unit
view p i = do
  { speak, listen } <- unwrap p
  listen (const $ pure unit)
  speak (Some [] (Just i))

instance Functor m => Profunctor (Widget m) where
  dimap contraf cof p = wrap $ unwrap p <#> \p' ->
    { speak: (_ <<< map contraf) p'.speak
    , listen: p'.listen <<< lcmap (map cof)
    }

instance Applicative m => Strong (Widget m) where
  first p = wrap ado
    let lastchab = unsafePerformEffect $ Ref.new (unsafeCoerce unit)
    p' <- unwrap p
    in
      { speak: \chab -> do
        case chab of
          None -> pure unit -- should never happen
          Some _ mab -> do
            let _ = unsafePerformEffect $ Ref.write mab lastchab
            p'.speak (map fst chab)
      , listen: \propagationab -> do
        p'.listen \cha -> do
          let prevchab = unsafePerformEffect $ Ref.read lastchab
          for_ prevchab \prevab -> propagationab (map (flip Tuple (snd prevab)) cha)
      }
  second p = wrap ado
    let lastchab = unsafePerformEffect $ Ref.new (unsafeCoerce unit)
    p' <- unwrap p
    in
      { speak: \chab -> do
        case chab of
          None -> pure unit -- should never happen
          Some _ mab -> do
            let _ = unsafePerformEffect $ Ref.write mab lastchab
            p'.speak (map snd chab)
      , listen: \propagationab -> do
        p'.listen \cha -> do
          let prevchab = unsafePerformEffect $ Ref.read lastchab
          for_ prevchab \prevab -> propagationab (map (Tuple (fst prevab)) cha)
      }

instance Applicative m => Choice (Widget m) where
  left p = wrap ado
    let lastomab = unsafePerformEffect $ Ref.new (unsafeCoerce unit)
    p' <- unwrap p
    in
      { speak: \chab -> do
        let _ = unsafePerformEffect $ Ref.write chab lastomab
        case chab of
          None -> pure unit
          Some s Nothing -> p'.speak $ Some s Nothing
          Some s (Just (Right _)) -> p'.speak $ Some s Nothing
          Some s (Just (Left a)) -> p'.speak $ Some s $ Just a
      , listen: \propagationab -> do
        p'.listen \cha -> do -- should never happen
          -- prevoab <- liftST $ ST.read lastomab -- TODO what to do with previous occurence? it could contain info about the change of a or b
          propagationab (Left <$> cha)
      }
  right p = wrap ado
    let lastomab = unsafePerformEffect $ Ref.new (unsafeCoerce unit)
    p' <- unwrap p
    in
      { speak: \omab -> do
        let _ = unsafePerformEffect $ Ref.write omab lastomab
        case omab of
          None -> pure unit
          Some s Nothing -> p'.speak $ Some s Nothing
          Some s (Just (Left _)) -> p'.speak $ Some s Nothing
          Some s (Just (Right a)) -> p'.speak $ Some s $ Just a
      , listen: \propagationab -> do
        p'.listen \oa -> do -- should never happen
          -- prevoab <- liftST $ ST.read lastomab -- TODO what to do with previous occurence? it could contain info about the change of a or b
          propagationab (Right <$> oa)
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
  w'.speak (Some [] (Just a))
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

field :: forall @l s r a . IsSymbol l => Row.Cons l a r s => WidgetOptics' a (Record s)
field = field' (reflectSymbol (Proxy @l)) (flip (set (Proxy @l))) (get (Proxy @l))
  where
    field' name setter getter = scopemap (Part name) >>> first >>> dimap (\s -> Tuple (getter s) s) (\(Tuple a s) -> setter s a)

prism :: forall a b s t. String -> (b -> t) -> (s -> Either t a) -> WidgetOptics a b s t
prism name construct deconstruct = Profunctor.prism construct deconstruct >>> scopemap (Variant name) -- TODO not sure about it

constructor :: forall a s. String -> (a -> s) -> (s -> Maybe a) -> WidgetOptics' a s
constructor name construct deconstruct = left >>> dimap (\s -> maybe (Right s) Left (deconstruct s)) (either construct identity) >>> scopemap (Part name)

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
    zoomOut (Some scopes mb) = Some (scope : scopes) mb
    zoomOut None = None

    zoomIn :: Change a -> Change a
    zoomIn (Some scopes ma) = case uncons scopes of
      Just { head, tail } | head == scope -> Some tail ma
      Nothing -> Some [] ma
      Just { head: Variant _ } -> Some [] ma -- not matching head but head is twist
      _ -> case scope of
        Variant _ -> Some [] ma -- not matching head but scope is twist
        _ -> None -- otherwise
    zoomIn None = None
