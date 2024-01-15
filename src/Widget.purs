module Widget
  ( Change(..)
  , Occurrence(..)
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
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (class Profunctor, dimap, lcmap)
import Data.Profunctor.Choice (class Choice, left)
import Data.Profunctor.Strong (class Strong, first)
import Data.Show.Generic (genericShow)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Class (class MonadEffect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Prim.Row as Row
import Record (get, set)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

newtype Widget m i o = Widget (m
  { speak :: Occurrence (Maybe i) -> m Unit
  , listen :: (Occurrence (Maybe o) -> m Unit) -> m Unit
  })

derive instance Newtype (Widget m i o) _

data Occurrence a = Occurrence Change a

data Change = Some | Scoped (NonEmptyArray.NonEmptyArray Scope) | None -- TODO: find already existing data type for it

derive instance Generic Change _
instance Show Change where
  show = genericShow

data Scope = Part String | Variant String

derive instance Generic Scope _
instance Show Scope where
  show = genericShow

derive instance Functor Occurrence

instance Semigroup Change where -- finds least common scope
  append None s = s
  append s None = s
  append Some _ = Some
  append _ Some = Some
  append (Scoped hops1) (Scoped hops2) = case NonEmptyArray.fromArray $ commonPrefix hops1 hops2 of
    Nothing -> Some -- no common prefix
    Just prefix -> Scoped prefix -- common prefix
    where
      commonPrefix :: forall a. Eq a => NonEmptyArray.NonEmptyArray a -> NonEmptyArray.NonEmptyArray a -> Array a
      commonPrefix a1 a2 = let
        {head: h1, tail: t1} = NonEmptyArray.uncons a1
        {head: h2, tail: t2} = NonEmptyArray.uncons a2
        in if h1 == h2 then h1:fromMaybe [] (commonPrefix <$> NonEmptyArray.fromArray t1 <*> NonEmptyArray.fromArray t2)
        else []

instance Monoid Change where
  mempty = None

derive instance Eq Scope

preview :: forall m i o. Monad m => Widget m i o -> m Unit
preview p = do
  { speak, listen } <- unwrap p
  listen (const $ pure unit)
  speak (Occurrence Some Nothing)

view :: forall m i o. Monad m => Widget m i o -> i -> m Unit
view p i = do
  { speak, listen } <- unwrap p
  listen (const $ pure unit)
  speak (Occurrence Some (Just i))

instance Functor m => Profunctor (Widget m) where
  dimap contraf cof p = wrap $ unwrap p <#> \p' ->
    { speak: (_ <<< map (map contraf)) $ p'.speak
    , listen: p'.listen <<< lcmap (map (map cof))
    }

instance Applicative m => Strong (Widget m) where
  first p = wrap ado
    let lastomab = unsafePerformEffect $ Ref.new (unsafeCoerce unit)
    p' <- unwrap p
    in
      { speak: \omab -> do
        let _ = unsafePerformEffect $ Ref.write omab lastomab
        case omab of
          Occurrence None _ -> pure unit -- should never happen
          _ -> p'.speak (map (map fst) omab)
      , listen: \propagationab -> do
        p'.listen \oa -> do
          let (Occurrence _ prevmab) = unsafePerformEffect $ Ref.read lastomab
          for_ prevmab \prevab -> propagationab (map (map (flip Tuple (snd prevab))) oa)
      }
  second p = wrap ado
    let lastomab = unsafePerformEffect $ Ref.new (unsafeCoerce unit)
    p' <- unwrap p
    in
      { speak: \omab -> do
        let _ = unsafePerformEffect $ Ref.write omab lastomab
        case omab of
          Occurrence None _ -> pure unit -- should never happen
          _ -> p'.speak (map (map snd) omab)
      , listen: \propagationab -> do
        p'.listen \oa -> do
          let (Occurrence _ prevmab) = unsafePerformEffect $ Ref.read lastomab
          for_ prevmab \prevab -> propagationab (map (map (Tuple (fst prevab))) oa )
      }

instance Applicative m => Choice (Widget m) where
  left p = wrap ado
    let lastomab = unsafePerformEffect $ Ref.new (unsafeCoerce unit)
    p' <- unwrap p
    in
      { speak: \omab -> do
        let _ = unsafePerformEffect $ Ref.write omab lastomab
        case omab of
          Occurrence None _ -> pure unit
          Occurrence _ Nothing -> p'.speak $ omab $> Nothing
          Occurrence _ (Just (Right _)) -> p'.speak $ omab $> Nothing
          Occurrence _ (Just (Left a)) -> p'.speak $ omab $> Just a
      , listen: \propagationab -> do
        p'.listen \oa -> do -- should never happen
          -- prevoab <- liftST $ ST.read lastomab -- TODO what to do with previous occurence? it could contain info about the change of a or b
          propagationab (map Left <$> oa)
      }
  right p = wrap ado
    let lastomab = unsafePerformEffect $ Ref.new (unsafeCoerce unit)
    p' <- unwrap p
    in
      { speak: \omab -> do
        let _ = unsafePerformEffect $ Ref.write omab lastomab
        case omab of
          Occurrence None _ -> pure unit
          Occurrence _ Nothing -> p'.speak $ omab $> Nothing
          Occurrence _ (Just (Left _)) -> p'.speak $ omab $> Nothing
          Occurrence _ (Just (Right a)) -> p'.speak $ omab $> Just a
      , listen: \propagationab -> do
        p'.listen \oa -> do -- should never happen
          -- prevoab <- liftST $ ST.read lastomab -- TODO what to do with previous occurence? it could contain info about the change of a or b
          propagationab (map Right <$> oa)
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
-- compare to: instance MonadEffect m => Semigroup (Widget m a a) where

instance Monad m => Semigroupoid (Widget m) where
  compose p2 p1 = wrap do
    p1' <- unwrap p1
    p2' <- unwrap p2
    p1'.listen \o -> p2'.speak o -- TODO what does it mean if o is Nothing?
    pure
      { speak: p1'.speak -- TODO call p2.speak Nothing?
      , listen: p2'.listen
      }
-- compare to: instance MonadEffect m => Semigroupoid (Widget m) where

-- impossible:
-- instance Monad m => Category (Widget m) where
--   identity = wrap $ pure
--     { speak: unsafeThrow "impossible"
--     , listen: unsafeThrow "impossible"
--     }

instance Functor m => Functor (Widget m a) where
  map f p = wrap $ unwrap p <#> \p' ->
    { speak: p'.speak
    , listen: p'.listen <<< lcmap (map (map f))
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

bracket :: forall m c i o i' o'. MonadEffect m => m c -> (c -> Occurrence (Maybe i') -> m (Occurrence (Maybe i))) -> (c -> Occurrence (Maybe o) -> m (Occurrence (Maybe o'))) -> Widget m i o -> Widget m i' o'
bracket afterInit afterInward beforeOutward w = wrap do
  w' <- unwrap w
  ctx <- afterInit
  pure
    { speak: \occur -> do
      occur' <- afterInward ctx occur
      w'.speak occur'
    , listen: \prop -> do
      w'.listen \occur -> do
        occur' <- beforeOutward ctx occur
        prop occur'
      }

fixed :: forall m a b s t. MonadEffect m => a -> Widget m a b -> Widget m s t
fixed a w = wrap do
  w' <- unwrap w
  w'.speak (Occurrence Some (Just a))
  pure
    { speak: const $ pure unit
    , listen: const $ pure unit
    }

scopemap :: forall m a b. Applicative m => Scope -> Widget m a b -> Widget m a b
scopemap scope p = wrap ado
  { speak, listen } <- unwrap p
  in
    { speak: \(Occurrence c a) -> speak $ Occurrence (zoomOut c) a
    , listen: \prop -> do
      listen \(Occurrence c a) -> prop $ Occurrence (zoomIn c) a
    }
  where
    zoomOut :: Change -> Change
    zoomOut Some = Scoped (scope `NonEmptyArray.cons'` [])
    zoomOut (Scoped scopes) = Scoped (scope `NonEmptyArray.cons` scopes)
    zoomOut None = None

    zoomIn :: Change -> Change
    zoomIn Some = Some
    zoomIn (Scoped scopes) = case NonEmptyArray.uncons scopes of
      { head, tail } | head == scope -> case uncons tail of -- matching head
        Just { head: headOtTail, tail: tailOfTail } -> Scoped $ NonEmptyArray.cons' headOtTail tailOfTail -- non empty tail
        Nothing -> Some -- empty tail
      { head: Variant _ } -> Some -- not matching head but head is twist
      _ -> case scope of
        Variant _ -> Some -- not matching head but scope is twist
        _ -> None -- otherwise
    zoomIn None = None

-- optics

type WidgetOptics a b s t = forall m. Applicative m => Widget m a b -> Widget m s t
type WidgetOptics' a s = forall m. Applicative m => Widget m a a -> Widget m s s

iso :: forall a s. String -> (s -> a) -> (a -> s) -> WidgetOptics' a s
iso name mapin mapout = dimap mapin mapout >>> scopemap (Variant name)

projection :: forall a b s. (s -> a) -> WidgetOptics a b s b
projection f = dimap f identity

lens :: forall a b s t. String -> (s -> a) -> (s -> b -> t) -> WidgetOptics a b s t
lens name getter setter = first >>> dimap (\s -> Tuple (getter s) s) (\(Tuple b s) -> setter s b) >>> scopemap (Variant name)

field :: forall @l s r a . IsSymbol l => Row.Cons l a r s => WidgetOptics' a (Record s)
field = field' (reflectSymbol (Proxy @l)) (flip (set (Proxy @l))) (get (Proxy @l))
  where
    field' name setter getter = scopemap (Part name) >>> first >>> dimap (\s -> Tuple (getter s) s) (\(Tuple a s) -> setter s a)

prism :: forall a b s t. String -> (b -> t) -> (s -> Either a t) -> WidgetOptics a b s t
prism name construct deconstruct = left >>> dimap deconstruct (either construct identity) >>> scopemap (Variant name) -- TODO not sure about it

constructor :: forall a s. String -> (a -> s) -> (s -> Maybe a) -> WidgetOptics' a s
constructor name construct deconstruct = left >>> dimap (\s -> maybe (Right s) Left (deconstruct s)) (either construct identity) >>> scopemap (Part name)
