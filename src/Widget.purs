module Widget
  ( Changed(..)
  , New(..)
  , Scope(..)
  , Widget(..)
  , WidgetOcular
  , WidgetOptics
  , WidgetOptics'
  , WidgetStatic
  , action
  , action'
  , adapter
  , affAdapter
  , constructor
  , debouncer
  , debouncer'
  , devoid
  , effAdapter
  , effBracket
  , field
  , iso
  , just
  , lens
  , prism
  , projection
  , spy
  , static
  , value
  )
  where

import Prelude

import Data.Array (fold, null, uncons, (:))
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Lens (Optic, Optic')
import Data.Lens as Profunctor
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (class Profunctor, dimap, lcmap)
import Data.Profunctor.Choice (class Choice, left)
import Data.Profunctor.Strong (class Strong)
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
import Ocular (Ocular, Static)
import Prim.Row as Row
import Record (get, set)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

newtype Widget m i o = Widget (m
  { toUser :: Changed i -> Effect Unit
  , fromUser :: (Changed o -> Effect Unit) -> Effect Unit
  })

derive instance Newtype (Widget m i o) _

data Changed a
  = Altered (New a)
  | Removed -- TODO rename to: none?

derive instance Functor Changed

data New a = New (Array Scope) a Boolean

derive instance Functor New

data Scope = Part String | Variant String

instance Show Scope where
  show = case _ of
    Part s -> "." <> s
    Variant s -> "?" <> s

derive instance Eq Scope

instance Functor m => Profunctor (Widget m) where
  dimap contraf cof p = wrap $ unwrap p <#> \p' ->
    { toUser: (_ <<< map contraf) p'.toUser
    , fromUser: p'.fromUser <<< lcmap (map cof)
    }

instance Applicative m => Strong (Widget m) where
  first p = wrap ado
    let lastab = unsafePerformEffect $ Ref.new (unsafeCoerce unit)
    p' <- unwrap p
    in
      { toUser: case _ of
          Removed -> p'.toUser Removed
          Altered (New scope ab cont) -> do
            let _ = unsafePerformEffect $ Ref.write ab lastab
            p'.toUser $ Altered $ New scope (fst ab) cont
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
          Removed -> p'.toUser Removed
          Altered (New scope ab cont) -> do
            let _ = unsafePerformEffect $ Ref.write ab lastab
            p'.toUser $ Altered $ New scope (snd ab) cont
      , fromUser: \prop -> do
        p'.fromUser \u -> do
          let prevab = unsafePerformEffect $ Ref.read lastab
          prop (map (Tuple (fst prevab)) u)
      }

instance Applicative m => Choice (Widget m) where
  left p = wrap ado
    p' <- unwrap p
    in
      { toUser: case _ of
        Removed -> p'.toUser Removed
        Altered (New _ (Right _) _) -> p'.toUser Removed
        Altered (New scope (Left a) cont) -> p'.toUser $ Altered $ New scope a cont
      , fromUser: \prop -> do
        p'.fromUser \u -> prop (Left <$> u)
      }
  right p = wrap ado
    p' <- unwrap p
    in
      { toUser: case _ of
        Removed -> p'.toUser Removed
        Altered (New _ (Left _) _) -> p'.toUser Removed
        Altered (New scope (Right a) cont) -> p'.toUser $ Altered $ New scope a cont
      , fromUser: \prop -> do
        p'.fromUser \u -> prop (Right <$> u)
      }

instance MonadEffect m => Semigroupoid (Widget m) where
  compose p2 p1 = wrap do
    p1' <- unwrap p1
    p2' <- unwrap p2
    pure
      { toUser: \cha -> do
        p2'.toUser Removed
        p1'.toUser cha
      , fromUser: \prop -> do
          p1'.fromUser p2'.toUser
          p2'.fromUser prop
      }

-- Notice: Widget is not a category
-- instance MonadEffect m => Category (Widget m) where
--   identity = wrap do
--     chaAVar <- liftEffect AVar.empty
--     pure
--       { toUser: \cha -> void $ AVar.put cha chaAVar mempty
--       , fromUser: \prop ->
--         let waitAndPropagate = void $ AVar.take chaAVar case _ of
--               Left error -> pure unit -- handle error
--               Right Removed -> ... -- impossible to propagate `Removed`
--               Right (Altered newa) -> do
--                 prop newa
--                 waitAndPropagate
--         in waitAndPropagate
--       }

instance Apply m => Semigroup (Widget m a a) where
  append p1 p2 = wrap ado
    p1' <- unwrap p1
    p2' <- unwrap p2
    in
      { toUser: \ch -> p1'.toUser ch *> p2'.toUser ch
      , fromUser: \prop -> (p1'.fromUser \u -> p2'.toUser u *> prop u) *> (p2'.fromUser \u -> p1'.toUser u *> prop u)
      }
-- Notice: optic `WidgetOptic m a b c c` is also a Semigroup

instance Applicative m => Monoid (Widget m a a) where
  mempty = devoid
-- Notice: optic `WidgetOptic m a b c c` is also a Monoid

-- a >>> devoid -- has an effect of `a` but stops propagation
-- a <> devoid == a == devoid <> a
devoid :: forall m . Applicative m => WidgetStatic m
devoid = wrap $ pure
  { toUser: mempty
  , fromUser: mempty
  }

-- optics

type WidgetOptics a b s t = forall m. MonadEffect m => Optic (Widget m) s t a b
type WidgetOptics' a s = forall m. MonadEffect m => Optic' (Widget m) s a

static :: forall a s t. a -> WidgetOptics a Void s t
static a w = wrap do
  w' <- unwrap w
  liftEffect $ w'.toUser $ Altered $ New [] a false
  pure
    { toUser: const $ pure unit
    , fromUser: const $ pure unit
    }

value :: forall a b. WidgetOptics a Void a b
value w = w >>> devoid

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

just :: forall a. WidgetOptics' a (Maybe a)
just = constructor "Just" Just identity

-- oculars

type WidgetOcular m = Ocular (Widget m)
type WidgetStatic m = Static (Widget m)

debouncer :: forall m. MonadEffect m => Milliseconds -> WidgetOcular m
debouncer millis = affAdapter $ pure
  { pre: case _ of
    (New _ i true) -> delay millis *> pure i
    (New _ i false) -> pure i
  , post: \(New _ i _) -> pure i
  }

debouncer' :: forall m. MonadEffect m => WidgetOcular m
debouncer' = debouncer (Milliseconds 300.0)

spy :: forall m. MonadEffect m => String -> WidgetOcular m
spy name w = wrap do
  { toUser, fromUser } <- unwrap w
  pure
    -- TODO use generic show
    -- { toUser: \ch -> log' ("< " <> show ch) *> toUser ch *> log' ">"
    { toUser: \ch -> log' "< " *> toUser ch *> log' ">"
    , fromUser: \prop -> do
      -- fromUser \u -> log' ("> " <> show u) *> prop u *> log' "<"
      fromUser \u -> log' "> " *> prop u *> log' "<"
    }
  where
    log' s = log $ "[WidgetSpy] " <> name <> " " <> s

-- modifiers

effBracket :: forall m a b. Monad m => m
  { beforeInput :: Changed a -> Effect Unit
  , afterInput :: Changed a -> Effect Unit
  , beforeOutput :: Changed b -> Effect Unit
  , afterOutput :: Changed b -> Effect Unit
  } -> Widget m a b -> Widget m a b
effBracket f w = wrap do
  { toUser, fromUser } <- unwrap w
  { beforeInput, afterInput, beforeOutput, afterOutput } <- f
  pure
    { toUser: \ch -> beforeInput ch *> toUser ch *> afterInput ch
    , fromUser: \prop -> do
      fromUser \u -> beforeOutput u *> prop u *> afterOutput u
    }

-- notice: this is not really optics, operates for given m
-- TODO add release parameter?
effAdapter :: forall m a b s t. Monad m => m { pre :: s -> Effect a, post ::  b -> Effect t} -> Widget m a b -> Widget m s t
effAdapter f w = wrap do
  { toUser, fromUser } <- unwrap w
  { pre, post } <- f
  pure
    { toUser: case _ of
      Removed -> toUser Removed
      Altered (New _ s cont) -> do
        a <- pre s
        toUser $ Altered $ New [] a cont
    , fromUser: \prop -> do
      fromUser case _ of
        Altered (New _ b cont) -> do
          t <- post b
          prop $ Altered $ New [] t cont
        Removed -> prop Removed
    }

action :: forall a i o. (i -> Aff o) -> WidgetOptics Boolean a i o
action arr = action' \i pro post -> do
  liftEffect $ pro true
  o <- arr i
  liftEffect $ pro false
  liftEffect $ post o

action' :: forall a b i o. (i -> (a -> Effect Unit) -> (o -> Effect Unit) -> Aff Unit) -> WidgetOptics a b i o
action' arr w = wrap do
  oVar <- liftEffect AVar.empty
  w' <- unwrap w
  pure
    { toUser: case _ of
      Removed -> w'.toUser Removed
      Altered (New _ i cont) -> do
        launchAff_ $ arr i (\a -> w'.toUser $ Altered $ New [] a cont) (\o -> void $ AVar.put o oVar mempty)
    , fromUser: \prop ->
      let waitAndPropagate = void $ AVar.take oVar case _ of
            Left error -> pure unit -- TODO handle error
            Right o -> do
              -- w'.toUser $ Altered $ New [] Nothing false
              prop $ Altered $ New [] o false -- TODO really?
              waitAndPropagate
      in waitAndPropagate
    }

affAdapter :: forall m a b s t. MonadEffect m => m { pre :: New s -> Aff a, post ::  New b -> Aff t} -> Widget m a b -> Widget m s t
affAdapter f w = wrap do
  { toUser, fromUser } <- unwrap w
  { pre, post } <- f
  mInputFiberRef <- liftEffect $ Ref.new Nothing
  mOutputFiberRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: case _ of
      Removed -> pure unit -- TODO really?
      Altered news@(New _ _ cont) -> launchAff_ do
        mFiber <- liftEffect $ Ref.read mInputFiberRef
        for_ mFiber $ killFiber (error "Obsolete input")
        newFiber <- forkAff do
          a <- pre news
          liftEffect $ toUser $ Altered $ New [] a cont
        liftEffect $ Ref.write (Just newFiber) mInputFiberRef
    , fromUser: \prop -> do
      fromUser case _ of
        Altered newb@(New _ _ cont) -> launchAff_ do
          mFiber <- liftEffect $ Ref.read mOutputFiberRef
          for_ mFiber $ killFiber (error "Obsolete output")
          newFiber <- forkAff do
            t <- post newb
            liftEffect $ prop $ Altered $ New [] t cont
          liftEffect $ Ref.write (Just newFiber) mOutputFiberRef
        Removed -> prop Removed
    }

-- private

scopemap :: forall m a b. Applicative m => Scope -> Widget m a b -> Widget m a b
scopemap scope p = wrap ado
  { toUser, fromUser } <- unwrap p
  in
    { toUser: \cha -> case zoomIn cha of
      Nothing -> pure unit
      Just cha' -> toUser cha'
    , fromUser: \prop -> do
      fromUser $ prop <<< zoomOut
    }
  where
    zoomOut :: Changed b -> Changed b
    zoomOut Removed = Removed
    zoomOut (Altered (New scopes mb cont)) = Altered $ New (scope : scopes) mb cont

    zoomIn :: Changed a -> Maybe (Changed a)
    zoomIn Removed = Just Removed
    zoomIn (Altered (New scopes ma cont)) = case uncons scopes of
      Just { head, tail } | head == scope -> Just $ Altered $ New tail ma cont
      Nothing -> Just $ Altered $ New [] ma cont
      Just { head: Variant _ } -> Just $ Altered $ New [] ma cont -- not matching head but head is twist
      _ -> case scope of
        Variant _ -> Just $ Altered $ New [] ma cont -- not matching head but scope is twist
        _ -> Nothing -- otherwise

