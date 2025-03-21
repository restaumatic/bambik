module UI
  ( Ctor
  , Field
  , New(..)
  , PropagationError
  , PropagationStatus
  , Scope(..)
  , UI(..)
  , UIO
  , UIOcular
  , UIOptics
  , action
  , action'
  , adapter
  , affAdapter
  , class Foo
  , class RecordFoo
  , fooRecord
  , constant
  , constructor
  , debounced
  , debounced'
  , effAdapter
  , field
  , foo
  , iso
  , just
  , left
  , lens
  , prism
  , projection
  , right
  , spied
  )
  where

import Prelude

import Control.Alt (class Alt)
import Control.Plus (class Plus)
import Data.Array (uncons, (:))
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Lens (Optic, first)
import Data.Lens as Profunctor
import Data.Lens.Extra.Types (Ocular)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (class Profunctor, dimap, lcmap)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Profunctor.Sum (class Sum, psum)
import Data.Profunctor.Zero (class Zero, pzero)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Symbol (reflectSymbol)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..), fst, snd)
import Debug (class DebugWarning, spy)
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (Aff, delay, error, forkAff, killFiber, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Prim.Row as Row
import Prim.RowList as RowList
import Record (get, set)
import Record as Record
import Type.Proxy (Proxy(..))
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

newtype UI m i o = UI (m
  { toUser :: New i -> Effect Unit
  , fromUser :: (New o -> Effect PropagationStatus) -> Effect Unit
  })

-- TODO: rename ValidationStatus/UserInputStatus?
type PropagationStatus = Maybe PropagationError

type PropagationError = String

derive instance Newtype (UI m i o) _

data New a = New (Array Scope) a Boolean

derive instance Functor New

data Scope = Part String | Variant String

instance Show Scope where
  show = case _ of
    Part s -> "." <> s
    Variant s -> "?" <> s

derive instance Eq Scope

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
          New scope ab cont -> do
            let _ = unsafePerformEffect $ Ref.write ab lastab
            p'.toUser $ New scope (fst ab) cont
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
          New scope ab cont -> do
            let _ = unsafePerformEffect $ Ref.write ab lastab
            p'.toUser $ New scope (snd ab) cont
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
        New scope (Right c) cont -> do
          let prop = unsafePerformEffect $ Ref.read propRef
          _ <- prop (New scope (Right c) cont)
          pure unit
        New scope (Left a) cont -> p'.toUser $ New scope a cont
      , fromUser: \prop -> do
        Ref.write prop propRef
        p'.fromUser \u -> prop (Left <$> u)
      }
  right p = wrap ado
    p' <- unwrap p
    let propRef = unsafePerformEffect $ Ref.new (unsafeCoerce unit)
    in
      { toUser: case _ of
        New scope (Left c) cont -> do
          let prop = unsafePerformEffect $ Ref.read propRef
          _ <- prop (New scope (Left c) cont)
          pure unit
        New scope (Right a) cont -> p'.toUser $ New scope a cont
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

instance Functor m => Functor (UI m a) where
  map f p = wrap $ unwrap p <#> \p' ->
    { toUser: p'.toUser
    , fromUser: p'.fromUser <<< lcmap (map f)
    }

instance Apply m => Alt (UI m a) where
  alt = psum

instance Applicative m => Plus (UI m a) where
  empty = pzero

instance Apply m => Semigroup (UI m a a) where
  append p1 p2 = wrap ado
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
-- Notice: optic `WidgetOptic m a b c c` is also a Semigroup

instance Applicative m => Monoid (UI m a a) where
  mempty = pzero
-- Notice: optic `WidgetOptic m a b c c` is also a Monoid

-- optics

type UIOptics a b s t = forall m. Functor m => Optic (UI m) s t a b

type UIO s t a b = forall m. Functor m => Optic (UI m) s t a b

projection :: forall a s t. (s -> a) -> UIOptics a Void s t
projection f = dimap f absurd

-- Optimized implementation. Not optimized would be `constant a = projection (const a)`.
constant :: forall a s t. a -> UIOptics a Void s t
constant a w = wrap $ ado
  w' <- unwrap w
  let initializedRef = unsafePerformEffect $ Ref.new false
  in
    { toUser: \_ -> do
      initialized <- Ref.read initializedRef
      when (not initialized) do
        Ref.write true initializedRef
        w'.toUser $ New [] a false
    , fromUser: mempty
    }

adapter :: forall a b s t. String -> (s -> a) -> (b -> t) -> UIOptics a b s t
adapter name mapin mapout = dimap mapin mapout >>> scopemap (Variant name) -- TODO not sure about `Variant name`

iso :: forall a s. String -> (s -> a) -> (a -> s) -> UIOptics a a s s
iso name mapin mapout = dimap mapin mapout >>> scopemap (Variant name)

lens :: forall a b s t. (s -> a) -> (s -> b -> t) -> UIOptics a b s t
lens getter setter = Profunctor.lens getter setter

prism :: forall a b s t. (b -> t) -> (s -> Either t a) -> UIOptics a b s t
prism to from = Profunctor.prism to from

type Field a s = UIOptics a a s s

-- TODO use Data.Lens.Record.prop?
field :: forall @l s r a. IsSymbol l => Row.Cons l a r s =>  (a -> Record s -> Maybe PropagationError) -> Field a (Record s)
field validate wa = scopemap (Part (reflectSymbol (Proxy @l))) $
  wrap $ ado
    wars <- unwrap (first wa)
    in
      { toUser: \chrs -> wars.toUser $ (map (\rs -> Tuple (get (Proxy @l) rs) rs)) chrs
      , fromUser: \prop -> wars.fromUser $ \chrs@(New _ (Tuple a rs) _) -> do
          case validate a rs of
            Just error -> pure $ Just error
            Nothing -> prop $ map (\(Tuple a rs) -> set (Proxy @l) a rs) chrs
      }

type Ctor a s = UIOptics (Maybe a) a s s

constructor :: forall a s. String -> (a -> s) -> (s -> Maybe a) -> Ctor a s
constructor name construct deconstruct w = scopemap (Variant name) $ dimap deconstruct construct w

-- TODO move to utils?
just :: forall a. Ctor a (Maybe a)
just = constructor "Just" Just identity

right :: forall a b. Ctor a (Either b a)
right = constructor "right" Right (case _ of
  Left _ -> Nothing
  Right r -> Just r)

left :: forall a b. Ctor a (Either a b)
left = constructor "left" Left (case _ of
  Right _ -> Nothing
  Left l -> Just l)

action :: forall i o. (i -> Aff o) -> UIOptics Boolean Void i o
action arr = action' \i pro post -> do
  liftEffect $ pro true
  o <- arr i
  liftEffect $ pro false
  liftEffect $ post o

action' :: forall a b i o. (i -> (a -> Effect Unit) -> (o -> Effect Unit) -> Aff Unit) -> UIOptics a b i o
action' arr w = wrap ado
  let oVar = unsafePerformEffect $ liftEffect AVar.empty
  w' <- unwrap w
  in
    { toUser: case _ of
      New _ i cont -> launchAff_ $ arr i (\a -> void $ w'.toUser $ New [] a cont) (\o -> void $ AVar.put o oVar mempty)
    , fromUser: \prop ->
      let waitAndPropagate = void $ AVar.take oVar case _ of
            Left error -> pure unit -- TODO handle error
            Right o -> do
              -- w'.toUser $ New [] Nothing false
              void $ prop $ New [] o false -- TODO really?
              waitAndPropagate
      in waitAndPropagate
    }

-- oculars

type UIOcular m = Ocular (UI m)

debounced' :: forall m. Applicative m => Milliseconds -> UIOcular m
debounced' millis = affAdapter $ pure
  { pre: case _ of
    (New _ i true) -> delay millis *> pure i
    (New _ i false) -> pure i
  , post: \(New _ i _) -> pure i
  }

debounced :: forall m. Applicative m => UIOcular m
debounced = debounced' (Milliseconds 300.0)

spied :: forall m. Functor m => DebugWarning => String -> UIOcular m
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
    spy' text (New scope a cont) = spy ("Spied UI \"" <> name <> "\" " <> text <> " new value in scope " <> show scope <> " with continuity " <> show cont) a

-- modifiers

-- notice: this is not really optics, operates for given m
-- TODO add release parameter?
-- TODO is this needed?
effAdapter :: forall m a b s t. Apply m => m { pre :: s -> Effect a, post ::  b -> Effect t} -> UI m a b -> UI m s t
effAdapter f w = wrap ado
  { toUser, fromUser } <- unwrap w
  { pre, post } <- f
  in
    { toUser: case _ of
      New _ s cont -> do
        a <- pre s
        toUser $ New [] a cont
    , fromUser: \prop -> do
      fromUser case _ of
        New _ b cont -> do
          t <- post b
          prop $ New [] t cont
    }

-- TODO is this needed?
affAdapter :: forall m a b s t. Apply m => m { pre :: New s -> Aff a, post ::  New b -> Aff t} -> UI m a b -> UI m s t
affAdapter f w = wrap ado
  { toUser, fromUser } <- unwrap w
  { pre, post } <- f
  let mInputFiberRef = unsafePerformEffect $ Ref.new Nothing
  let mOutputFiberRef = unsafePerformEffect $ Ref.new Nothing
  in
    { toUser: case _ of
      news@(New _ _ cont) -> launchAff_ do
        mFiber <- liftEffect $ Ref.read mInputFiberRef
        for_ mFiber $ killFiber (error "Obsolete input")
        newFiber <- forkAff do
          a <- pre news
          liftEffect $ toUser $ New [] a cont
        liftEffect $ Ref.write (Just newFiber) mInputFiberRef
    , fromUser: \prop -> do
      fromUser case _ of
        newb@(New _ _ cont) -> do
          launchAff_ do
            mFiber <- liftEffect $ Ref.read mOutputFiberRef
            for_ mFiber $ killFiber (error "Obsolete output")
            newFiber <- forkAff do
              t <- post newb
              liftEffect $ prop $ New [] t cont
            liftEffect $ Ref.write (Just newFiber) mOutputFiberRef
          pure Nothing
    }

class Foo f where
  foo :: f

instance Foo String where
  foo = ""

instance Foo Unit where
  foo = unit

instance Foo (Array a) where
  foo = []


class RecordFoo :: forall k. k -> Row Type -> Constraint
class RecordFoo rl r | rl -> r where
  fooRecord :: Proxy rl -> Record r

instance ( IsSymbol name
         , Foo value
         , Row.Cons name value tailRow row
         , RecordFoo tailRowList tailRow -- type level recursion is here
         , Row.Lacks name tailRow
         )
      => RecordFoo (RowList.Cons name value tailRowList) row where
  fooRecord _ = Record.insert (Proxy @name) foo (fooRecord (Proxy @tailRowList))

instance RecordFoo RowList.Nil () where
  fooRecord _ = {}

instance (RowList.RowToList r rl
         , RecordFoo rl r)
        =>  Foo (Record r) where
  foo = fooRecord (Proxy @rl)

-- private

scopemap :: forall m a b. Functor m => Scope -> UI m a b -> UI m a b
scopemap scope p = wrap ado
  { toUser, fromUser } <- unwrap p
  in
    { toUser: \newa -> case zoomIn newa of
      Nothing -> pure unit
      Just newa' -> toUser newa'
    , fromUser: \prop -> do
      fromUser $ zoomOut >>> prop
    }
  where
    zoomOut :: New b -> New b
    zoomOut (New scopes mb cont) = New (scope : scopes) mb cont

    zoomIn :: New a -> Maybe (New a)
    zoomIn (New scopes ma cont) = case uncons scopes of
      Nothing -> Just $ New [] ma cont
      Just { head, tail } | head == scope -> Just $ New tail ma cont
      Just { head: Variant _ } -> Just $ New [] ma cont -- not matching head but head is twist
      _ -> case scope of
        Variant _ -> Just $ New [] ma cont -- not matching head but scope is twist
        _ -> Nothing -- otherwise

