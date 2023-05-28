-- There exists Data.Functor.Invariant module in purescript-invariant (https://pursuit.purescript.org/packages/purescript-invariant/6.0.0) already,
-- yet here we tweak class hierarchy of invariant, covariant and invariant functors according to observation made in [1]: 
-- "you could argue that in an ideal world the definition for Functor should change to class ExpFunctor f => Functor f".
module Data.Invariant where

import Prelude

import Control.MonadFix (mfix)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Ref as Ref

-- Functor class hierarchy

class Invariant f where
    invmap :: forall a b . (a -> b) -> (b -> a) -> f a -> f b
    -- aka exponential functor

class Invariant f <= Covariant f where
    covmap :: forall a b . (a -> b) -> f a -> f b
    -- law: invmap = const <<< covmap

class Invariant f <= Contravariant f where
    conmap :: forall a b . (b -> a) -> f a -> f b
    -- law: invmap = const conmap

class Invariant f <= CartesianInvariant f where
    invfirst :: forall a b. f a -> f (Tuple a b)
    invsecond :: forall a b. f b -> f (Tuple a b)

class Invariant f <= CoCartesianInvariant f where
    invleft :: forall a b. f a -> f (Either a b)
    invright :: forall a b. f b -> f (Either a b)

    -- TODO: MonoidalInvariant

-- Polymorphic invariant transformers - invariant optics 

type InvOptic :: forall k. (k -> Type) -> k -> k -> Type
type InvOptic i a b = i a -> i b

type InvAdapter a b = forall i. Invariant i => InvOptic i a b

type InvLens a b = forall i. CartesianInvariant i => InvOptic i a b

type InvPrism a b = forall i. CoCartesianInvariant i => InvOptic i a b

    -- TODO: InvTraversal

-- Invariant polymorphic transformers - ?

type InvTransformer i = forall a . Invariant i => i a -> i a

-- 

class Invariant i <= FooInvariant i where
    invappend :: forall a . i a -> i a -> i a
    invempty :: forall a . i a
    -- laws: 
    --  invappend a invempty == a = invappend invempty a
    --  invappend a (invappend b c) == invappend (invappend a b) c

combineCartesian :: forall i a b . Invariant i => CartesianInvariant i => FooInvariant i => i a -> i b -> i (Tuple a b)
combineCartesian a b = invfirst a `invappend` invsecond b

combineCoCartesian :: forall i a b . Invariant i => CoCartesianInvariant i => FooInvariant i => i a -> i b -> i (Either a b)
combineCoCartesian a b = invleft a `invappend` invright b

-- Example

newtype Widget a = Widget (a -> (a -> Effect Unit) -> Effect (a -> Effect Unit))

mkstorage :: forall a . Show a => String -> Widget a
mkstorage name = Widget \a callback -> do
    let render a = log $ "render: " <> name <> ":=" <> show a
    render a
    pure render

instance Invariant Widget where
    invmap f g (Widget widget) = Widget  \b callbackb -> (_ <<< g) <$> widget (g b) (callbackb <<< f)

instance CartesianInvariant Widget where
    invfirst (Widget widget) = Widget \ab callbackab -> do
        bref <- Ref.new $ snd ab
        update <- widget (fst ab) \a -> do
            b <- Ref.read bref 
            callbackab (Tuple a b)
        pure  \ab -> do
            Ref.write (snd ab) bref
            -- TODO: only if we know that (fst ab) has changed 
            update (fst ab)
    invsecond (Widget widget) = Widget \ab callbackab -> do
        aref <- Ref.new $ fst ab
        update <- widget (snd ab) \b -> do
            a <- Ref.read aref
            callbackab (Tuple a b)
        pure  \ab -> do
            Ref.write (fst ab) aref
            -- TODO: only if we know that (snd ab) has changed
            update (snd ab)

instance CoCartesianInvariant Widget where
    invleft (Widget widget) = Widget \aorb callbackaorb -> do
        -- TODO create slot
        mupdateRef <- case aorb of
            Left a -> do
                update <- widget a $ callbackaorb <<< Left
                Ref.new (Just update)
            Right b -> Ref.new Nothing
        pure \aorb -> case aorb of
            Left a -> do
              mUpdate <- Ref.read mupdateRef
              update <- case mUpdate of
                Just update -> pure update
                Nothing -> do
                    -- TODO cleanup slot
                    update <- widget a $ callbackaorb <<< Left
                    Ref.write (Just update) mupdateRef
                    pure update
              update a
            Right _ -> do
              -- TODO cleanup slot
              Ref.write Nothing mupdateRef
    invright (Widget widget) = Widget \aorb callbackaorb -> do
        -- TODO create slot
        mupdateRef <- case aorb of
            Right b -> do
                update <- widget b $ callbackaorb <<< Right
                Ref.new (Just update)
            Left a -> Ref.new Nothing
        pure \aorb -> case aorb of
            Right b -> do
              mUpdate <- Ref.read mupdateRef
              update <- case mUpdate of
                Just update -> pure update
                Nothing -> do
                    -- TODO cleanup slot
                    update <- widget b $ callbackaorb <<< Right
                    Ref.write (Just update) mupdateRef
                    pure update
              update b
            Left _ -> do
              -- TODO cleanup slot
              Ref.write Nothing mupdateRef

instance FooInvariant Widget where
    invempty = Widget $ const $ const $ pure mempty
    invappend (Widget widget1) (Widget widget2) = Widget \a callbacka -> do
      { update1, update2 } <- mfix \get -> do -- :: forall a. ((Unit -> a) -> m a) -> m a 
        let {update1: update1', update2: update2'} = get unit
        update1 <- widget1 a $ \a -> do
            update2' a
            callbacka a
        update2 <- widget2 a \a -> do
            update1' a
            callbacka a
        pure { update1, update2 }
      pure \a -> do
        update1 a
        update2 a

-- Invariant transformers

-- References
-- 1. Edward Kmett: Rotten Bananas, http://comonad.com/reader/2008/rotten-bananas/  
