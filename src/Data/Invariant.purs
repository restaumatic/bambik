-- There exists Data.Functor.Invariant module in purescript-invariant (https://pursuit.purescript.org/packages/purescript-invariant/6.0.0) already,
-- yet here we tweak class hierarchy of invariant, covariant and invariant functors according to observation made in [1]: 
-- "you could argue that in an ideal world the definition for Functor should change to class ExpFunctor f => Functor f".
module Data.Invariant where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect(..))
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

-- example = do
--     (Tuple inputFirstName storageFirstName) <- mkInput "John"
--     (Tuple inputLastName storageLastName) <- mkInput "Doe"
  
-- mkinput :: a -> Effect (Tuple (a -> Effect Unit) (Storage a))
-- mkinput a = do
--     storage 
--     pure $ Tuple input storage


newtype Storage a = Storage (Effect
    { render :: a -> Effect Unit
    , listen :: (a -> Effect Unit) -> Effect Unit
    })

mkstorage :: forall a . Show a => String -> Effect (Tuple (Storage a) (a -> Effect Unit))
mkstorage name = do
    callbackRef <- Ref.new Nothing
    let storage = { render: \a -> log $ "render: " <> name <> ":=" <> show a, listen: Just >>> flip Ref.write callbackRef }
    let touch a = do
            storage.render a
            mcallback <- Ref.read callbackRef
            case mcallback of
                Just callback -> callback a
                _ -> pure unit
    pure $ Tuple (Storage (pure storage)) touch

instance Invariant Storage where
    invmap f g (Storage storage) = Storage do
        { render, listen } <- storage
        pure { render: g >>> render, listen: \callback -> listen (f >>> callback)}

instance CartesianInvariant Storage where
    invfirst (Storage storage) = Storage do
        { render, listen } <- storage
        pure { render: storage.render <<< fst, listen:  }
    -- invsecond (Storage storage) = Storage { render: storage.render <<< snd }

-- foo = Ref.new 0

-- Invariant transformers

-- References
-- 1. Edward Kmett: Rotten Bananas, http://comonad.com/reader/2008/rotten-bananas/  
