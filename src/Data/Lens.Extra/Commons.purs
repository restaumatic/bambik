module Data.Lens.Extra.Commons
  ( constructor
  , property
  , input
  , just
  , left
  , missing
  , missing'
  , nothing
  , projection
  , right
  )
  where

import Prelude

import Data.Default (class Default, default)
import Data.Either (Either(..))
import Data.Lens (Iso, Lens, Prism, lens, prism)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor (dimap)
import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Record (get)
import Type.Prelude (Proxy(..))

-- This is just `Data.Lens.Record.prop` but with a type signature allowing for type annotations
property :: forall @l s r a. IsSymbol l => Row.Cons l a r s =>  Lens (Record s) (Record s) a a
property = prop (Proxy :: Proxy l)

input :: forall @l s r a. IsSymbol l => Row.Cons l a r s =>  Lens (Record s) (Record s) a a
input = property @l

output :: forall @l s r a t. IsSymbol l => Row.Cons l a r s =>  Lens (Record s) t a Void
output = let l = (Proxy :: Proxy l) in lens (get l) (\_ x -> absurd x)

constructor :: forall a s. (a -> s) -> (s -> Maybe a) -> Iso s s (Maybe a) a
constructor construct deconstruct w = dimap deconstruct construct w

projection :: forall a s t. (s -> a) -> Iso s t a Void
projection f = dimap f absurd

missing :: forall a. a -> Prism (Maybe a) (Maybe a) a a
missing default = prism Just case _ of
  Just a -> Left (Just a)
  Nothing -> Right default

missing' :: forall a. Default a => Iso (Maybe a) (Maybe a) a a
missing' = dimap (fromMaybe default) Just

nothing :: forall a. Default a => Iso (Maybe a) (Maybe a) (Maybe a) a
nothing = dimap (case _ of 
  Nothing -> Just default
  _ -> Nothing) Just

just :: forall a. Iso (Maybe a) (Maybe a) (Maybe a) a
just = dimap identity Just 

right :: forall a b. Iso (Either b a) (Either b a) (Maybe a) a
right = flip dimap Right (case _ of
  Left _ -> Nothing
  Right r -> Just r)

left :: forall a b. Iso (Either b a) (Either b a) (Maybe b) b
left = flip dimap Left (case _ of
  Right _ -> Nothing
  Left l -> Just l)
