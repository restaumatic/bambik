module Data.Lens.Extra.Commons where

import Control.Category (identity)
import Data.Either (Either(..))
import Data.Function (const, flip)
import Data.Lens (Iso, Prism, Lens, prism)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Profunctor (dimap)
import Data.Symbol (class IsSymbol)
import Data.Void (Void, absurd)
import Prim.Row as Row
import Type.Prelude (Proxy(..))

-- This is just `Data.Lens.Record.prop` but with a type signature allowing for type annotations
field :: forall @l s r a. IsSymbol l => Row.Cons l a r s =>  Lens (Record s) (Record s) a a
field = prop (Proxy :: Proxy l)

constructor :: forall a s. (a -> s) -> (s -> Maybe a) -> Iso s s (Maybe a) a
constructor construct deconstruct w = dimap deconstruct construct w

projection :: forall a s t. (s -> a) -> Iso s t a Void
projection f = dimap f absurd

missing :: forall a. a -> Prism (Maybe a) (Maybe a) a a
missing default = prism Just case _ of
  Just a -> Left (Just a)
  Nothing -> Right default

missing' :: forall a. a -> Prism (Maybe a) a (Maybe a) a
missing' default = prism identity case _ of
  Just a -> Left a
  Nothing -> Right (Just default)


just :: forall a. Iso (Maybe a) (Maybe a) (Maybe a) a
just = flip dimap Just identity

nothing :: forall a. a -> Iso (Maybe a) (Maybe a) (Maybe a) a
nothing default p = dimap (maybe (Just default) (const Nothing)) Just p

withDefault :: forall a. a -> Iso (Maybe a) (Maybe a) a a
withDefault default p = dimap (fromMaybe default) Just p

right :: forall a b. Iso (Either b a) (Either b a) (Maybe a) a
right = flip dimap Right (case _ of
  Left _ -> Nothing
  Right r -> Just r)

left :: forall a b. Iso (Either b a) (Either b a) (Maybe b) b
left = flip dimap Left (case _ of
  Right _ -> Nothing
  Left l -> Just l)