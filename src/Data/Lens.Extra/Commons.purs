module Data.Lens.Extra.Commons where

import Prelude

import Data.Default (class Default, default)
import Data.Either (Either(..))
import Data.Lens (Iso, Lens, Prism, Optic, lens, prism)
import Data.Lens (Lens, lens)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor (dimap)
import Data.Profunctor.StrongLike as StrongLike
import Data.Symbol (class IsSymbol)
import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Prim.Row as Row
import Record (get)
import Record (get, set)
import Type.Prelude (Proxy(..))
import Type.Proxy (Proxy)

-- This is just `Data.Lens.Record.prop` but with a type signature allowing for type annotations
field :: forall @l s r a. IsSymbol l => Row.Cons l a r s =>  LensLike (Record s) (Record s) a a
field = prop' (Proxy :: Proxy l)

type LensLike s t a b = forall p. StrongLike.StrongLike p => Optic p s t a b

prop'
  :: forall l r1 r2 r a b
   . IsSymbol l
  => Row.Cons l a r r1
  => Row.Cons l b r r2
  => Proxy l
  -> LensLike (Record r1) (Record r2) a b
prop' l = StrongLike.lens (get l) ((set l))

input :: forall @l s r a. IsSymbol l => Row.Cons l a r s =>  LensLike (Record s) (Record s) a a
input = field @l

output :: forall @l s r a t. IsSymbol l => Row.Cons l a r s =>  Lens (Record s) t a Void
output = let l = (Proxy :: Proxy l) in lens (get l) (\_ x -> absurd x)

constructor :: forall a s. (a -> s) -> (s -> Maybe a) -> Iso s s (Maybe a) a
constructor construct deconstruct w = dimap deconstruct construct w

projection :: forall a s t. (s -> a) -> Iso s t a Void
projection f = dimap f absurd

missing :: forall a. a -> Prism (Maybe a) (Maybe a) a a
missing default = StrongLike.prism Just case _ of
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