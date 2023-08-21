module Data.Profunctor.Optics
  ( Adapter
  , Isomorphism
  , Projection
  , Constant
  , Prism
  , Lens
  , Constructor
  , Field
  , Null
  , adapter
  , constant
  , constructor
  , field
  , module Data.Profunctor
  , null
  , projection
  )
  where

import Prelude

import Data.Either (Either(..), either)
import Data.Invariant.Transformers.Changed (Change(..), Changed(..), Scope(..), zoomIn, zoomOut)
import Data.Maybe (Maybe, maybe)
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Choice (class Choice, left)
import Data.Profunctor.Plus (class ProfunctorZero, pzero)
import Data.Profunctor.Strong (class Strong, first)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row as Row
import Record (get, set)
import Type.Proxy (Proxy(..))

-- identation to emphasize hierarchy
type Adapter a b s t = forall p. Profunctor p => p (Changed a) (Changed b) -> p (Changed s) (Changed t)
type   Isomorphism a s = Adapter a a s s
type   Projection a s = Adapter a  Void s s
type     Constant a = forall s. Projection a s
type Lens a b s t = forall p. Strong p => p (Changed a) (Changed b) -> p (Changed s) (Changed t)
type   Field a s = Lens a a (Record s) (Record s)
type Prism a b s t = forall p. Choice p => p (Changed a) (Changed b) -> p (Changed s) (Changed t)
type   Constructor a s = Prism a a s s
type Null = forall p a b s t. ProfunctorZero p => p a b -> p s t

adapter :: forall a s. String -> (a -> s) -> (s -> a) -> Isomorphism a s
adapter name outside inside = dimap
  (\(Changed c b) -> Changed (zoomIn (Variant name) c) (inside b))
  (\(Changed c a) -> Changed (zoomOut (Variant name) c) (outside a))

projection :: forall a s. (s -> a) -> Projection a s
projection f = dimap
  (\(Changed c s) -> Changed c (f s))
  (\(Changed c a) -> Changed c (absurd a))

constant :: forall a. a -> Constant a
constant a = dimap
  (\_ -> Changed None a)
  (\(Changed c a) -> Changed c (absurd a))

field :: forall @l s r a . IsSymbol l => Row.Cons l a r s => Field a s
field = field' (reflectSymbol (Proxy @l)) (flip (set (Proxy @l))) (get (Proxy @l))
  where
    field' partName setter getter = first >>> dimap
      (\(Changed c s) -> Tuple (Changed (zoomIn (Part partName) c) (getter s)) s)
      (\(Tuple (Changed c a) s) -> Changed (zoomOut (Part partName) c) (setter s a))

constructor :: forall a s. String -> (a -> s) -> (s -> Maybe a) -> Constructor a s
constructor name construct deconstruct = left >>> dimap
  (\(Changed c s) -> maybe (Right (Changed c s)) (\a -> Left (Changed (zoomIn (Part name) c) a)) (deconstruct s))
  (\saors -> either (\(Changed c a) -> Changed (zoomOut (Part name) c) (construct a)) identity saors)

null :: Null
null = const pzero

