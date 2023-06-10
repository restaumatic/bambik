-- Polymorphic invariant transformers - invariant optics 
module Data.Invariant.Optics
  ( Hop
  , Path(..)
  , class Tagged
  , getPath
  , setPath
  , constructorInvPrism
  , invAdapter
  , invAffineTraversal
  , invAffineTraversal'
  , invLens
  , invPrism
  , modifyPath
  , projection
  , property
  , remainingPath
  , replace
  , zeroed
  )
  where

import Prelude hiding (zero)

import Data.Array (cons, drop, intercalate, length, null, zipWith)
import Data.Either (Either(..), either)
import Data.Foldable (and)
import Data.Function (on)
import Data.Invariant (class Cartesian, class CoCartesian, class Invariant, invfirst, invleft, invmap, invright, invsecond)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Plus (class Plus, zero)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row as Row
import Record (get, set)
import Type.Proxy (Proxy)


class Tagged :: forall k. (k -> Type) -> Constraint
class Tagged i where
    getPath :: forall a. i a -> Path
    setPath :: forall a. Path -> i a -> i a

modifyPath :: forall i a . Tagged i => (Path -> Path) -> i a -> i a
modifyPath f ia = setPath (f (getPath ia)) ia

newtype Path = Path (Array Hop)

derive instance Newtype Path _

type Hop = String

instance Semigroup Path where
  append p1 p2 = wrap $ on (<>) unwrap p1 p2

instance Monoid Path where
  mempty = wrap []

instance Show Path where
  show (Path hops) = intercalate "." hops

-- hops2 - hops1
remainingPath :: Path -> Path -> Maybe Path
remainingPath (Path hops1) (Path hops2) = if arePrefixing hops1 hops2 then Just $ Path $ drop (length hops1) hops2 else Nothing

-- TODO Move to extras
arePrefixing :: forall a . Eq a => Array a -> Array a -> Boolean
arePrefixing hops1 hops2 = null hops1 || null hops2 || let commonPrefixComparison = zipWith (==) hops1 hops2 in and commonPrefixComparison && (length commonPrefixComparison == length hops1 || length commonPrefixComparison == length hops2)

invAdapter :: forall i a s . Invariant i => (a -> s) -> (s -> a) -> i a -> i s
invAdapter f g = invmap f g

invLens :: forall i a s. Invariant i => Cartesian i => (s -> a) -> (s -> a -> s) -> i a -> i s
invLens get set ia = invmap (\(Tuple a s) -> set s a) (\s -> Tuple (get s) s) (invfirst ia)

property'
  :: forall i l r1 r a
   . Invariant i
  => Cartesian i
  => IsSymbol l
  => Row.Cons l a r r1
  => Proxy l
  -> i a -> i (Record r1)
property' l = invLens (\s -> get l s) (\s a -> (set l) a s)

property
  :: forall i l r1 r a
   . Invariant i
  => Cartesian i
  => Tagged i
  => IsSymbol l
  => Row.Cons l a r r1
  => Proxy l
  -> i a -> i (Record r1)
property l ia = let hop = reflectSymbol l in property' l ia # modifyPath (\(Path hops) -> Path (hop `cons` hops))

invPrism :: forall i a s. Invariant i => CoCartesian i => (a -> s) -> (s -> Either a s) -> i a -> i s
invPrism review preview ia = invmap (\aors -> either review identity aors) preview (invleft ia)

constructorInvPrism :: forall i a s. Invariant i => CoCartesian i => (a -> s) -> (s -> Maybe a) -> i a -> i s
constructorInvPrism construct deconstruct ia = invmap (\(aors :: Either a s) -> either construct identity aors) (\s -> maybe (Right s) Left (deconstruct s)) (invleft ia)

invAffineTraversal
  :: forall s a i
   . Invariant i
  => CoCartesian i
  => Cartesian i
  => (s -> a -> s)
  -> (s -> Either s a)
  -> i a -> i s
invAffineTraversal set pre = invAffineTraversal' (\s -> Tuple (set s) (pre s))

invAffineTraversal'
  :: forall s a i
   . Invariant i
  => Cartesian i
  => CoCartesian i
  => (s -> Tuple (a -> s) (Either s a))
  -> i a -> i s
invAffineTraversal' to pab =
  invmap (\(Tuple b f) -> either identity b f) to (invsecond (invright pab))

projection :: forall i a s . Invariant i => Cartesian i => (s -> a) -> i a -> i s
projection f = invLens f (\s _ -> s)


-- TODO: these are not a strict optic
replace :: forall i a . Invariant i => i a -> i a -> i a
replace = const

zeroed :: forall i a s . Invariant i => Plus i => i a -> i s
zeroed = const zero
