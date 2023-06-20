-- Polymorphic invariant transformers - invariant optics 
module Data.Invariant.Optics
  ( Hop
  , Path(..)
  , UserInput
  , UserInputType(..)
  , class Tagged
  , constructorInvPrism
  , getPath
  , invAffineTraversal
  , invAffineTraversal'
  , invLens
  , invPrism
  , modifyPath
  , pathDifference
  , prefixingArrays
  , prefixingPaths
  , projection
  , propagatedDown
  , propagatedUp
  , property
  , replace
  , setPath
  , userInput
  , userInputType
  , userInputValue
  , zeroed
  )
  where

import Prelude hiding (zero)

import Data.Array (cons, drop, intercalate, length, zipWith)
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

---

newtype UserInput a = UserInput
  { value :: a
  , path :: Path}

derive instance Functor UserInput

data UserInputType
  = Alteration
  | Replacement

userInput :: forall a. a -> UserInput a
userInput value = UserInput { value, path: mempty}

propagatedUp :: forall a. Path -> UserInput a -> UserInput a
propagatedUp propagationPath (UserInput {value, path}) = UserInput {value, path: propagationPath <> path}

propagatedDown :: forall a. Path -> UserInput a -> Maybe (UserInput a)
propagatedDown propagationPath (UserInput {value, path}) = pathDifference path propagationPath <#> \remPath -> UserInput { path: remPath, value }

userInputType :: forall a. UserInput a -> UserInputType
userInputType (UserInput { path }) = case path of
  Path [] -> Replacement
  Path _ -> Alteration

userInputValue :: forall a. UserInput a -> a
userInputValue (UserInput { value }) = value

---

pathDifference :: Path -> Path -> Maybe Path
pathDifference (Path hops1) (Path hops2) = Path <$> arrayDifference hops1 hops2

-- TODO Move to extras
-- ar1 `arrayDifference` ar2 == Just ar3 <=> ar1 == ar2 <> a3 otherwise ar1 `arrayDifference` ar2 == Nothing - TODO that is wrong when ar1 = []
arrayDifference :: forall a . Eq a => Array a -> Array a -> Maybe (Array a)
arrayDifference ar1 ar2 = let z = zipWith (==) ar1 ar2 in if and z then Just (drop (length z) ar1) else Nothing

-- commutative
prefixingPaths :: Path -> Path -> Boolean
prefixingPaths (Path hops1) (Path hops2) = prefixingArrays hops1 hops2

-- TODO Move to exports
-- commutative
prefixingArrays ∷ ∀ (a ∷ Type). Eq a ⇒ Array a → Array a → Boolean
prefixingArrays ar1 ar2 = and $ zipWith (==) ar1 ar2

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
