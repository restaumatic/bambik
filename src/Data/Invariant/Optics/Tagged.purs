module Data.Invariant.Optics.Tagged
  ( Hop
  , Path
  , UserInput(..)
  , UserInputType(..)
  , class Tagged
  , constructorInvPrism
  , getPath
  , prefixingPaths
  , propagatedDown
  , propagatedUp
  , property
  , property'
  , setPath
  , userInput
  , userInputType
  , userInputValue
  )
  where

import Prelude

import Data.Array (cons, drop, intercalate, length, zipWith)
import Data.Either (Either(..), either)
import Data.Foldable (and)
import Data.Function (on)
import Data.Invariant (class Cartesian, class CoCartesian, class Invariant, invleft, invmap)
import Data.Invariant.Optics (invLens)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row as Row
import Record (get, set)
import Type.Proxy (Proxy)

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

constructorInvPrism :: forall i a s. Invariant i => CoCartesian i => (a -> s) -> (s -> Maybe a) -> i a -> i s
constructorInvPrism construct deconstruct ia = invmap (\(aors :: Either a s) -> either construct identity aors) (\s -> maybe (Right s) Left (deconstruct s)) (invleft ia)


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
  , path :: Maybe Path}

derive instance Functor UserInput

instance Show (UserInput a) where
  show (UserInput { path: Just path' }) = show path'
  show (UserInput { path: Nothing }) = "-"

data UserInputType
  = Alteration
  | Replacement
  | Newness

userInput :: forall a. a -> UserInput a
userInput value = UserInput { value, path: Just mempty}

propagatedUp :: forall a. Path -> UserInput a -> UserInput a
propagatedUp propagationPath (UserInput {value, path}) = UserInput {value, path: (propagationPath <> _) <$> path}

-- TODO fix
propagatedDown :: forall a. Path -> UserInput a -> Maybe (UserInput a)
propagatedDown _ ui@(UserInput {path: Nothing}) = Just ui
propagatedDown _ (UserInput {value, path: Just (Path [])}) = Just $ UserInput {value, path: Nothing }
propagatedDown propagationPath (UserInput {value, path: Just path'}) = pathDifference path' propagationPath # \mRemPath -> mRemPath <#> \remPath -> UserInput { path: Just remPath, value }

userInputType :: forall a. UserInput a -> UserInputType
userInputType (UserInput { path }) = case path of
  Nothing -> Newness
  Just (Path []) -> Replacement
  Just (Path _) -> Alteration

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
