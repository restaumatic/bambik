-- | Structural equality and order of plain values (guardrails L18, experiment
-- | holey-weak-types): a selector compares its options, and a keyed collection
-- | indexes its keys, by the values' structure rather than by an `Eq`/`Ord`
-- | instance — so neither demands a type only a business function closes.
-- | Plain values are records, variants, arrays, strings, numbers, booleans.
-- | The key is read through the ecosystem's `Foreign`, so the algebra keeps
-- | no JavaScript of its own.
module Data.Profunctor.Row.Structural (withStructuralEq, withStructuralOrd) where

import Prelude (class Eq, class Ord, compare, map, otherwise, show, (<>), (==), (||), (<<<))
import Data.Array (sort)
import Data.Maybe (maybe)
import Data.String (joinWith)
import Foreign (Foreign, isArray, isNull, isUndefined, typeOf, unsafeFromForeign, unsafeToForeign)
import Foreign.Object (Object)
import Foreign.Object as Object
import Unsafe.Coerce (unsafeCoerce)

structuralKey :: forall a. a -> String
structuralKey = keyOf <<< unsafeToForeign

keyOf :: Foreign -> String
keyOf f
  | isNull f || isUndefined f = "null"
  | isArray f = "[" <> joinWith "," (map keyOf (unsafeFromForeign f :: Array Foreign)) <> "]"
  | typeOf f == "object" =
      let o = unsafeFromForeign f :: Object Foreign
      in "{" <> joinWith "," (map (\k -> show k <> ":" <> maybe "null" keyOf (Object.lookup k o)) (sort (Object.keys o))) <> "}"
  | typeOf f == "string" = show (unsafeFromForeign f :: String)
  | typeOf f == "number" = show (unsafeFromForeign f :: Number)
  | typeOf f == "boolean" = show (unsafeFromForeign f :: Boolean)
  | otherwise = typeOf f

newtype Structural a = Structural a

instance Eq (Structural a) where
  eq (Structural x) (Structural y) = structuralKey x == structuralKey y

instance Ord (Structural a) where
  compare (Structural x) (Structural y) = compare (structuralKey x) (structuralKey y)

newtype GivenEq a r = GivenEq (Eq a => r)
newtype GivenOrd a r = GivenOrd (Ord a => r)

-- | Run a computation needing `Eq a` with structural equality — `Structural`
-- | is a newtype, so its dictionary compares the bare values.
withStructuralEq :: forall @a r. (Eq a => r) -> r
withStructuralEq f = case (unsafeCoerce (GivenEq f :: GivenEq a r) :: GivenEq (Structural a) r) of GivenEq g -> g

withStructuralOrd :: forall @a r. (Ord a => r) -> r
withStructuralOrd f = case (unsafeCoerce (GivenOrd f :: GivenOrd a r) :: GivenOrd (Structural a) r) of GivenOrd g -> g
