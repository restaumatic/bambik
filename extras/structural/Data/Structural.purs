-- | Structural equality and order of plain values: two values compare by
-- | their structure, read through the ecosystem's `Foreign`, with no
-- | `Eq`/`Ord` instance asked of their type. Plain values are records,
-- | variants, arrays, strings, numbers and booleans; anything else (a
-- | function, a foreign object's prototype) compares by its JavaScript type
-- | alone, so two functions are equal.
-- |
-- | Laws, for plain `x` and `y` of one type with lawful instances:
-- |
-- | ```
-- | withStructuralEq @a (x == y) = (x == y)                 -- agreement
-- | withStructuralOrd @a (compare x x) = EQ                 -- reflexivity
-- | withStructuralOrd @a (compare x y) = EQ  iff  withStructuralEq @a (x == y)
-- | ```
-- |
-- | The order is total and consistent with the equality but otherwise
-- | arbitrary (it compares rendered keys), which is all a `Map` index
-- | needs. Record fields are read in sorted label order, so two records
-- | equal as values are equal here whatever order built them.
-- |
-- | Why it exists (guardrails L18): a keyed collection indexes its keys
-- | and an introducing editor compares its initial values, and neither may
-- | demand an instance, since the key's type is closed only by a business
-- | function that a holey build does not have. It mentions no row and no
-- | profunctor, hence its own source root beside `extras/variant`; it
-- | keeps no JavaScript of its own.
module Data.Structural (withStructuralEq, withStructuralOrd) where

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
