-- | Single-field row profunctors over `Record`s, built on `RowStrong` (i.e. on `Strong`).
-- |
-- |   * `introduceProperty` — grow: add a field computed from the whole record (`second` +
-- |     insert; the source `p (Record s) prop` may read the accumulator — the `p s r` shape).
-- |   * `eliminateProperty`  — shrink: drop a field, feeding its value to a sink (`first` +
-- |     delete). The transpose of `introduceProperty`.
-- |   * `editProperty`        — edit an existing field in place: `RowStrong`'s `focusField`
-- |     specialized to type-preserving, i.e. the standard field lens.
-- |
-- | Sum-side counterparts live in `Data.Profunctor.RowToRow.Case`.
module Data.Profunctor.RowToRow.Property
  ( introduceProperty
  , eliminateProperty
  , editProperty
  ) where

import Data.Lens (Lens, Optic)
import Data.Profunctor (dimap)
import Data.Profunctor.RowToRow.RowStrong (class RowStrong, focusField)
import Data.Profunctor.Strong (first, second)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), snd)
import Prim.Row (class Cons, class Lacks)
import Record (delete, get, insert)
import Type.Proxy (Proxy(..))

-- | Introduce a new field `l :: prop`, computing its value from the whole record `s`
-- | (the `p s r` shape). `id &&& f` followed by `insert`.
introduceProperty
  :: forall p @l prop s t
   . IsSymbol l
  => Cons l prop s t
  => Lacks l s
  => RowStrong p
  => Optic p (Record s) (Record t) (Record s) prop
introduceProperty f =
  dimap (\s -> Tuple s s) (\(Tuple s p) -> insert (Proxy @l) p s) (second f)

-- | Eliminate the field `l :: prop`, feeding its value to a sink and keeping the rest.
-- | The transpose of `introduceProperty`: `first` + `delete`.
eliminateProperty
  :: forall p @l prop s t x
   . IsSymbol l
  => Cons l prop t s
  => Lacks l t
  => RowStrong p
  => Optic p (Record s) (Record t) prop x
eliminateProperty f =
  dimap (\s -> Tuple (get (Proxy @l) s) (delete (Proxy @l) s)) snd (first f)

-- | Edit an existing field in place — `RowStrong`'s `focusField` at a type-preserving focus.
editProperty
  :: forall @l s r a
   . IsSymbol l
  => Cons l a r s
  => Lens (Record s) (Record s) a a
editProperty = focusField (Proxy @l)
