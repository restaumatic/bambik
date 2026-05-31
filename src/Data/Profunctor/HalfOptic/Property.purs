-- | Product-side half-optics over `Record`s — all of them just `Strong`.
-- |
-- | 2×2×pin position: row = product (`Tuple`/`Record`). The old `ReadP`/`WriteP`/`FormP`/
-- | `EditPropP` classes were `Unit`-pinned weakenings of `Strong`; under the `p s r`/`p w s`
-- | shape the pin is the copy (`Δ`) rather than discard (`!`), which *is* full `Strong`. So
-- | these combinators carry only a `Strong p` constraint — and since `UI` is `Strong`, they
-- | work directly on `UI`.
-- |
-- |   * `introduceProperty` — introduce (grow): `second` + insert. The source `p (Record s) prop`
-- |     may **read the accumulator** (the `p s r` shape); a context-free source just ignores it.
-- |   * `eliminateProperty`  — eliminate (consume): transpose of introduce, `first` + delete.
-- |   * `editProperty`       — edit an existing field: the standard `Strong` field lens,
-- |     reused from `Data.Lens.Extra.Commons.property` (this is what `EditPropP` was).
-- |
-- | Sum-side counterparts live in `Data.Profunctor.HalfOptic.Case`.
module Data.Profunctor.HalfOptic.Property
  ( introduceProperty
  , eliminateProperty
  , editProperty
  ) where

import Data.Lens (Lens, Optic)
import Data.Lens.Extra.Commons (property) as Commons
import Data.Profunctor (dimap)
import Data.Profunctor.Strong (class Strong, first, second)
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
  => Strong p
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
  => Strong p
  => Optic p (Record s) (Record t) prop x
eliminateProperty f =
  dimap (\s -> Tuple (get (Proxy @l) s) (delete (Proxy @l) s)) snd (first f)

-- | Edit an existing field in place — the standard `Strong` field lens (formerly `EditPropP`).
editProperty
  :: forall @l s r a
   . IsSymbol l
  => Cons l a r s
  => Lens (Record s) (Record s) a a
editProperty = Commons.property @l
