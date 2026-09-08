-- | The coined **product→sum** strength `Resolving` — the `× → +` analogue of
-- | the ecosystem's `Data.Profunctor.Strong`, and the reason it lives in its
-- | own module: like `Strong`/`Choice` it is stated **positionally**
-- | (`Tuple`/`Either`), with no row in sight. Its co-strength is
-- | `Data.Profunctor.Coresolving`, one module over exactly as `Costrong` sits
-- | beside `Strong`. The row layer that builds on it is
-- | `Data.Profunctor.Row.RecordToVariant`; the optic it generates is
-- | `Data.Lens.Shutter`.
-- |
-- | This module is a **complement of the ecosystem's own**, not bambik's: it
-- | claims a `Data.Profunctor.*` name because it belongs in that family
-- | beside `Strong`/`Choice`/`Costrong`/`Cochoice`, and it lives under the
-- | separate `extras/profunctor` source root to say so — nothing here
-- | mentions `PUI`, a row, or a carrier, so it could be lifted into
-- | `purescript-profunctor` unchanged.
module Data.Profunctor.Resolving
  ( class Resolving
  , resolve
  )
  where

import Data.Either (Either)
import Data.Profunctor (class Profunctor)
import Data.Tuple (Tuple)

-- | The **unary** product→sum strength for this direction: a single **loop /
-- | iteration step**. `resolve` runs a transformer `p a b` on an input `a`
-- | alongside a carried state `c`, returning a `Step`:
-- |
-- | ```
-- | resolve :: p a b -> p (Tuple a c) (Either b c)
-- |                                      -- Left  b = Done b  (finish)
-- |                                      -- Right c = Loop c  (continue)
-- | ```
-- |
-- | State enters guaranteed (product input) and leaves optionally (a branch of
-- | the sum output), so the step may *halt*; closing the `c` channel gives `p`
-- | a terminating iteration (`tailRec`-style). It is the `identity`-pinned form
-- | of the positional product→sum base merge
-- | `p a b -> p c d -> p (Tuple a c) (Either b d)` (its second operand fixed
-- | to `identity`) — the product→sum analogue of how `RecordToRecord.subStrong` is the
-- | unary form of `recordToRecord`.
-- |
-- | With no out-of-band loop signal in the wire protocol (values are just
-- | values), the `PUI` instance derives the branch **from time**: every
-- | emission loops (`Right`) while the UI component is still moving, and the last
-- | emission resolves (`Left`) at quiescence — so the seeded retraction
-- | reads `coresolve (resolve g >>> seeded (Right c0)) ≈ debounced g`.
-- | (No `(->)` instance: a timeless carrier could only give the trivial
-- | always-`Done` step, which carries no iteration.)
-- |
-- | **Laws.** Naturality in `a`, `b` and dinaturality in `c` come free
-- | (parametricity); the Tambara coherences are **deliberately absent and
-- | provably unavailable**: at `c := 1` a lawful unit coherence would force
-- | `resolve g = rmap Left g`, erasing the loop, and two nested `resolve`s
-- | cannot fuse even in principle — input residuals compose as `c × d`,
-- | output residuals as `c + d`, and no single channel carries both. So
-- | this is a **single-application mixed strength**, not a Tambara module,
-- | and the class alone is property-light: a carrier without a
-- | `Coresolving` half admits degenerate instances (`Cont`'s always-`Done`
-- | `resolve`). Its equational content lives in the seeded retraction with
-- | its co-strength (`Data.Profunctor.Coresolving`; tested in
-- | test/Main.purs). The consequence for the optic is stated in
-- | `Data.Lens.Shutter`: existential constructors are sound, completeness
-- | is not claimed.
-- |
-- | This is the **bare strength** for the `× → +` direction (the analogue of
-- | `Strong`/`Choice`); the row combinator built on it is
-- | `RecordToVariant.subResolving` — exactly as `RecordToRecord.subStrong` is
-- | built on `Strong`.
class Profunctor p <= Resolving p where
  resolve :: forall a b c. p a b -> p (Tuple a c) (Either b c)
