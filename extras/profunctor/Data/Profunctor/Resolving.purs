-- | The coined **product→sum** strength `Resolving` — the `× → +` analogue of
-- | the ecosystem's `Data.Profunctor.Strong`, stated **positionally**
-- | (`Tuple`/`Either`) with no row in sight. Its co-strength is
-- | `Data.Profunctor.Coresolving`; the row layer on it is
-- | `Data.Profunctor.Row.RecordToVariant`; the optic it generates is
-- | `Data.Lens.Shutter`. A complement of the ecosystem's `Data.Profunctor`
-- | family, mentioning no `PUI`, row or carrier, liftable into
-- | `purescript-profunctor` unchanged — hence the `extras/profunctor` root.
module Data.Profunctor.Resolving
  ( class Resolving
  , resolve
  )
  where

import Data.Either (Either)
import Data.Profunctor (class Profunctor)
import Data.Tuple (Tuple)

-- | The **unary** product→sum strength for this shape: a single **loop /
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
-- | a terminating iteration (`tailRec`-style). It is the positional
-- | product→sum merge `p a b -> p c d -> p (Tuple a c) (Either b d)` with
-- | its second operand fixed to `identity`.
-- |
-- | With no out-of-band loop signal on the wire (values are just values),
-- | the `PUI` instance derives the branch **from time**: every emission
-- | loops (`Right`) while the component is still moving, and the last
-- | resolves (`Left`) at quiescence — so the seeded retraction reads
-- | `coresolve (resolve g >>> seeded (Right c0)) ≈ debounced g`, and
-- | `debounced` stands on it. No `(->)` instance: a timeless carrier could
-- | only give the trivial always-`Done` step, which carries no iteration.
-- |
-- | **Laws.** Naturality in `a`, `b` and dinaturality in `c` come free
-- | (parametricity); the Tambara coherences are **deliberately absent and
-- | provably unavailable**: at `c := 1` a lawful unit coherence would force
-- | `resolve g = rmap Left g`, erasing the loop, and two nested `resolve`s
-- | cannot fuse even in principle — input residuals compose as `c × d`,
-- | output residuals as `c + d`, and no single channel carries both. So
-- | this is a **single-application mixed strength**, not a Tambara module,
-- | and the class alone is property-light: a carrier without a
-- | `Coresolving` half admits degenerate instances (a `resolve` that
-- | always takes `Left`). Its equational content is the seeded retraction
-- | with its co-strength (`Data.Profunctor.Coresolving`; test/Main.purs);
-- | the consequence for the optic is `Data.Lens.Shutter`'s: existential
-- | constructors are sound, completeness is not claimed. It has no row
-- | form — a stage is typed at one row (`Data.Profunctor.Row`, *What is
-- | deliberately not here*).
class Profunctor p <= Resolving p where
  resolve :: forall a b c. p a b -> p (Tuple a c) (Either b c)
