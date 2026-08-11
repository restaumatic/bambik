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
-- | emission resolves (`Left`) at quiescence — so
-- | `coresolve (resolve g) = debounced g ≅ g` up to time, once primed.
-- | (No `(->)` instance: a timeless carrier could only give the trivial
-- | always-`Done` step, which carries no iteration.)
-- |
-- | This is the **bare strength** for the `× → +` direction (the analogue of
-- | `Strong`/`Choice`); the row combinator built on it is
-- | `RecordToVariant.subResolving` — exactly as `RecordToRecord.subStrong` is
-- | built on `Strong`.
class Profunctor p <= Resolving p where
  resolve :: forall a b c. p a b -> p (Tuple a c) (Either b c)
