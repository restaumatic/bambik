-- | Sum-side **introduce** primitive — the `Void`-pinned, irreducible row profunctor.
-- |
-- | Where `RowChoice` (the row-typed `Choice`) covers focusing and eliminating an existing
-- | case, *introducing* a new case is genuinely outside `Choice`:
-- |
-- | ```
-- | liftIntroVar :: p Void r -> p s (Either s r)   -- "r introduced, s preserved"
-- | ```
-- |
-- | The source `p Void r` is pinned to the **initial** object `Void` and emits the new case
-- | spontaneously — there is no input for `Choice`'s `right` to dispatch on, so
-- | `Choice ⇏ IntroVarP`. This is the one row-profunctor capability not derivable from
-- | `Strong`/`Choice`; everything else in `RowToRow` reduces to `RowStrong`/`RowChoice`.
module Data.Profunctor.RowToRow.IntroVarP
  ( class IntroVarP
  , liftIntroVar
  ) where

import Data.Either (Either)
import Data.Profunctor (class Profunctor)
import Data.Void (Void)

-- | Introduce a static, context-independent value `r` into the output sum, preserving
-- | the input `s` as the `Left` alternative. The source `p Void r` is always-available
-- | and not driven by the input (it may still change on its own, e.g. a button or a clock).
class Profunctor p <= IntroVarP p where
  liftIntroVar :: forall s r. p Void r -> p s (Either s r) -- r introduced, s preserved
