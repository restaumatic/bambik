-- | Sum-side **introduce** primitive — the `Void`-pinned half of `Choice`.
-- |
-- | 2×2×pin position: row = sum (coproduct, `Either`/`Variant`), column = introduce
-- | (grow the output). The source's input is pinned to the **initial** object `Void`:
-- |
-- | ```
-- | liftIntroVar :: p Void r -> p s (Either s r)   -- "r introduced, s preserved"
-- | ```
-- |
-- | Unlike the product introduce primitive (which lives in `HalfOptic.Property` and is
-- | just `Strong`), this `Void` pin is **forced by case-exclusivity**: when the introduced
-- | case is active, every existing case in `s` is absent, so the source has no sibling to
-- | read. Crucially it does **not** fold onto `Choice` either: `right` only fires its branch
-- | on a `Right` input, but the introduced case is never an input — the source `p Void r`
-- | emits it spontaneously — so `Choice ⇏ IntroVarP`. `IntroVarP` is therefore *incomparable*
-- | to `Choice` and is the one genuinely irreducible half-optic class.
-- |
-- | Its transpose, sum-eliminate, behaves oppositely: it *does* fold onto `Choice` (via
-- | `left`), which is why there is no `ExceptP` class — see `HalfOptic.Case.eliminateCase`.
module Data.Profunctor.HalfOptic.IntroVarP
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
