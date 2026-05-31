-- | Sum-side half-optics over `Variant`s — the transpose-duals of `HalfOptic.Property`.
-- |
-- | 2×2×pin position: row = sum (`Either`/`Variant`), so the ambient class is `Choice`
-- | (its `left`/`right` are the coproduct analogues of `Strong`'s `first`/`second`):
-- |
-- |   * `eliminateCase` — eliminate (consume): folds onto **`Choice`** via `left`
-- |     (`Choice ⇒ ExceptP`, exactly as product eliminate folds onto `Strong` via `first`).
-- |     Takes a diverging `p case Void` handler; the eliminated case is routed `Left` and
-- |     exits through the `Void` slot, the survivors pass `Right`.
-- |   * `focusCase`     — focus an existing case: the standard **`Choice`** prism (`right`),
-- |     reused from `Data.Lens.Extra.Commons.variant`.
-- |   * `introduceCase` — introduce (grow): the one genuinely irreducible primitive. Its
-- |     source `p Void case` has no input for `Choice` to dispatch on (the new case fires
-- |     spontaneously), so `Choice ⇏ IntroVarP`; it keeps its own `Void`-pinned class.
-- |
-- | So of the four sum operations, three reference `Choice` directly (`left` for eliminate,
-- | `right`/prism for focus) and only sum-introduce needs the bespoke `IntroVarP`.
module Data.Profunctor.HalfOptic.Case
  ( introduceCase
  , eliminateCase
  , focusCase
  ) where

import Prelude

import Data.Either (Either(..), either)
import Data.Lens (Optic, Prism)
import Data.Lens.Extra.Commons (variant) as Commons
import Data.Profunctor (dimap, lcmap, rmap)
import Data.Profunctor.Choice (class Choice, left)
import Data.Profunctor.HalfOptic.IntroVarP (class IntroVarP, liftIntroVar)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant, expand, inj, on)
import Prim.Row (class Cons, class Union)
import Type.Proxy (Proxy(..))

-- | Introduce a new case `l` from a static source, preserving the existing cases.
-- | The only sum half-optic that is *not* `Choice` — `liftIntroVar` injects a case that
-- | the input never carries, which `Choice`'s input dispatch cannot do. The free `Unit→Void`
-- | step is the inlined `lcmap absurd`.
introduceCase
  :: forall p @l case_ s t r
   . IsSymbol l
  => Cons l case_ s t
  => Union s r t
  => IntroVarP p
  => Optic p (Variant s) (Variant t) (Record ()) case_
introduceCase src =
  rmap
    (case _ of
       Left vars -> expand vars
       Right i -> inj (Proxy @l) i)
    (liftIntroVar (lcmap absurd src))

-- | Eliminate the case `l` via a diverging handler `p case Void`, preserving the rest.
-- | Folds onto `Choice`: `left` runs the handler on the routed `Left` case (its `Void`
-- | output is `absurd`) and passes the survivors through `Right`.
eliminateCase
  :: forall p @l case_ s t
   . IsSymbol l
  => Cons l case_ t s
  => Choice p
  => p case_ Void
  -> p (Variant s) (Variant t)
eliminateCase handler =
  dimap (on (Proxy @l) Left Right) (either absurd identity) (left handler)

-- | Focus an existing case in place — the standard `Choice` prism (from `Commons.variant`).
focusCase
  :: forall @l s r a
   . IsSymbol l
  => Cons l a r s
  => Prism (Variant s) (Variant s) a a
focusCase = Commons.variant @l
