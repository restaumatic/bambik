-- | `Variant → Variant` row profunctors, organized (uniformly across the four
-- | direction modules) as:
-- |
-- |   * **strength** — `Choice` (ecosystem class, imported): the unary power,
-- |     minimal and interop-friendly.
-- |   * **direction class** — `VariantToVariant`, the binary **merge**: the one
-- |     genuine per-carrier primitive.
-- |   * **free functions over the strength** — everything else: `focusVariant`
-- |     (sub-variant focus), `case_` (case prism), `caseToVariant` (absorb one
-- |     input case), `prismE`.
-- |
-- | Law connecting the two classes, for carriers with `identity :: p a a`:
-- | the unary introduce operator is the **identity-pinned merge**,
-- |
-- | ```
-- | caseToVariant @l g = variantToVariant (lcmap unwrap g) identity
-- |   where unwrap :: [ l :: f ] -> f   -- eliminate the singleton variant
-- | ```
-- |
-- | and conversely a merge is an iterated chain of single-case steps
-- | (see doc/row-profunctors.md, "The precise correspondence").
-- |
-- | (Introducing a *fresh* case is the one operation outside `Choice`:
-- | `Choice`'s `left`/`right` are *gated* — they fire only on a selected input
-- | branch — but an introduced case has no input selector, so it can never be
-- | emitted by `left`/`right`, even given a producer. Contrast `Strong`'s
-- | ungated `second`, which always emits its field, hence `recordToProperty`
-- | exists and `introduceCase` cannot — not here: the `× → +` direction has it
-- | as `RecordToVariant.recordToCase`. Built instead via the
-- | `Sum`/`variantToVariant` path, not a focus combinator.)
module Data.Profunctor.Row.VariantToVariant
  ( bind
  , variantToVariant
  , class VariantToVariant
  , discard
  , focusVariant
  , prismE
  , case_
  , caseToVariant
  , splitVariant
  )
  where

import Control.Category (identity)
import Data.Either (Either(..), either)
import Data.Lens (Prism)
import Data.Maybe (Maybe(..))
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Choice (class Choice, left)
import Data.Symbol (class IsSymbol)
import Data.Unit (Unit, unit)
import Data.Variant (class Contractable, contract, expand, inj, on)
import Effect.Exception.Unsafe (unsafeThrow)
import Prim.Row (class Cons, class Union)
import Type.Proxy (Proxy(..))
import Type.Row.Constraints (class DispatchableVariants, class ExclusiveRows, class InclusiveRows)

class Profunctor p <= VariantToVariant p where
  variantToVariant :: forall i1 i1l i2 i2l o1 o2 o12 o1x o2x i o.
    ExclusiveRows i1 i2 i =>
    InclusiveRows o1 o2 o o12 o1x o2x =>
    DispatchableVariants i1 i2 i1l i2l =>
    p [ | i1 ] [ | o1 ] -> p [ | i2 ] [ | o2 ] -> p [ | i ] [ | o ]

bind :: forall f i1 i1l i2 i2l o1 o2 o12 o1x o2x i o.
  VariantToVariant f =>
  ExclusiveRows i1 i2 i =>
  InclusiveRows o1 o2 o o12 o1x o2x =>
  DispatchableVariants i1 i2 i1l i2l =>
  f [ | i1 ] [ | o1 ] -> (f [ | i1 ] [ | o1 ] -> f [ | i2 ] [ | o2 ]) -> f [ | i ] [ | o ]
bind first cont = variantToVariant first (cont first)

discard :: forall f i1 i1l i2 i2l o1 o2 o12 o1x o2x i o.
  VariantToVariant f =>
  ExclusiveRows i1 i2 i =>
  InclusiveRows o1 o2 o o12 o1x o2x =>
  DispatchableVariants i1 i2 i1l i2l =>
  f [ | i1 ] [ | o1 ] -> (Unit -> f [ | i2 ] [ | o2 ]) -> f [ | i ] [ | o ]
discard first cont = bind first (\_ -> cont unit)

-- | Row-typed `Choice`: focus a **sub-variant** `sub`, transforming it while carrying the
-- | complement `rest` of the cases unchanged. The coproduct dual of `focusRecord`
-- | — operates on rows on **both sides**:
-- |
-- | ```
-- | focusVariant :: p [ | sub ] [ | sub' ] -> p [ | s ] [ | t ]
-- |               -- where s = sub ∪ rest,  t = sub' ∪ rest   (ExclusiveRows)
-- | ```
-- |
-- | The labeled analogue of `Choice`'s `left`/`right`, carrying the complement *row* `rest`.
-- | Plain `Choice` underneath: dispatch `s` into `sub | rest` (via
-- | `Data.Variant.contract`), run the argument on the `sub` branch via `left`, and re-merge
-- | both branches into `t` (via `expand`).
focusVariant
  :: forall p sub sub' rest s t
   . Choice p
  => ExclusiveRows sub rest s
  => ExclusiveRows sub' rest t
  => Contractable s sub
  => Contractable s rest
  => p [ | sub ] [ | sub' ]
  -> p [ | s ] [ | t ]
focusVariant g = dimap splitVariant (either expand expand) (left g)

-- Dispatch a wider variant into the focused sub-variant or the complement.
splitVariant
  :: forall sub rest s
   . ExclusiveRows sub rest s
  => Contractable s sub
  => Contractable s rest
  => [ | s ]
  -> Either [ | sub ] [ | rest ]
splitVariant v = case contract v of
  Just sub -> Left sub
  Nothing -> case contract v of
    Just rest -> Right rest
    Nothing -> unsafeThrow "focusVariant: case in neither sub nor rest"

-- | Construct a `Prism` straight from its **existential encoding**
-- | `∃c. (s → a + c) × (b + c → t)`: pick the residual `c`, then supply `decon`
-- | (match `s` as the focus `a` or the complement `c`) and `recon` (rebuild `t`
-- | from the built `b` or that same complement `c`). The quantified `c` is the
-- | eliminator of that existential; `left` (`Choice`) is the carrier. The standard
-- | `Data.Lens.prism` is this at the co-Yoneda witness `c := t`. Mirror of
-- | `lensE` (first), `shutterE` (resolve), `reelE` (retain).
prismE :: forall s t a b c. (s -> Either a c) -> (Either b c -> t) -> Prism s t a b
prismE decon recon g = dimap decon recon (left g)

-- | Focus an existing case in place — the standard `Choice` case prism,
-- | type-changing: focus `a → b` turns row `s` into `t` (same rows except at
-- | `l`, witnessed by the shared remainder `rest`; `Union rest mix t` lets the
-- | untouched complement `expand` into the new row). `b := a` recovers the
-- | simple `p a a -> p [ | s ] [ | s ]` form. Built via `prismE` at
-- | `c := [ | rest ]`.
case_
  :: forall @l p s t a b rest mix
   . IsSymbol l
  => Cons l a rest s
  => Cons l b rest t
  => Union rest mix t
  => Choice p
  => p a b -> p [ | s ] [ | t ]
case_ =
  prismE
    (on (Proxy @l) Left Right)
    (either (inj (Proxy @l)) expand)

-- | Accept an **extra** input case `l :: f` (the **focus**) and dispatch it
-- | into the remaining cases (the **background** `b`; the grown input is the
-- | **shot** `s`): the wrapped `p f [ | b ]` consumes the new case's value and emits the
-- | whole output variant itself (deciding which case the adapted event becomes);
-- | every other case passes through untouched via the sum codiagonal. An event
-- | adapter/normalizer. The exact dual of `recordToProperty`: the whole row
-- | sits at the wrapped profunctor's *output* end here, at its *input* end
-- | there — sums grow the input row, products grow the output row. (Introducing
-- | an *output* case is the impossible direction — gated `left` could never
-- | emit it — but introducing an *input* case is exactly this.) Pinning the
-- | dispatch unreachable eliminates the case outright:
-- | `caseToVariant @l (rmap absurd (sink :: p f Void))` drops case `l`,
-- | passing the survivors through.
caseToVariant
  :: forall @l p b s f
   . IsSymbol l
  => Cons l f b s
  => Choice p
  => p f [ | b ]
  -> p [ | s ] [ | b ]
caseToVariant g =
  dimap (on (Proxy @l) Left Right) (either identity identity) (left g)
