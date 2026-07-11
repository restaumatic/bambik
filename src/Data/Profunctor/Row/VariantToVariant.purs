-- | `Variant → Variant` row profunctors: the direction class
-- | `VariantToVariant` — the binary **merge**, the one genuine per-carrier
-- | primitive — with its qualified-do sugar. One-at-a-time events dispatch to
-- | the operand handling their case (`ExclusiveRows` on input: exactly one
-- | handler per case); outputs may overlap (`InclusiveRows`). Over ecosystem
-- | `Choice`: `case_` (the value-level case prism, via `prismE`) and
-- | `splitVariant` (the focus/background dispatch `reelWrap` shares).
-- |
-- | The **nullary** operator is the merge **unit** — the class's own
-- | `pempty :: p (Variant ()) (Variant ())`: `variantToVariant pempty g = g`.
-- | Here silence is not merely lawful but forced — both empty-variant ends
-- | are uninhabited, so the unit can neither receive nor emit — and any
-- | silent element implements it (`UI`: `pempty = silence`).
module Data.Profunctor.Row.VariantToVariant
  ( bind
  , variantToVariant
  , case_
  , class VariantToVariant
  , discard
  , pempty
  , prismE
  , splitVariant
  )
  where

import Data.Either (Either(..), either)
import Data.Lens (Prism)
import Data.Maybe (Maybe(..))
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Choice (class Choice, left)
import Data.Symbol (class IsSymbol)
import Data.Unit (Unit, unit)
import Data.Variant (class Contractable, Variant, contract, expand, inj, on)
import Effect.Exception.Unsafe (unsafeThrow)
import Prim.Row (class Cons, class Union)
import Type.Proxy (Proxy(..))
import Data.Profunctor.Row (class DispatchableVariants, class ExclusiveRows, class InclusiveRows)

class Profunctor p <= VariantToVariant p where
  variantToVariant :: forall i1 i1l i2 i2l o1 o2 o12 o1x o2x i o.
    ExclusiveRows i1 i2 i =>
    InclusiveRows o1 o2 o o12 o1x o2x =>
    DispatchableVariants i1 i2 i1l i2l =>
    p [ | i1 ] [ | o1 ] -> p [ | i2 ] [ | o2 ] -> p [ | i ] [ | o ]
  -- | The **nullary** merge — the unit: handles no cases, emits no cases.
  -- | Both empty-variant ends are uninhabited, so silence is forced — any
  -- | silent element implements it (`UI`: `pempty = silence`).
  pempty :: p (Variant ()) (Variant ())

bind :: forall p i1 i1l i2 i2l o1 o2 o12 o1x o2x i o.
  VariantToVariant p =>
  ExclusiveRows i1 i2 i =>
  InclusiveRows o1 o2 o o12 o1x o2x =>
  DispatchableVariants i1 i2 i1l i2l =>
  p [ | i1 ] [ | o1 ] -> (p [ | i1 ] [ | o1 ] -> p [ | i2 ] [ | o2 ]) -> p [ | i ] [ | o ]
bind first cont = variantToVariant first (cont first)

discard :: forall p i1 i1l i2 i2l o1 o2 o12 o1x o2x i o.
  VariantToVariant p =>
  ExclusiveRows i1 i2 i =>
  InclusiveRows o1 o2 o o12 o1x o2x =>
  DispatchableVariants i1 i2 i1l i2l =>
  p [ | i1 ] [ | o1 ] -> (Unit -> p [ | i2 ] [ | o2 ]) -> p [ | i ] [ | o ]
discard first cont = bind first (\_ -> cont unit)

-- Dispatch a shot into the focused sub-variant or the background.
splitVariant
  :: forall f b s
   . ExclusiveRows f b s
  => Contractable s f
  => Contractable s b
  => [ | s ]
  -> Either [ | f ] [ | b ]
splitVariant v = case contract v of
  Just f -> Left f
  Nothing -> case contract v of
    Just b -> Right b
    Nothing -> unsafeThrow "splitVariant: case in neither focus nor background"

-- | Construct a `Prism` straight from its **existential encoding**
-- | `∃c. (s → a + c) × (b + c → t)`: pick the residual `c`, then supply `decon`
-- | (match `s` as the focus `a` or the complement `c`) and `recon` (rebuild `t`
-- | from the built `b` or that same complement `c`). The quantified `c` is the
-- | eliminator of that existential; `left` (`Choice`) is the carrier. The standard
-- | `Data.Lens.prism` is this at the co-Yoneda witness `c := t`.
prismE :: forall s t a b c. (s -> Either a c) -> (Either b c -> t) -> Prism s t a b
prismE decon recon g = dimap decon recon (left g)

-- | Focus an existing case in place — the standard `Choice` case prism, read
-- | photographically as **refocusing**: the **focus** `f → f'` changes, the
-- | **background** `b` stays, so the **shot** `s` becomes `s'` (`Union b mix s'`
-- | lets the untouched background `expand` into the new row). `f' := f`
-- | recovers the simple `p f f -> p [ | s ] [ | s ]` form. Built via `prismE`
-- | at `c := [ | b ]`.
case_
  :: forall @l p f f' b s s' mix
   . IsSymbol l
  => Cons l f b s
  => Cons l f' b s'
  => Union b mix s'
  => Choice p
  => p f f' -> p [ | s ] [ | s' ]
case_ =
  prismE
    (on (Proxy @l) Left Right)
    (either (inj (Proxy @l)) expand)
