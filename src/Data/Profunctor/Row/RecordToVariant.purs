-- | `Record → Variant` (× → +) row profunctors, organized (uniformly across
-- | the four direction modules) as:
-- |
-- |   * **strength** — `Resolving` (defined here; `UI m` instances only, no
-- |     `(->)`): the unary power, a loop/iteration step.
-- |   * **direction class** — `RecordToVariant`, the binary **merge**: the one
-- |     genuine per-carrier primitive.
-- |   * **free functions over the strength** — everything else: `shutterWrap`
-- |     (sub-record focus), `resolveProperty` (thread one label),
-- |     `propertyToCase` (single-field focus), `recordToCase` (introduce; mere
-- |     `Profunctor`), the `Shutter` optic with `shutter`/`shutterE`.
-- |
-- | Law connecting the two classes: the mixed directions have no `identity` to
-- | pin (nothing inhabits a mode-crossing diagonal), but they have a **unit** —
-- | `pzero :: p {} []` (cf. `Data.Profunctor.Zero`), the silent source. The
-- | unary introduce operator is the **unit-pinned merge**,
-- |
-- | ```
-- | recordToCase @l g = recordToVariant (rmap (inj (Proxy @l)) g) pzero
-- | ```
-- |
-- | and a pinned unit contributes nothing — which is why `recordToCase`
-- | collapses to plain `rmap (inj l)` on any `Profunctor`.
module Data.Profunctor.Row.RecordToVariant
  ( Shutter
  , bind
  , class RecordToVariant
  , discard
  , recordToVariant
  , class Resolving
  , propertyToCase
  , recordToCase
  , resolve
  , resolveProperty
  , shutter
  , shutterE
  , shutterWrap
  )
  where

import Data.Either (Either, either)
import Data.Profunctor (class Profunctor, dimap, rmap)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Data.Variant (expand, inj)
import Prim.Row (class Cons, class Lacks, class Union)
import Record (delete, get)
import Type.Proxy (Proxy(..))
import Type.Row.Constraints (class ExclusiveRows, class InclusiveRows)
import Unsafe.Coerce (unsafeCoerce)

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
-- | of the binary base merge `Data.Profunctor.ProductToSum.prosum` (its second
-- | operand fixed to `identity`) — the product→sum analogue of how
-- | `focusRecord` is the unary form of `recordToRecord`.
-- |
-- | (No `(->)` instance: the only one would be the trivial always-`Done` step,
-- | which carries no iteration — this class is for profunctors that actually loop.)
-- |
-- | This is the **bare strength** for the `× → +` direction (the analogue of
-- | `Strong`/`Choice`); the row combinator built on it is `shutterWrap` below —
-- | exactly as `focusRecord` is built on `Strong`.
class Profunctor p <= Resolving p where
  resolve :: forall a b c. p a b -> p (Tuple a c) (Either b c)

class Profunctor p <= RecordToVariant p where
  recordToVariant :: forall i1 o1 i2 o2 i12 i1x i2x o12 o1x o2x i o.
    InclusiveRows i1 i2 i i12 i1x i2x =>
    InclusiveRows o1 o2 o o12 o1x o2x =>
    p { | i1 } [ | o1 ] -> p { | i2 } [ | o2 ] -> p { | i } [ | o ]

bind :: forall f i1 o1 i2 o2 i12 i1x i2x o12 o1x o2x i o.
  RecordToVariant f =>
  InclusiveRows i1 i2 i i12 i1x i2x =>
  InclusiveRows o1 o2 o o12 o1x o2x =>
  f { | i1 } [ | o1 ] -> (f { | i1 } [ | o1 ] -> f { | i2 } [ | o2 ]) -> f { | i } [ | o ]
bind first cont = recordToVariant first (cont first)

discard :: forall f i1 o1 i2 o2 i12 i1x i2x o12 o1x o2x i o.
  RecordToVariant f =>
  InclusiveRows i1 i2 i i12 i1x i2x =>
  InclusiveRows o1 o2 o o12 o1x o2x =>
  f { | i1 } [ | o1 ] -> (Unit -> f { | i2 } [ | o2 ]) -> f { | i } [ | o ]
discard first cont = bind first (\_ -> cont unit)

-- | Single-field specialization of `resolve` — the `edit`-position combinator
-- | for this direction (the analogue of `property`/`case_` when input and
-- | output kinds differ). It threads one label `l` as **input field ↔ output
-- | case**: field `l :: x` is split off the input record, and the wrapped
-- | profunctor either runs on the rest (emitting some case of `o`, the `Done`
-- | branch) or the field's value escapes directly as output case `l` (the
-- | `Loop`/short-circuit branch).
resolveProperty
  :: forall @l p x lo i i' o o'
   . Resolving p
  => IsSymbol l
  => Cons l x i i'
  => Lacks l i
  => Cons l x o o'
  => Cons l x () lo
  => Union o lo o'
  => p { | i } [ | o ]
  -> p { | i' } [ | o' ]
resolveProperty g =
  dimap
    (\s -> Tuple (delete (Proxy @l) s) (get (Proxy @l) s))
    (either expand (inj (Proxy @l)))
    (resolve g)

-- | The single-field **focus** for this direction — the `× → +` analogue of
-- | `property` (row-typed `first`), built on `resolve` exactly as `property` is
-- | built on `first`. Field `l` of the input record is the focus fed to the
-- | wrapped `p a b`; the leftover `{ | r }` cannot stay a record inside the
-- | `Variant` output, so — as in `shutterWrap` — it is wrapped as a single
-- | output case `w`: `Done b` emits case `l`, the `Loop`/escape branch emits
-- | case `w` carrying the untouched rest. The single-field form of
-- | `shutterWrap`; the transpose of `resolveProperty`, which runs the wrapped
-- | profunctor on the *rest* and lets the focused field escape.
propertyToCase
  :: forall @l @w p s r a b lx wx t
   . Resolving p
  => IsSymbol l
  => IsSymbol w
  => Cons l a r s
  => Lacks l r
  => Cons l b lx t
  => Cons w { | r } wx t
  => p a b
  -> p { | s } [ | t ]
propertyToCase g =
  dimap
    (\s -> Tuple (get (Proxy @l) s) (delete (Proxy @l) s))
    (either (inj (Proxy @l)) (inj (Proxy @w)))
    (resolve g)

-- | The `× → +` member of the introduce family: the wrapped `p { | r } f` reads
-- | the whole record — `r`, the **reality** the camera is pointed at, which
-- | never enters the shot — and its result, the **focus**
-- | `f`, is emitted as
-- | output case `l`. This is the `introduceCase` that `VariantToVariant`
-- | documents as impossible — there, a fresh output case must coexist with
-- | gated pass-through cases and can never fire; here nothing else emits, the
-- | computed case fires unconditionally, and no strength is needed at all:
-- | plain `rmap (inj l)` on any `Profunctor`. (The **background** `b` of the
-- | output **shot** `s` is simply never produced — the widening is free, as
-- | with `inj` itself.)
recordToCase
  :: forall @l p r b s f
   . IsSymbol l
  => Cons l f b s
  => Profunctor p
  => p { | r } f
  -> p { | r } [ | s ]
recordToCase = rmap (inj (Proxy @l))

-- | The optic `resolve` induces: the **Shutter**. Eliminating the residual `c`
-- | (instantiated to `s`) by co-Yoneda collapses `∃c. (s → a × c) × (b + c → t)`
-- | to `(view : s → a) × (build : b → t) × (escape : s → t)` — a lens that can
-- | *snap shut*: run the focus and `build` (the `Done` branch), or `escape`
-- | straight to `t` (the `Loop`/short-circuit). Like a camera shutter: it opens,
-- | loops while held, then snaps to a single captured value.
type Shutter s t a b = forall p. Resolving p => p a b -> p s t

shutter :: forall s t a b. (s -> a) -> (b -> t) -> (s -> t) -> Shutter s t a b
shutter view build escape g = shutterE (\s -> Tuple (view s) s) (either build escape) g

-- | Construct a `Shutter` straight from its **existential encoding**
-- | `∃c. (s → a × c) × (b + c → t)`: pick the residual `c`, then supply `decon`
-- | (split `s` into a focus `a` and the residual `c`) and `recon` (rebuild `t`
-- | from the focus result `b` — the `Done` branch — *or* the residual `c` — the
-- | `Loop`/escape branch). The quantified `c` is exactly the eliminator of that
-- | existential; `resolve` is the carrier that threads `c`. `shutter` is this at
-- | the co-Yoneda witness `c := s` (`decon = \s -> Tuple (view s) s`,
-- | `recon = either build escape`).
shutterE :: forall s t a b c. (s -> Tuple a c) -> (Either b c -> t) -> Shutter s t a b
shutterE decon recon g = dimap decon recon (resolve g)

-- | Row existential `Shutter` focusing a whole **sub-Record `i`** of the full
-- | input `i'`; the residual is the **rest** `{ | rest }` (`ExclusiveRows i rest
-- | i'`, the same split `focusRecord` uses). Crossing `× → +`, the rest can't stay
-- | a record in the `Variant` output, so it is **wrapped as a single output case
-- | `w`** — a variant carrying the record (`o' = o` plus case `w`). The inner
-- | `p { | i } [ | o ]` runs on the focus: `Done` expands its result into
-- | `o'`, `Loop` injects the retained rest-record into case `w`. The mixed-direction
-- | analogue of `focusRecord` — same sub-record focus, but the complement is
-- | *wrapped* to cross into the variant output rather than carried same-kind.
-- | The `× → +` row combinator over the bare strength `Resolving`, just as
-- | `focusRecord` is the row combinator over `Strong`.
-- |
-- | ```purescript
-- | -- focus (item, qty); wrap the leftover { note } into output case `draft`
-- | checkout :: Shutter
-- |   { item :: String, qty :: Int, note :: String }              -- i'  full input
-- |   [ priced :: Int, draft :: { note :: String } ]              -- o'  full output
-- |   { item :: String, qty :: Int }                              -- i   sub-Record focus
-- |   [ priced :: Int ]                                            -- o   inner output
-- | checkout = shutterWrap @"draft"
-- | ```
shutterWrap
  :: forall @w p i i' rest o o' mix
   . Resolving p
  => IsSymbol w
  => ExclusiveRows i rest i'
  => Cons w { | rest } o o'
  => Union o mix o'
  => p { | i } [ | o ]
  -> p { | i' } [ | o' ]
shutterWrap g =
  shutterE
    (\s -> Tuple (unsafeCoerce s) (unsafeCoerce s))
    (either expand (inj (Proxy @w)))
    g