-- | `Record → Variant` (× → +) row profunctors, organized (uniformly across
-- | the four direction modules) as:
-- |
-- |   * **strength** — `Resolving` (defined here; `PUI m` instances only, no
-- |     `(->)`): the unary power, a loop/iteration step.
-- |   * **direction class** — `RecordToVariant`, the binary **merge**: the one
-- |     genuine per-carrier primitive.
-- |   * **free functions over the strength** — everything else: `shutterWrap`
-- |     (sub-record focus), `resolveProperty` (thread one label),
-- |     `propertyToCase` (single-field focus), `recordToCase` (introduce; mere
-- |     `Profunctor`), the `Shutter` optic with `shutter`/`shutterE`, the
-- |     `Coshutter` optic with `coshutter`/`coshutterE` (the reversed
-- |     `Reel`) — and
-- |     over the co-strength `Coresolving`: `folding @w` (the terminating
-- |     fold at row granularity).
-- |
-- | Law connecting the two classes: the mixed directions have no `identity` to
-- | pin (nothing inhabits a mode-crossing diagonal), but they have the class's
-- | own **unit** `pempty :: p {} (Variant ())`, the silent source. The unary
-- | introduce operator is the **unit-pinned merge**,
-- |
-- | ```
-- | recordToCase @l g = recordToVariant (rmap (inj (Proxy @l)) g) pempty
-- | ```
-- |
-- | and a pinned unit contributes nothing — which is why `recordToCase`
-- | collapses to plain `rmap (inj l)` on any `Profunctor`.
-- |
-- | As nullary operator, `pempty` is the empty merge:
-- | `recordToVariant pempty g = g`. Silence is forced on the output end (the
-- | empty variant is uninhabited) and sufficient on the input end (the empty
-- | record demands nothing), so `PUI` implements it as its silent widget:
-- | `pempty = silence`.
module Data.Profunctor.Row.RecordToVariant
  ( Coshutter
  , coshutter
  , coshutterE
  , Shutter
  , bind
  , class Coresolving
  , class RecordToVariant
  , coresolve
  , discard
  , folding
  , pempty
  , recordToVariant
  , class Resolving
  , propertyToCase
  , echoCase
  , asCase
  , recordToCase
  , toCase
  , resolve
  , resolveProperty
  , shutter
  , shutterE
  , shutterWrap
  )
  where

import Data.Either (Either(..), either)
import Control.Category (class Category, identity)
import Control.Semigroupoid ((>>>))
import Data.Profunctor (class Profunctor, dimap, rmap)
import Data.Profunctor.Seeding (class Seeding, seeded)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Data.Variant (Variant, case_, expand, inj, on)
import Prim.Row (class Cons, class Union)
import Record (get)
import Record (union) as Record
import Record.Unsafe (unsafeDelete)
import Type.Proxy (Proxy(..))
import Data.Profunctor.Row (class ExclusiveRows, class SharedRecordInputs, class SharedVariantOutputs)
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
-- | of the positional product→sum base merge
-- | `p a b -> p c d -> p (Tuple a c) (Either b d)` (its second operand fixed
-- | to `identity`) — the product→sum analogue of how `focusRecord` is the
-- | unary form of `recordToRecord`.
-- |
-- | With no out-of-band loop signal in the wire protocol (values are just
-- | values), the `PUI` instance derives the branch **from time**: every
-- | emission loops (`Right`) while the widget is still moving, and the last
-- | emission resolves (`Left`) at quiescence — so
-- | `coresolve (resolve g) = debounced g ≅ g` up to time, once primed.
-- | (No `(->)` instance: a timeless carrier could only give the trivial
-- | always-`Done` step, which carries no iteration.)
-- |
-- | This is the **bare strength** for the `× → +` direction (the analogue of
-- | `Strong`/`Choice`); the row combinator built on it is `shutterWrap` below —
-- | exactly as `focusRecord` is built on `Strong`.
class Profunctor p <= Resolving p where
  resolve :: forall a b c. p a b -> p (Tuple a c) (Either b c)

-- | The **co-strength** of `Resolving` — its retraction: where `resolve`
-- | *adds* the loop channel `c`, `coresolve` *ties* it. A `Right c` emission
-- | is retained as the state paired with subsequent inputs; a `Left b` exits.
-- | Semantically a **terminating fold**: inputs accumulate through `c` until
-- | the wrapped profunctor decides `b` — the fourth loop flavor in the trace
-- | quartet (`Costrong` = state that emits each step, `Cochoice` = control
-- | that emits at exit, `Coresolving` = state that emits at exit,
-- | `Coretaining` = control that emits each step).
-- |
-- | Retraction law, shared by all four traces: `coresolve (resolve g) ≅ g` —
-- | once the state channel is primed (state must enter somewhere; the `PUI`
-- | instance is knowledge-gated like `Costrong`, withholding inputs until a
-- | first `c` exists).
-- |
-- | (No `(->)` instance: tying a knot takes state.)
class Profunctor p <= Coresolving p where
  coresolve :: forall a b c. p (Tuple a c) (Either b c) -> p a b

-- | `coresolve` at row granularity — the **terminating fold** with labeled
-- | channels: the wrapped profunctor sees its input joined with the folded
-- | state sub-record `fb`, and answers with a variant that either continues
-- | the fold (case `w`, carrying the next `{ | fb }` — retained silently)
-- | or exits (any `done` case — emitted). The `× → +` co-analogue of
-- | `shutterWrap`: there the background is wrapped as case `w` to *escape*,
-- | here case `w` is unwrapped to *loop*. No coercions: `on` splits the
-- | output variant exactly.
-- |
-- | The fold state is an **entity** — it exists from the fold's very
-- | beginning — and `folding` takes its t=0 value `{ | fb }` as the first
-- | argument: at registration the seed is emitted once as case `w` (a
-- | `seeded` wire composed onto the output), priming the state channel
-- | before any input arrives — a `folding` stage never starves.
-- | Emission-primed exotica remain expressible with raw
-- | `coresolve`/`coshutter`.
folding
  :: forall @w p i fb iw done ow
   . Seeding p
  => Coresolving p
  => IsSymbol w
  => ExclusiveRows i fb iw
  => Cons w { | fb } done ow
  => { | fb }
  -> p { | iw } [ | ow ]
  -> p { | i } [ | done ]
folding seed g =
  coresolve
    (dimap
      (\(Tuple i fb) -> Record.union i fb)
      (on (Proxy @w) Right Left)
      (g >>> seeded (inj (Proxy @w) seed)))

-- | The optic `coresolve` induces: the **Coshutter** — the `Reel` run
-- | backwards (`Coshutter s t a b ≅ Reel b a t s`). Eliminating the residual
-- | `c` (instantiated to `s → a`) by co-Yoneda collapses
-- | `∃c. (s × c → a) × (b → t + c)` to a single `step : b → t + (s → a)`:
-- | each emission either exits with `t` or yields a **new way to read
-- | inputs** — the fold state is a reader. The collapsed form has no initial
-- | reader, which is exactly why the `PUI` carrier gates inputs until primed.
-- | `folding @w` is this optic at row granularity.
type Coshutter s t a b = forall p. Coresolving p => p a b -> p s t

coshutter :: forall s t a b. (b -> Either t (s -> a)) -> Coshutter s t a b
coshutter step = coshutterE (\(Tuple s f) -> f s) step

-- | Construct a `Coshutter` straight from its **existential encoding**
-- | `∃c. (s × c → a) × (b → t + c)`: pick the fold channel `c`, then supply
-- | `decon` (read the input joined with the fold state) and `recon` (exit or
-- | continue each emission). `coshutter` is this at the co-Yoneda witness
-- | `c := s → a`.
coshutterE :: forall s t a b c. (Tuple s c -> a) -> (b -> Either t c) -> Coshutter s t a b
coshutterE decon recon g = coresolve (dimap decon recon g)

class Profunctor p <= RecordToVariant p where
  recordToVariant :: forall i1 o1 i2 o2 i12 i1x i2x o12 o1x o2x i o.
    SharedRecordInputs i1 i2 i i12 i1x i2x =>
    SharedVariantOutputs o1 o2 o o12 o1x o2x =>
    p { | i1 } [ | o1 ] -> p { | i2 } [ | o2 ] -> p { | i } [ | o ]
  -- | The **nullary** merge — the unit: reads nothing, emits no cases. The
  -- | silent source of the header's law; silence is forced on the uninhabited
  -- | variant output and sufficient on the empty record input, so any silent
  -- | element implements it (`PUI`: `pempty = silence`).
  pempty :: p {} (Variant ())

bind :: forall p i1 o1 i2 o2 i12 i1x i2x o12 o1x o2x i o.
  RecordToVariant p =>
  SharedRecordInputs i1 i2 i i12 i1x i2x =>
  SharedVariantOutputs o1 o2 o o12 o1x o2x =>
  p { | i1 } [ | o1 ] -> (p { | i1 } [ | o1 ] -> p { | i2 } [ | o2 ]) -> p { | i } [ | o ]
bind first cont = recordToVariant first (cont first)

discard :: forall p i1 o1 i2 o2 i12 i1x i2x o12 o1x o2x i o.
  RecordToVariant p =>
  SharedRecordInputs i1 i2 i i12 i1x i2x =>
  SharedVariantOutputs o1 o2 o o12 o1x o2x =>
  p { | i1 } [ | o1 ] -> (Unit -> p { | i2 } [ | o2 ]) -> p { | i } [ | o ]
discard first cont = bind first (\_ -> cont unit)

-- | Single-field specialization of `resolve` — the `edit`-position combinator
-- | for this direction. Where `property` **refocuses** (background fixed, focus
-- | transformed), this **re-backgrounds**: the **focus** `f` at `l` is held
-- | fixed and threaded across the boundary as **input field ↔ output case**,
-- | while the wrapped profunctor transforms the **background** `b → b'`
-- | (turning the input **shot** `s` into the output shot `s'`). The `Done`
-- | branch emits some case of `b'`; the `Loop`/short-circuit branch lets the
-- | focus escape directly as output case `l`.
resolveProperty
  :: forall @l p f lf b s b' s'
   . Resolving p
  => IsSymbol l
  => Cons l f b s
  => Cons l f b' s'
  => Cons l f () lf
  => Union b' lf s'
  => p { | b } [ | b' ]
  -> p { | s } [ | s' ]
resolveProperty g =
  dimap
    -- no `Lacks`: `unsafeDelete` realizes the layout `Cons l f b s` pins —
    -- under a shadowed duplicate label the outer entry wins, the same
    -- first-label convention `inj`/`on` follow.
    (\s -> Tuple (unsafeDelete (reflectSymbol (Proxy @l)) s) (get (Proxy @l) s))
    (either expand (inj (Proxy @l)))
    (resolve g)

-- | The single-field **focus** for this direction — the `× → +` analogue of
-- | `property` (row-typed `first`), built on `resolve` exactly as `property` is
-- | built on `first`. The **focus** `f` at `l` of the input **shot** `s` is fed
-- | to the wrapped `p f f'`; the **background** `{ | b }` cannot stay a record
-- | inside the `Variant` output, so — as in `shutterWrap` — it is wrapped as a
-- | single output case `w`: `Done` emits case `l :: f'`, the `Loop`/escape
-- | branch emits case `w` carrying the untouched background. The single-field
-- | form of `shutterWrap`; the transpose of `resolveProperty`, which runs the
-- | wrapped profunctor on the *background* and lets the focus escape.
propertyToCase
  :: forall @l @w p f f' b s lx wx s'
   . Resolving p
  => IsSymbol l
  => IsSymbol w
  => Cons l f b s
  => Cons l f' lx s'
  => Cons w { | b } wx s'
  => p f f'
  -> p { | s } [ | s' ]
propertyToCase g =
  dimap
    -- no `Lacks`: `unsafeDelete` realizes the layout `Cons l f b s` pins —
    -- under a shadowed duplicate label the outer entry wins, the same
    -- first-label convention `inj`/`on` follow.
    (\s -> Tuple (get (Proxy @l) s) (unsafeDelete (reflectSymbol (Proxy @l)) s))
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
-- | Adopt a **canonically-labeled** event component (`[ clicked :: a ]` out,
-- | the citizenship-carrying interface) as business case `l`: renames the
-- | case, input untouched — `rmap`-only, the `asField` twin at `× → +`.
asCase :: forall @l p i a s. IsSymbol l => Profunctor p => Cons l a () s => p i [ clicked :: a ] -> p i [ | s ]
asCase = rmap (on (Proxy @"clicked") (inj (Proxy @l)) case_)

recordToCase
  :: forall @l p r b s f
   . IsSymbol l
  => Cons l f b s
  => Profunctor p
  => p { | r } f
  -> p { | r } [ | s ]
recordToCase = rmap (inj (Proxy @l))

-- | Introduce a widget's **bare** output as case `l`, projected by the
-- | payload projection — `recordToCase` freed from the record-input
-- | constraint, at the **closed singleton row** (the `field`/`echoCase`
-- | lesson: pinned empty background, so it infers with no annotations).
-- | The payload projection is the mechanism's own argument (import-tower
-- | rule L16: projections ride mechanisms, applications never map raw
-- | channels): `listOf {} entries item # toCase @"picked" _.key`;
-- | `identity` says verbatim, the `forValue` of case introduction. The
-- | output-side dual of `onCase` and the general sibling of `asCase`
-- | (which renames the canonical `clicked` case).
toCase :: forall @l p i a b s. IsSymbol l => Cons l b () s => Profunctor p => (a -> b) -> p i a -> p i [ | s ]
toCase f = rmap (\a -> inj (Proxy @l) (f a))

-- | `recordToCase` over the echo wire, at the **closed singleton row** —
-- | the `field` lesson applied to `× → +`: the pinned empty background
-- | (`Cons l { | r } () s`) is what lets this infer with no annotations as
-- | a merge operand, where `recordToCase @l identity`'s open output row is
-- | ambiguous under the merges' `Nub`. Echoes every record fed as output
-- | case `l` — the pass-through operand of an event merge.
echoCase
  :: forall @l p r s
   . IsSymbol l
  => Cons l { | r } () s
  => Profunctor p
  => Category p
  => p { | r } [ | s ]
echoCase = rmap (inj (Proxy @l)) identity

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

-- | Row existential `Shutter` focusing a whole **sub-Record** — the row-valued
-- | **focus** `f` — of the input **shot** `s`; the residual is the **background**
-- | `{ | b }` (`ExclusiveRows f b s`, the same split `focusRecord` uses).
-- | Crossing `× → +`, the background can't stay a record in the `Variant`
-- | output, so it is **wrapped as a single output case `w`** — a variant
-- | carrying the record. The output extension is itself shot-shaped:
-- | `Cons w { | b } b' s'` — the wrapped background is the focus of a second
-- | shot at `w`, against the inner output `b'`. The inner
-- | `p { | f } [ | b' ]` runs on the focus: `Done` expands its result into
-- | `s'`, `Loop` injects the retained background-record into case `w`. The
-- | mixed-direction analogue of `focusRecord` — same sub-record focus, but the
-- | background is *wrapped* to cross into the variant output rather than
-- | carried same-kind. The `× → +` row combinator over the bare strength
-- | `Resolving`, just as `focusRecord` is the row combinator over `Strong`.
-- |
-- | ```purescript
-- | -- focus (item, qty); wrap the background { note } into output case `draft`
-- | checkout :: Shutter
-- |   { item :: String, qty :: Int, note :: String }              -- s   input shot
-- |   [ priced :: Int, draft :: { note :: String } ]              -- s'  output shot
-- |   { item :: String, qty :: Int }                              -- f   sub-Record focus
-- |   [ priced :: Int ]                                            -- b'  inner output
-- | checkout = shutterWrap @"draft"
-- | ```
shutterWrap
  :: forall @w p f b s b' s' mix
   . Resolving p
  => IsSymbol w
  => ExclusiveRows f b s
  => Cons w { | b } b' s'
  => Union b' mix s'
  => p { | f } [ | b' ]
  -> p { | s } [ | s' ]
shutterWrap g =
  shutterE
    (\s -> Tuple (unsafeCoerce s) (unsafeCoerce s))
    (either expand (inj (Proxy @w)))
    g