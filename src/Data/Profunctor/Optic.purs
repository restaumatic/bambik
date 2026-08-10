-- | The **optics** the four strengths and their co-strengths generate by
-- | Pastro–Street, one pair per row-profunctor shape:
-- |
-- | ```
-- | shape        strength     strength optic  co-strength    co-strength optic
-- | -----------  -----------  --------------  -------------  -----------------
-- | p {|a} {|b}  Strong       Lens            Costrong       Colens *
-- | p [|a] [|b]  Choice       Prism           Cochoice       Coprism *
-- | p {|a} [|b]  Resolving *  Shutter *       Coresolving *  Coshutter *
-- | p [|a] {|b}  Retaining *  Reel *          Coretaining *  Coreel *
-- | ```
-- |
-- | `Lens`/`Prism` come from `profunctor-lenses`; the six marked `*` do not
-- | exist in the ecosystem — `Costrong`/`Cochoice` are ecosystem classes
-- | whose optics were never built, and `Resolving`/`Retaining` are this
-- | library's coinage. Every optic here is `Optic p s t a b = p a b -> p s t`
-- | at **arbitrary** `s t a b`: nothing in this module mentions a row, which
-- | is exactly why it is not in `Data.Profunctor.Row.*`. The row layer's
-- | combinators are these optics at row granularity — `feedback` is a
-- | `Colens`, `iterate` a `Coprism`, `folding` a `Coshutter`, `unfolding` a
-- | `Coreel`, and `subResolving`/`subRetaining` are a `Shutter`/`Reel`.
-- |
-- | Each family comes as the collapsed form (co-Yoneda applied at a chosen
-- | witness) plus the `*E` **existential encoding**, where the residual `c`
-- | is yours to name — see `test/BusinessOptics.purs` for `c` as a business
-- | type.
module Data.Profunctor.Optic
  ( Colens
  , colens
  , colensE
  , prismE
  , Coprism
  , coprism
  , coprismE
  , Shutter
  , shutter
  , shutterE
  , Coshutter
  , coshutter
  , coshutterE
  , Reel
  , reel
  , reelE
  , Coreel
  , coreel
  , coreelE
  )
  where

import Control.Category (identity)
import Data.Either (Either, either)
import Data.Lens (Prism)
import Data.Profunctor (dimap)
import Data.Profunctor.Choice (left)
import Data.Profunctor.Cochoice (class Cochoice, unleft)
import Data.Profunctor.Costrong (class Costrong, unfirst)
import Data.Profunctor.Resolving (class Coresolving, class Resolving, coresolve, resolve)
import Data.Profunctor.Retaining (class Coretaining, class Retaining, coretain, retain)
import Data.Tuple (Tuple(..))

-- | The optic `unfirst` induces: the **Colens** — the lens run backwards
-- | (`Colens s t a b ≅ Lens b a t s`). Eliminating the residual `c`
-- | (instantiated to `b`) by co-Yoneda collapses `∃c. (s × c → a) × (b → t × c)`
-- | to `(join : s → b → a) × (out : b → t)`: each input is read **against the
-- | UI component's own last output** — the residual a lens would carry visibly in
-- | the type is hidden, threaded through state instead. The collapsed form
-- | shows why the `PUI` carrier gates it (there is no last output before the
-- | first emission). `feedback` is this optic at row granularity.
type Colens s t a b = forall p. Costrong p => p a b -> p s t

colens :: forall s t a b. (s -> b -> a) -> (b -> t) -> Colens s t a b
colens join out = colensE (\(Tuple s b) -> join s b) (\b -> Tuple (out b) b)

-- | Construct a `Colens` straight from its **existential encoding**
-- | `∃c. (s × c → a) × (b → t × c)`: pick the looped channel `c`, then supply
-- | `decon` (read the input joined with the channel) and `recon` (split each
-- | emission into the output and the channel's next value). `colens` is this
-- | at the co-Yoneda witness `c := b`.
colensE :: forall s t a b c. (Tuple s c -> a) -> (b -> Tuple t c) -> Colens s t a b
colensE decon recon g = unfirst (dimap decon recon g)

-- | Construct a `Prism` straight from its **existential encoding**
-- | `∃c. (s → a + c) × (b + c → t)`: pick the residual `c`, then supply `decon`
-- | (match `s` as the focus `a` or the complement `c`) and `recon` (rebuild `t`
-- | from the built `b` or that same complement `c`). The quantified `c` is the
-- | eliminator of that existential; `left` (`Choice`) is the carrier. The standard
-- | `Data.Lens.prism` is this at the co-Yoneda witness `c := t`.
prismE :: forall s t a b c. (s -> Either a c) -> (Either b c -> t) -> Prism s t a b
prismE decon recon g = dimap decon recon (left g)

-- | The optic `unleft` induces: the **Coprism** — the prism run backwards
-- | (`Coprism s t a b ≅ Prism b a t s`). Eliminating the residual `c`
-- | (instantiated to `a`) by co-Yoneda collapses `∃c. (s + c → a) × (b → t + c)`
-- | to `(embed : s → a) × (step : b → t + a)`: every input becomes a focus,
-- | and every focus result either exits with `t` or **re-enters as the next
-- | focus input** — `tailRec` at the optic level. Where a prism's residual
-- | passes by visibly in the type, a coprism's circulates hidden as control
-- | flow. `iterate` is this optic at row granularity.
type Coprism s t a b = forall p. Cochoice p => p a b -> p s t

coprism :: forall s t a b. (s -> a) -> (b -> Either t a) -> Coprism s t a b
coprism embed step = coprismE (either embed identity) step

-- | Construct a `Coprism` straight from its **existential encoding**
-- | `∃c. (s + c → a) × (b → t + c)`: pick the looped channel `c`, then supply
-- | `decon` (read a fresh input or a looped value) and `recon` (exit or loop
-- | each emission). `coprism` is this at the co-Yoneda witness `c := a`.
coprismE :: forall s t a b c. (Either s c -> a) -> (b -> Either t c) -> Coprism s t a b
coprismE decon recon g = unleft (dimap decon recon g)

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

-- | The optic `retain` induces: the **Reel**. Eliminating the residual `c`
-- | (instantiated to `b → t`) by co-Yoneda collapses `∃c. (s → a + c) × (b × c → t)`
-- | to `s → Either a (b → t)` — a per-input dispatch that either surfaces a focus
-- | `a`, or supplies a *finisher* `b → t` drawn from retained state. Like a film
-- | reel: a wound transport that holds its position and never finishes.
type Reel s t a b = forall p. Retaining p => p a b -> p s t

reel :: forall s t a b. (s -> Either a (b -> t)) -> Reel s t a b
reel dispatch g = reelE dispatch (\(Tuple b f) -> f b) g

-- | Construct a `Reel` straight from its **existential encoding**
-- | `∃c. (s → a + c) × (b × c → t)`: pick the residual `c`, then supply `decon`
-- | (match `s` as a fresh focus `a` or a resumed state `c`) and `recon` (combine
-- | the focus result `b` with the carried state `c` into `t`). The quantified `c`
-- | is exactly the eliminator of that existential; `retain` is the carrier. `reel`
-- | is this at the co-Yoneda witness `c := b → t` (`recon = \(Tuple b f) -> f b`,
-- | i.e. evaluation).
reelE :: forall s t a b c. (s -> Either a c) -> (Tuple b c -> t) -> Reel s t a b
reelE decon recon g = dimap decon recon (retain g)

-- | The optic `coretain` induces: the **Coreel** — the `Shutter` run
-- | backwards (`Coreel s t a b ≅ Shutter b a t s`). Eliminating the residual
-- | `c` (instantiated to `b`) by co-Yoneda collapses
-- | `∃c. (s + c → a) × (b → t × c)` to
-- | `(embed : s → a) × (out : b → t) × (resume : b → a)`: every emission
-- | both leaves as `t` and **re-enters as the next focus input** — a
-- | generator, producing on every step. `unfolding @w` is this optic at row
-- | granularity.
type Coreel s t a b = forall p. Coretaining p => p a b -> p s t

coreel :: forall s t a b. (s -> a) -> (b -> t) -> (b -> a) -> Coreel s t a b
coreel embed out resume = coreelE (either embed resume) (\b -> Tuple (out b) b)

-- | Construct a `Coreel` straight from its **existential encoding**
-- | `∃c. (s + c → a) × (b → t × c)`: pick the resume channel `c`, then supply
-- | `decon` (read a fresh input or a resumed value) and `recon` (split each
-- | emission into the output and the channel's next value). `coreel` is this
-- | at the co-Yoneda witness `c := b`.
coreelE :: forall s t a b c. (Either s c -> a) -> (b -> Tuple t c) -> Coreel s t a b
coreelE decon recon g = coretain (dimap decon recon g)
