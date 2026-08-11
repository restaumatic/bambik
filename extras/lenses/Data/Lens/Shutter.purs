-- | The **`Shutter`** — the optic `Data.Profunctor.Resolving`'s `resolve`
-- | generates by Pastro–Street, for the coined `× → +` shape. Both the class
-- | and the optic are this library's. Its co-optic is `Data.Lens.Coshutter`,
-- | and nothing here mentions a row: the row form is
-- | `Data.Profunctor.Row.RecordToVariant.subResolving`.
-- |
-- | It claims a `Data.Lens.*` name and lives under the separate
-- | `extras/lenses` source root because it belongs to that family by
-- | construction — an optic over a strength, mentioning no `PUI`, no row and
-- | no carrier. Unlike `Colens`/`Coprism` it could not be upstreamed alone:
-- | its class is coined too, so the pair travels together.
module Data.Lens.Shutter
  ( Shutter
  , shutter
  , shutterE
  )
  where

import Data.Either (Either, either)
import Data.Profunctor (dimap)
import Data.Profunctor.Resolving (class Resolving, resolve)
import Data.Tuple (Tuple(..))

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
