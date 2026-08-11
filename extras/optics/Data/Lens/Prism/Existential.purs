-- | The **existential encoding** of the ecosystem's `Prism`, the one
-- | constructor `Data.Lens.Prism` does not export. It is not a coined optic —
-- | `Prism` and its `Choice` are both the ecosystem's — so it lives under
-- | `Data.Lens.Prism.*` rather than beside the six coined optics, and it
-- | extends that family rather than shadowing `Data.Lens.Prism` itself.
-- | Used by `Data.Profunctor.Row.VariantToVariant.focusCase`.
-- |
-- | The purest **complement of the ecosystem's own** in the tree — both the
-- | optic and its `Choice` are `profunctor-lenses`', and only this one
-- | constructor is missing — hence the `Data.Lens.*` name and the separate
-- | `extras/optics` source root.
module Data.Lens.Prism.Existential
  ( prismE
  )
  where

import Data.Either (Either)
import Data.Lens (Prism)
import Data.Profunctor (dimap)
import Data.Profunctor.Choice (left)

-- | Construct a `Prism` straight from its **existential encoding**
-- | `∃c. (s → a + c) × (b + c → t)`: pick the residual `c`, then supply `decon`
-- | (match `s` as the focus `a` or the complement `c`) and `recon` (rebuild `t`
-- | from the built `b` or that same complement `c`). The quantified `c` is the
-- | eliminator of that existential; `left` (`Choice`) is the carrier. The standard
-- | `Data.Lens.prism` is this at the co-Yoneda witness `c := t`.
prismE :: forall s t a b c. (s -> Either a c) -> (Either b c -> t) -> Prism s t a b
prismE decon recon g = dimap decon recon (left g)
