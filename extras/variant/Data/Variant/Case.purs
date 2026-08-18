-- | The **value-level label read** the ecosystem's `Data.Variant` does not
-- | export: the case label of a variant value, verbatim. It is not a coined
-- | concept — the value, the machinery (`unvariant`) and the reflected symbol
-- | are all the ecosystem's, only this composition is missing — so it lives
-- | under `Data.Variant.*`, extending that family rather than shadowing it,
-- | in its own `extras/variant` source root. Liftable into
-- | `purescript-variant` unchanged.
-- |
-- | Law (label-is-copy):
-- |
-- |   caseText (inj @l a) = reflectSymbol (Proxy @l)
-- |
-- | Under the label-indexed vocabulary a case label *is* the copy it draws
-- | (a selector's `choice @l` states its copy once, at its case), so a
-- | business `match` whose branches merely restate their labels — verbatim
-- | or re-cased — is this function in disguise: write the label as the exact
-- | copy the line needs and read it back with `caseText`.
module Data.Variant.Case
  ( caseText
  )
  where

import Data.Symbol (reflectSymbol)
import Data.Variant (Unvariant(..), Variant, unvariant)

-- | The case label of a variant value, verbatim — `Unvariant`'s eliminator
-- | applied to symbol reflection alone, the payload dropped.
caseText :: forall r. Variant r -> String
caseText v = case unvariant v of Unvariant f -> f \l _ -> reflectSymbol l
