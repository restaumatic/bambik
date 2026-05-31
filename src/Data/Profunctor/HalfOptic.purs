-- | # Half-optics
-- |
-- | Single-field/single-case optics, organized as the **2×2×pin** that the design note
-- | `doc/row-to-row-vs-half-lens.md` describes. Each half-optic is the `Strong`/`Choice`
-- | fanout with one slot pinned to a monoidal unit; row = product vs sum, column =
-- | introduce vs eliminate, plus the in-place edit/focus diagonal.
-- |
-- | ```
-- |                introduce (grow output)      eliminate (consume input)     edit / focus
-- |  product   introduceProperty  (Strong)   eliminateProperty  (Strong)   edit       (Strong lens, first/second)
-- |  (Record)  ── transpose ───────────────────────────────────┘
-- |  sum       introduceCase   (IntroVarP)   eliminateCase    (Choice)     focusCase  (Choice prism, left/right)
-- |  (Variant) ── transpose ───────────────────────────────────┘
-- | ```
-- |
-- | **Product row = `Strong`** (`first`/`second`). Under the `p s r`/`p w s` shape the pin
-- | is the copy (`Δ`), which is full `Strong`; so `introduceProperty`/`eliminateProperty`
-- | carry only `Strong`, and `edit` is the standard field lens
-- | (`Data.Lens.Extra.Commons.property`). Because `UI` is `Strong`, these work on `UI`.
-- |
-- | **Sum row = `Choice`** (`left`/`right`) — *almost*. `eliminateCase` folds onto `Choice`
-- | via `left` (`Choice ⇒ ExceptP`, mirroring product eliminate via `first`), and `focusCase`
-- | is the `Choice` prism (`right`, via `Commons.variant`). The sole exception is
-- | `introduceCase`: its source `p Void case` has no input for `Choice` to dispatch on (the
-- | new case fires spontaneously), so `Choice ⇏ IntroVarP` and it keeps a dedicated class.
-- |
-- | **The boundary adaptor quartet** (`Unit`↔`Void`) is gone from the API:
-- |   * `FormP` (`p Void r -> p Unit r`, genuine) — dissolved by the `p s r` shape.
-- |   * `XP` (`p Unit a -> p Void a`, free `lcmap absurd`) — inlined in `introduceCase`.
-- |   * `YP` (`p a Void -> p a Unit`, free `rmap absurd`) — gone with the product `Strong` fold.
-- |   * `ZP` (`p a Unit -> p a Void`, genuine) — sidestepped: `eliminateCase` takes a `Void` handler.
module Data.Profunctor.HalfOptic
  ( module Property
  , module Case
  , module IntroVarP
  ) where

import Data.Profunctor.HalfOptic.Property (edit, eliminateProperty, introduceProperty) as Property
import Data.Profunctor.HalfOptic.Case (eliminateCase, focusCase, introduceCase) as Case
import Data.Profunctor.HalfOptic.IntroVarP (class IntroVarP, liftIntroVar) as IntroVarP
