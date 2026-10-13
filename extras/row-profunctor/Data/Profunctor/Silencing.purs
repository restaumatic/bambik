-- | **Silence as carrier structure** — `Seeding`'s and `Looping`'s
-- | sibling: `Seeding` says *a beginning exists* (one registration moment),
-- | `Looping` that *feedback exists* (an emission can re-enter its input),
-- | `Silencing` that *nothing exists* — a component that is fed anything
-- | and emits no case, ever.
-- |
-- | The method is row-shaped on purpose: `silence :: p { | i } [ | o }`,
-- | a record in and a variant out, at **any** rows. That shape is the
-- | honesty boundary. `{}` is terminal and `Variant ()` initial, so the one
-- | wire-built candidate — `identity` placed between them — does not
-- | exist: no function turns a record into a case, and parametricity
-- | extends "emits no case" from the empty variant to every variant row.
-- | So where the other three shapes borrow their merge unit from
-- | `Category` (`identity`, the `{}` wire, `lcmap case_ identity`), the
-- | `×→+` merge borrows its unit from here — and like `Seeding` and
-- | `Looping` this is a class of its own rather than a merge method, so
-- | that **no merge class carries a unit** and every shape's unit law is
-- | conditional on the class that supplies it
-- | (`Data.Profunctor.Row.RecordToVariant`, "Laws at `×→+`").
-- |
-- | Laws: `silence` is the unit of the `×→+` merge on the nose,
-- | `recordToVariant silence g = g = recordToVariant g silence`, and it
-- | is absorbed by any stage after it, `silence >>> g ≈ silence` for
-- | `g` fed only by it. Observationally it is what a detached emitter
-- | pane *is* (`PUI.Web.provided`, doc/observational-semantics.md), the
-- | reference element of the `×→+` semantics.
-- |
-- | Instances: `PUI m` (a component with empty `toUser`/`fromUser`),
-- | `Cont r` under `Monoid r` (`mempty`). No `(->)` instance — a function
-- | into a variant must return a case — which is also why the `×→+`
-- | merge has none.
module Data.Profunctor.Silencing
  ( class Silencing
  , silence
  ) where

import Data.Profunctor (class Profunctor)

class Profunctor p <= Silencing p where
  silence :: forall i o. p { | i } [ | o ]
