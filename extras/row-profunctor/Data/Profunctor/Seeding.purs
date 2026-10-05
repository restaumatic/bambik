-- | **Pointedness: the terminal record's one answer.** An entity (a record
-- | channel) has a known initial state; an event (a variant channel) merely
-- | occurs. The **point** `announce a :: p {} a` is how a carrier supplies
-- | that initial state: a map out of the terminal record that emits `a`.
-- |
-- | What the point *is* follows from rows alone. `{}` has exactly one
-- | value, so every feed of a `{}`-input component is a repetition of the
-- | first, and the Repetition law of a record input
-- | (`Data.Profunctor.Row`, "The laws") makes answering it once and
-- | answering it every time the same behaviour:
-- |
-- |   * **answer** — `announce a ≈ lcmap (const a) identity`: the point is
-- |     the wire's answer to the terminal record's one value. (Up to
-- |     stutter, the observational equivalence of doc/observational-
-- |     semantics.md §2; on a timeless carrier, an equality.)
-- |
-- | So every `Category`+`Choice` profunctor has a lawful point — `(->)`'s is
-- | `const`, `Cont`'s answers its continuation — and what a *stateful*
-- | carrier adds is one thing, **when** it answers:
-- |
-- |   * **early** — a carrier with a registration moment gives the answer
-- |     at registration, before any feed arrives, and (by Repetition) never
-- |     again: feeding it `{}` changes nothing.
-- |
-- | That earliness is the operational ⊥ of the trace asymmetry theorem
-- | (doc §5): the seeded `×`-trace forms compose a `seeded` wire into a
-- | positional loop no feed can reach, and it primes the loop only because
-- | the answer comes before the first input. On `(->)` earliness is vacuous
-- | (no time). The knots need no seeded wire at all: `looped` and `cycled`
-- | are primed by their first feed, which `with` supplies.
-- |
-- | The **seeded echo wire** `seeded a :: p a a` — `identity`'s pass-through
-- | plus the point — is *derived*, through `Choice`: the point sits on the
-- | `Left` branch, the wire on the `Right`, and both branches rejoin,
-- |
-- | ```
-- | seeded a = dimap Right (either identity identity) (left (announce a))
-- | ```
-- |
-- | so the pointed wire needs no second primitive (`Choice` is therefore a
-- | superclass, beside `Category` for the wire); on `(->)` it is `identity`,
-- | the seed invisible, as a timeless wire should have it. It is the
-- | initial-state supply a seeded value-level trace builds on (`debounced`
-- | primes its loop branch by composing a `seeded` into the traced chain),
-- | while `with a w = announce a >>> w` is the discharge form (`Data.Profunctor.Row.RecordToRecord`) —
-- | and the mount (`PUI.Web.HTML.body`) feeds the closed app `{}` once, the
-- | terminal record's one value, which a point answered already. The merge
-- | units are *not* pointed: they are `identity` at the unit object, and
-- | the record gates ignore a contribution of zero fields — pointing is
-- | this class's business alone.
module Data.Profunctor.Seeding
  ( isHole
  , class Seeding
  , announce
  , seeded
  ) where

import Control.Category (class Category, identity)
import Data.Eq ((==))
import Unsafe.Coerce (unsafeCoerce)
import Data.Either (Either(..), either)
import Data.Function (const)
import Data.Profunctor (dimap)
import Data.Profunctor.Choice (class Choice, left)

class (Category p, Choice p) <= Seeding p where
  announce :: forall a. a -> p {} a

-- | The timeless point: the answer, with no moment to give it early.
instance Seeding (->) where
  announce = const

seeded :: forall p a. Seeding p => a -> p a a
seeded a = dimap Right (either identity identity) (left (announce a))

-- | Whether a value is a **hole** — logic not written yet (guardrails L18):
-- | `PUI.Web.hole` answers `true` to this one field and throws on every other
-- | touch, and no other value has it. A word that consumes a logic value when
-- | built rather than on data — a seed it announces, a period it schedules —
-- | skips a hole, so a view runs before its logic exists. Pure: a field read.
isHole :: forall a. a -> Boolean
isHole a = (unsafeCoerce a :: { "__bambikHole" :: Boolean })."__bambikHole" == true
