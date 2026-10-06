-- | **Self-reference as carrier structure** — `Seeding`'s sibling: `Seeding`
-- | says *a beginning exists* (one registration moment), `Looping` says
-- | *feedback exists* (an emission can re-enter its own input). The two
-- | things a timeless carrier lacks — hence, like `Seeding`, deliberately
-- | **no `(->)` instance**: feeding a function its own output is `fix`, a
-- | fixpoint computation, not a wire.
-- |
-- | The method is the `×`-diagonal **self-trace** at record rows: feed a
-- | UI component its own emissions. A *class* because no ecosystem class
-- | reaches it: on knowledge-gated carriers `Costrong`'s `unfirst` cannot
-- | self-feed (no `c` before the first emission, no emission before the
-- | first input — the gate deadlocks), so the self-feeding special case is
-- | carrier structure, not a derivation. Row-shaped in the method itself:
-- | the looped value is an entity (a model row). It is the one knot: a
-- | loop through the four shapes is cut at its record junction, and an
-- | event re-enters through its fold. The variant side needs none —
-- | `Cochoice`'s `unleft` holds raw on `PUI` (the carrier is traced over
-- | `+`, pointed-traced over `×`), and a variant knot (`cycled`, 2026-10-05
-- | to 2026-10-06) was retired as a second way of writing the same loop
-- | whose model no line of the view could declare.
-- |
-- | Laws — the trace axioms restricted to the diagonal (`identity` on
-- | `Category` carriers), stated up to the observational equivalence of
-- | doc/observational-semantics.md (boundary channels; inner feeds compared
-- | up to consecutive duplication — the quotient feed-idempotence licenses):
-- |
-- | ```
-- | looped identity         = identity                        (yanking)
-- | looped (dimap f f⁻¹ g)  = dimap f f⁻¹ (looped g)          (conjugation — dinaturality at an iso f)
-- | looped (looped g)       ≈ looped g                        (idempotence: the guard; nesting only duplicates feeds)
-- | ```
-- |
-- | Conjugation needs the inverse pair: `dimap f f` in both positions is
-- | false for every non-involutive iso — the loop path would re-feed
-- | `f (f y)` where the re-fed value must be `g`'s own emission `y`
-- | verbatim (counterexample in test/Main.purs).
-- |
-- | The equational triple is necessary, not sufficient: `looped = identity`
-- | satisfies all three. What the class *means* is a fourth, behavioral law
-- | (temporal, like `Seeding`'s point law):
-- |
-- |   * **re-entry** — each emission of the wrapped UI component is fed
-- |     back to it exactly once, before propagating; the echoes that
-- |     re-feed provokes are swallowed.
-- |
-- | **Where rows end and the carrier begins**, stated so the primitive is
-- | not larger than it must be. Two of `looped`'s three facts are row
-- | consequences: the re-entry guard is the Repetition law of a record
-- | input made operational — the re-fed row is the row just emitted, so
-- | the echoes the re-feed provokes are repetitions, and swallowing them is
-- | the quotient `≈` already takes; and the knot does not deadlock where
-- | positional `unfirst` does because a record **input is shared** — the
-- | loop-back and the outside are two feeders of one inclusive input row,
-- | and shared inputs are ungated (`SharedRecordInputs`), where `unfirst`'s
-- | `Tuple` input waits for both halves. What is not a row consequence is
-- | the cycle itself: an emission becoming a feed. That is the class. The
-- | knot hides nothing and takes no seed of its own: it exposes the whole
-- | row and is primed by its first feed (`bracketed` loops the editor state
-- | the fed variant supplies; `with seed` feeds an app its model) — a
-- | looped state is a model field, never a hidden channel (the field-level
-- | `feedback @l` was retired 2026-10-05).
-- |
-- | What the carrier-agnostic layer builds on it: `with seed (looped w)` (the
-- | app shape) and `bracketed` (the variant-editor bracket),
-- | both in `Data.Profunctor.Row.RecordToRecord`.
module Data.Profunctor.Looping
  ( class Looping
  , looping
  , looped
  )
  where

import Data.Profunctor (class Profunctor)

class Profunctor p <= Looping p where
  looping :: forall r. p { | r } { | r } -> p { | r } { | r }

-- | The knot at a declared row: `looped @( count :: Int ) w`, the
-- | class method with the loop's row as its visible argument (a class
-- | member's first visible argument would be the carrier, so the method is
-- | `looping` and this is its face).
looped :: forall @r p. Looping p => p { | r } { | r } -> p { | r } { | r }
looped = looping
