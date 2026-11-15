-- | `Record → Variant` (× → +) row profunctors, organized (uniformly across
-- | the four shape modules) as:
-- |
-- |   * **strength** — `Resolving` (`Data.Profunctor.Resolving`; `PUI m`
-- |     instances only, no `(->)`): the unary power, a loop/iteration step.
-- |     Its co-strength `Coresolving` is in `Data.Profunctor.Coresolving`,
-- |     and their optics are `Data.Lens.Shutter` and `Data.Lens.Coshutter`
-- |     — neither the classes nor the optics mention a row, so none of them
-- |     lives here.
-- |   * **shape class** — `RecordToVariant`, the binary **merge**: the
-- |     one genuine per-carrier primitive, with its qualified-do sugar
-- |     (`bind`/`discard`). It carries no unit, like the other merge
-- |     classes; theirs is `Category`'s `identity`, and this one, which no
-- |     wire reaches, is the carrier's silent component, named by no class.
-- |   * **free functions** — over `Strong`: `joined` (a source's cases joined
-- |     with the row its emitter was fed — replay is this at the empty
-- |     payload, `PUI.Web`'s click source dropping the `{}` event). No strength or co-strength row form: a
-- |     stage is typed at one row, so no word narrows it to a sub-record
-- |     with the rest escaping as a case, and a fold's state is a model
-- |     field, never a hidden one (`Data.Profunctor.Row`, *What is
-- |     deliberately not here*).
-- |
-- | A word lives where the sides it constrains do: one constraining both
-- | sides in its shape's module, one constraining a single side on the
-- | floor (`Data.Profunctor.Row`)
-- | — so no shape module imports another, and every module holds only
-- | its merge, its strength and trace forms, and the words that genuinely
-- | span both of its sides.
-- | `joined` takes `toCase` from the floor.
-- |
-- | ## Laws at `×→+`
-- |
-- | The six laws of Data.Profunctor.Row ("The laws") read at this shape:
-- | a component `w :: p { | i } [ | o ]` is an **emitter** fed a row;
-- | the merge is `m = recordToVariant w1 w2`, inputs shared (one row
-- | `i`: both operands fed the merge's whole row), outputs owned (each
-- | operand is closed to the cases it emits and the merged row is
-- | exactly their union, `OwnedVariantOutputs`: an emitter owns the case
-- | its label names, an action its outcomes). It is the **pairing**,
-- | read at occurrences: an emitter's output is the occurrences of its
-- | cases over time, and a record of occurrence sources is one stream
-- | of tagged occurrences, so the product of the operands' outputs is
-- | the variant of their cases. Both operands are fed and both may emit,
-- | and a case has one emitter: a block of emitters lists the events
-- | that can occur, a case written twice is a compile error naming it,
-- | and so is a handler after the block for a case nothing emits.
-- |
-- |   1. **Repetition** — owed: `feed x ; feed x ≈ feed x`. A feed writes
-- |      the replay slot; a second write of the same row changes nothing.
-- |   2. **Answer** — nothing: a feed is answered by no emission. It
-- |      **arms** — every emission has a cause outside the channel (a
-- |      click, an `Aff` settling, quiescence) — which is what lets
-- |      a loop re-feed an emitter without firing it. What an
-- |      emission carries is the emitter's contract, not the shape's:
-- |      a button replays the row last fed (`joined` of a click).
-- |   3. **Symmetry and associativity** — up to `≈`.
-- |   4. **Projection** — `π_k` is the whole row: every feed of `m`
-- |      reaches each operand whole, arming it, and nothing else does.
-- |      `exact` is the identity: a variant carries its one tag, so an
-- |      operand's case enters the merged row as it is, unmarked, and no
-- |      two operands emit the same case.
-- |   5. **Preservation** — `m` obeys 1 and 2: a feed reaches both
-- |      operands whole and writes each one's replay slot, so a repeated
-- |      feed changes neither; neither answers it, so neither does their
-- |      passage.
-- |   6. **Monotonicity** — `w1 ⊑ w1'` implies
-- |      `recordToVariant w1 w2 ⊑ recordToVariant w1' w2`.
-- |
-- | On `PUI`: broadcast in, passage out — each emission exits as it
-- | occurs, nothing retained, no state (`Applicative m` suffices). Law 2
-- | is why this shape has no starvation: a silent emitter is lawful, and
-- | an absent one is the silent component (below). Probes carry law and
-- | shape in test/Main.purs (`answer ×→+` over `joined` of a click probe,
-- | the derivation every vocabulary emitter is, `PUI.replayed`; the
-- | real emitters are clicked on the leaf-law bench); 3–6, with 5's
-- | repetition and arming over `joined` emitters, run over every script
-- | to a bound in test/Exhaustive.purs.
-- |
-- | The carrier's silent component — fed nothing, emitting nothing — is
-- | what an **absent emitter is**. A `×→+` component that
-- | exists in one state and not in another
-- | (`button @"Start" # provided @"halted" phaseOf`) is, in the state
-- | where it is absent, observationally silent: fed nothing, emitting
-- | nothing. That is why the type is variant-output only — silent at
-- | *variant* output is an emitter with nothing to say, while silent at an
-- | inhabited *record* output would be a starved gate, which is why the
-- | record-side pane answers every feed instead — `inCase`, with the fed
-- | row. But absence is **not a copairing**: routing the input away
-- | from `w` leaves `w`'s own emission channel connected, so
-- | `lcmap f' (left w >>> right silent)` still emits when the emitter
-- | fires in the "absent" case (checked on the probe carrier: a Start
-- | button clicked while `timing` still emitted). Only removing
-- | the component silences it, and removal is `Hosting`'s — the DOM's —
-- | not the algebra's. So `provided` stays a carrier primitive, and
-- | the silent component is the *value* of what it removes, not an operand it is
-- | built from. (Deleted and restored on this argument; CHANGELOG, 2026-09-11.)
module Data.Profunctor.Row.RecordToVariant
  ( class RecordToVariant
  , recordToVariant
  , joined
  , bind
  , discard
  )
  where

import Control.Category (identity)
import Data.Lens.Lens (lens)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Row (class JoinedCases, class OwnedVariantOutputs)
import Data.Profunctor.Strong (class Strong)
import Data.Unit (Unit, unit)
import Data.Variant.Internal (VariantRep(..))
import Unsafe.Coerce (unsafeCoerce)

class Profunctor p <= RecordToVariant p where
  recordToVariant
    :: forall i o1 o2 o o1l o2l
     . OwnedVariantOutputs o1 o2 o o1l o2l
    => p { | i } [ | o1 ]
    -> p { | i } [ | o2 ]
    -> p { | i } [ | o ]

bind
  :: forall p i o1 o2 o o1l o2l
   . RecordToVariant p
  => OwnedVariantOutputs o1 o2 o o1l o2l
  => p { | i } [ | o1 ]
  -> (p { | i } [ | o1 ] -> p { | i } [ | o2 ])
  -> p { | i } [ | o ]
bind first cont = recordToVariant first (cont first)

discard
  :: forall p i o1 o2 o o1l o2l
   . RecordToVariant p
  => OwnedVariantOutputs o1 o2 o o1l o2l
  => p { | i } [ | o1 ]
  -> (Unit -> p { | i } [ | o2 ])
  -> p { | i } [ | o ]
discard first cont = bind first (\_ -> cont unit)

-- | **Case `l` joined with the row its emitter was fed** (`×→+`): the
-- | lens whose getter is `identity` — `Strong`'s `first` under the
-- | diagonal, the fed row riding the state channel — and whose setter
-- | distributes that row into the occurrence's case, so it leaves beside
-- | the payload as one record, `{ event,
-- | model }`, under the same case. It is how an event carries the row
-- | of the **element it happened in**: an emitter inside a list element,
-- | joined there, leaves with that element's row — `( listOfAt' @"Line
-- | plated" @"dish" @"lines" … ) # joined @"Line plated"` under `#
-- | gatheredOf @"number" cooking` (diner), the plated dish with its
-- | order. At an application's top level nothing is joined: the model
-- | meets the event at its fold (`PUI`'s `foldInto`, `kept`). The label
-- | **closes the source** to case `l`, and the joined case leaves closed
-- | to it too, so the source stands in an emitter block as the one
-- | emitter of `l` (a label-free
-- | `joined` over every case of a closed source row was the word from
-- | 2026-10-14 to 2026-10-15; until 2026-10-16 the label left the
-- | source's other cases passing, and the action declared its outcome
-- | row on its line; until 2026-11-15 the joined case was widened into
-- | a row the block shared; CHANGELOG). A source of several outcomes names them all, `joined @(l1 + l2)`. A click is the degenerate
-- | case where the occurrence carries nothing of its own: `PUI.Web.clicked`
-- | is this with the `{}` event dropped and the case renamed — a
-- | derivation, so no word of its own (`Data.Profunctor.Row`, *Retention*).
joined
  :: forall @ls p r v v1
   . JoinedCases ls r v v1
  => Strong p
  => p { | r } [ | v ]
  -> p { | r } [ | v1 ]
joined = joinedEvery

joinedEvery :: forall p r v v1. Strong p => p { | r } [ | v ] -> p { | r } [ | v1 ]
joinedEvery = lens identity withModel
  where
  withModel :: { | r } -> [ | v ] -> [ | v1 ]
  withModel r e = case (unsafeCoerce e :: VariantRep _) of
    VariantRep rep -> unsafeCoerce (VariantRep { type: rep.type, value: { event: rep.value, model: r } })
