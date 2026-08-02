# Record ↔ variant symmetry

A survey of the API's record-side/variant-side correspondence: what pairs
exactly, which asymmetries are *forced* by the record/variant ontology, and
which were accidental (and what was done about them). The transposition is
`× ↔ +`: records are all-at-once entities, variants are one-at-a-time
events; swapping the modes swaps `RecordToRecord ↔ VariantToVariant` and
`RecordToVariant ↔ VariantToRecord`.

## The correspondence map

| role | `×` side | `+` side |
|---|---|---|
| merge + unit | `recordToRecord` / `pempty` | `variantToVariant` / `pempty` |
| mixed merges | `recordToVariant` | `variantToRecord` |
| strength | `Strong` (ecosystem) | `Choice` (ecosystem) |
| mixed strengths | `Resolving` / `resolve` | `Retaining` / `retain` |
| co-strengths | `Costrong`, `Coresolving` | `Cochoice`, `Coretaining` |
| reversed optic | `Colens` | `Coprism` |
| mixed optics | `Shutter` / `Coshutter` | `Reel` / `Coreel` (each the other's reversal) |
| trace row forms | `feedback`, `folding @w` | `iterate`, `unfolding @w` |
| single-label lens/prism | `property` | `case_` |
| label thread (mixed) | `resolveProperty` | `retainCase` |
| single-label focus (mixed) | `propertyToCase` | `caseToProperty` |
| introduce | `recordToCase @l` | `caseToRecord @l` |
| sub-structure wrap | `focusRecord` (parcel), `shutterWrap` | `focusVariant` (cashbox), `reelWrap` |
| rename adopter | `asField @l` | `asCase @l` (out) / `forCase @l f` (in) |
| whole-value adopter | `projected f` | `toCases f` (out) / `forCases f` (in) |
| bare structural feed | `atField @l` | `onCase @l` (dispatch) / `atCase @l f` (gated pane) |
| bare output introduce | — (`completed` covers it) | `toCase @l f` |
| conditional pane | `provided f` (`Maybe`) | `atCase @l f` (`Maybe` is the 2-case variant; `atCase = provided ∘ prj`) |
| pass-through display stage | `tapped` (echo-driven), `displayed` (unconditional) | `observed` (unconditional, feed-time forward) |
| subsumption vehicle | `Union` widening (`widenRecordInput`) | `Contractable` narrowing (`observed`), `Union` expansion on outputs |
| collections | `foreach @l`, `edited @l`, `acted @l` | `dispatched`, `accumulated` |

## Principled asymmetries (forced, kept)

1. **Sharing polarity** — the root of everything below, encoded in
   `Data.Profunctor.Row`: `×` inputs are *shared* (inclusive, label-blind
   broadcast), `+` inputs are *owned* (exactly one handler per case);
   `×` outputs owned, `+` outputs shared.
2. **Units** — a lawful record-output unit must *announce* its
   informationless `{}` (a class member; parametric silence cannot), a
   variant-output unit is forced silent (uninhabited ends,
   `pempty = silence`).
3. **Seeds** — every `×`-flavored trace form takes an initial state
   (`feedback`/`folding`/`unfolding`, `with`/`mvu`), `iterate` takes none:
   entities pre-exist, events occur
   (doc/pointedness-entities-vs-events.md).
4. **Field vs case existence** — `atField` feeds unconditionally (a field
   always exists), `atCase` attaches/detaches (a case exists
   one-at-a-time). Same structural role, opposite temporal behavior — the
   duality itself, not a naming accident.
5. **Collection extent** — `acted`'s gather gate ("withhold until every
   element spoke") has no `+` dual: keyed *inputs* have open extent, so
   "every key spoke" is undefined. The container square honestly has five
   members, not six (doc/collections-profunctor-algebra.md).
6. **Payload boundary** — `toCase @l` takes a payload projection (it
   dissolves output-side lambdas at collection sites), `onCase @l` takes
   none: a case payload is pinned by its consumer as often as by its
   emitter, so the input side stays exact
   (doc/experiment-ad-hoc-rows.md).

## Canonical rows (the split is honest)

The record side speaks one canonical editor/display row, the variant side
two:

| citizen sort | canonical interface | adopters |
|---|---|---|
| editors / displays (`× → ×`) | `{ value :: a }` | `asField @l`, `forField @l f`, `forProperty @l f`, `projected f` |
| event emitters (`× → +`) | `[ clicked :: a ]` | `asCase @l`, `toCases f` |
| statuses (`+ → ×`) | `[ event :: a ]` | `forCase @l f`, `forCases f` |
| actions (busy displays) | `{ busy :: Boolean }` | (the `action` bracket) |

The split follows the citizens, not a naming gap: an emitter and a status
are different sorts (one replays what it was shown, one renders what
happened), and the record side quietly splits the same way (`{ value }`
vs `action`'s `{ busy }`). Unifying the case labels to `value` would erase
real information — `updated (match { clicked: … })` at a bare button says
what happened; `match { value: … }` would not.

## The accidental gaps, and what closed them

* **`forCases`** (in `VariantToRecord`, re-exported from `PUI`) — `toCases`'
  input dual. Before it, "one status instance serving several mutually
  exclusive outcomes" was inexpressible under import-tower L16 (the
  required `lcmap (match … >>> inj event)` is app-banned); flight-booker
  carried two sibling snackbars of which only one could ever show.
  flight-booker now reads `snackbar # forCases (match { booked: …,
  rejected: … })`, and its business emissions carry bare payloads (the
  itinerary, the problem) with the copy in the toast widget, per A8.
  `toCases` moved beside `toCase`/`asCase` in `RecordToVariant` (generic
  over `Profunctor`) so the plural adopters live in their direction
  modules, mirrored.
* **`observed`** (in `PUI`) — the variant answer to `tapped`/`displayed`:
  a status made an event pass-through stage. Events forward exactly once,
  at feed time, and the status's own emissions are dropped (entities are
  idempotent, so a display echo may re-forward them; events are one-shot,
  so an echo re-emission would duplicate). Subsumption runs the variant
  way — `Contractable` narrowing: the status consumes the cases it knows,
  background cases pass untouched. payment's retry loop is the showcase:
  `snackbar # forCase @"charge" retryLine # observed` inside the
  `iterate` chain narrates each declined attempt without interrupting the
  loop.
* **`focusVariant`** (in `VariantToVariant`, re-exported from `PUI`) —
  `focusRecord`'s transpose, completing the wrap family's `+ → +` corner:
  the wrapped profunctor handles the focus cases, background cases pass
  untouched. It first sat in the survey as failing reachability; cashbox
  reached for it — the money events (refund, payout) detour through
  confirmation dialogs while the audit event flows straight to the fold —
  and the demand admitted it. Each dialog inside cashbox's focus is the
  closed-singleton wrap in merge position (below).
* **Deliberately unnamed** (recorded in `VariantToVariant`'s header):
  `field`'s `+ → +` transpose is `onCase @l >>> toCase @l' f` (fails
  subsumption) — demo-covered by weather's about-dialog and cashbox's two
  confirmation dialogs, which are exactly that composition.

## Demo coverage of the twins

Every *vocabulary-level* pair in the map is now exercised by a demo on
both sides. The sub-structure focus pair gets a focused demo each, the
way the trace quartet did: **parcel** (`focusRecord` — a flat
`{ recipient, street, city }` model, the reusable address sub-form a
citizen over its own closed `{ street, city }` row, the background field
threaded), and **cashbox** (`focusVariant` — selective interception as
UX: confirmation dialogs for the money sub-family, instant passage for
the rest). The mixed-direction wraps (`shutterWrap`, `reelWrap`) and the
single-label threads (`property`/`case_`, `resolveProperty`/`retainCase`,
`propertyToCase`/`caseToProperty`) are algebra beneath the row forms and
optics — their coverage lives in `spago test` and the business-optics
test modules, which is where the survey leaves them.
