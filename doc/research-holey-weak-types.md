# Holes against the real library: weakening the types (experiment)

*Branch `holey-weak-types`, 2026-09-30. Question: can guardrails L18 — every
view runs with its logic modules stubbed to bare untyped `hole`s — hold
against the **real** library, with no `.Holey` twin modules and no compiler
change, only by changing the library's own types? Answer: yes, 102 of 102,
with footprint checking intact — in the end (round 3), by one row per stage
and row-polymorphic footprints, not by loosening.*

## Result

| | twins (`holey-development`) | weakened library (this branch) |
| --- | --- | --- |
| holey views compiling | 102 / 102 | 96 / 102 |
| holey views mounting clean, reaching no hole under input | 102 / 102 | 96 / 96 |
| real stack (`spago test`, bundles, `npm run smoke`) | green | green (1578 smoke checks; the one failure is the 6 missing holey twins) |
| footprint checking in real builds | unchanged | **gone** for every read function |

The six that do not compile — potluck, reorder, photo-gallery, both twins
each — merge a zero-field operand (a static, chrome) beside a whole-row
editor inside a `×→×` merge whose row only the logic closes. The gate needs
`RowToList` over the owned rows to enrol its participants, and with the seed
a hole the row's tail is open. Closing this needs the merge machinery itself
changed — an evidence-free instance-chain case for a `()` side, and a
pass-through branch in the gate — which is a redesign, not a weakening.

## What had to change, in order of discovery

1. **Shared merge sides.** `SharedRecordInputs`/`SharedVariantOutputs` lost
   their `InclusiveRows` superclass. Freeing record inputs outright broke the
   *real* build: the shared side also carries the context's row down into
   the operands, and without it an operand's owned evidence (a whole-row
   editor inside `folding`, a collection item) saw an open row. Record
   inputs became an **equality** (`TypeEquals`: every operand is fed the
   merge's whole row); variant outputs stay free.
2. **Every read function.** With equal-input merges, an exact-footprint
   business function no longer fits, so subsumption moved from the stages to
   **every leaf that takes a read function**: `text`, `attrWith`, `clWhen`,
   `clicked`'s key, `provided`/`shownWhen`/`inCase` classifiers, the number
   displays of all six vocabularies (`progress`, `progressBar`,
   `linearProgress`, `ratingDisplay`, `imagePane`), `listOf`/`foreach`
   projections, `dynamic`, every status's copy function (`snackbar`, `toast`,
   `messageBar`, `banner`, `output`), `action`'s function, `toCase`'s payload
   function, and the stages' handlers (`applied`, `updated`, `every`,
   `settled`). Each is its old definition (`…Exact`) coerced to a signature
   whose read row is unrelated to its input row.
3. **Statics and `with`** became input-polymorphic, so they sit in an
   equal-input merge (`staticText`, `staticHTML`, `staticString`, `static`,
   `hr`, `divider`, `columnHeader`, `imageListItem`, `each`, `ticks`).
4. **Components keep equality, not freedom.** Freeing an emitter's or a
   content's input (`updated`, `applied`, `clicked`, `replaying`, `shown`,
   `confirmed`) cut propagation the same way the merges did, so those inputs
   are the whole row. The keyed collections are the exception with meaning:
   an item's input is the element row **minus its key** (`acted`, `edited`),
   stated exactly by the `Cons` they already carried.
5. **Row splits** stopped relating focus and background: `subChoice` routes
   by its focus labels alone; `subStrong`, `feedback`, `folding`, `unfolding`
   drop `ExclusiveRows`, and their join became a state-first union instead of
   `exactRow`-trimmed input (equivalent while the two are disjoint by type).
   `shownWhen`/`shownEach` were rebuilt on `shown` — a pass-through beside a
   zero-field pane needs no merge at all, a simplification that would hold
   without this experiment.
6. **`Eq`/`Ord` became structural.** Selectors (every `select`/`radio…`/
   `segmented…`/`dropdown`/`tabBar` in every vocabulary, in all three
   words) and keyed collections compare by a structural key computed in JS
   (`Data.Profunctor.Row.Structural`) — needed by real builds too, since a
   free projection like `_.cells` no longer infers its element type.
7. **Runtime hole-awareness.** `hole` answers `true` to `__bambikHole`, read
   by a pure `isHole` (`Data.Profunctor.Seeding`). `announce`, the
   `folding`/`unfolding` seeds, `ticks`, the debounced text fields, `action`,
   `foreach` and `each` treat a hole as absent. This is needed even for a
   `{}` `body` feeds once (crud's and order-form's load actions; until
   2026-10-06 written as a redundant `with {}`): it is real data, and it
   reaches a load action that is a hole.

## What was lost

- **Footprint safety, everywhere.** A business function reading a field the
  model lacks compiles, and reads `undefined` at runtime. writing.md's
  exact-footprint rule ("the signature is the decision") has nothing left to
  enforce it.
- **The merges' row algebra.** A `×→+`/`+→+` merge no longer checks that its
  output covers its operands' cases; tests over them now state the merged
  row by annotation.
- **Laws restated, not just re-typed.** The `×→×` and `×→+` units are the
  input-polymorphic `{}` wire, no longer `identity` at `{}`; the interchange
  law needs an explicit widening on its merged side; `replaying`'s source no
  longer subsumes; a collection item that reads its key must coerce.
- **A JS-free algebra.** `PUI`'s collections now import a JavaScript module.
- **Application code leaks.** order-dashboard's packaged controls had to split
  their read row from their input row and drop `Eq a` — every packaged
  control an app writes would follow the same rule.
- `type-equality` became a direct dependency.

## Round 2: footprints as open rows

The free read rows of round 1 (steps 2, 3 and 6) were replaced by **one row
per stage**: a read function's input is the row the stage is fed, and a
business function states its footprint by **row polymorphism** instead of a
closed row — `countLine :: forall r. { count :: Int | r } -> String`.
Unification needs no `Union`, so a bare hole leaves nothing stuck, while a
function reading a field the model lacks is a compile error again, reported
at the view line (checked: `text wrongLine` over the counter model fails
with `Could not match type ( nope :: Int … )`).

| | round 1 (free rows) | round 2 (open rows) |
| --- | --- | --- |
| holey views compiling / running clean | 96 / 96 | 96 / 96 |
| real stack | green | green (`spago test` incl. the 36 exhaustive laws; 1578 smoke checks) |
| footprint checking | gone | **restored**, by unification |
| library leaves (`text`, displays, statuses, `action`, `toCase`, …) | coerced `…Exact` copies | their original one-row signatures |
| application code | packaged controls split their rows | packaged controls unchanged |

What changed on top of round 1's merges, statics, row splits, structural
comparison and runtime guards:

- **The stages** read the row they are fed: `applied`/`updated`/`every`/
  `settled` take `{ | big } -> { | big }`, the panes' classifiers and
  `shownEach`'s projection read `{ | row }`, `foreach`'s projection reads its
  input, `tooltipWith` its content's row.
- **The collections' items** are typed at the whole element row, key
  included, and the carrier **re-sets** the key on each emission
  (`Record.set`): an item that reads its key beside a whole-row editor that
  echoes it is otherwise untypeable. Identity stays unforgeable at runtime;
  the type no longer forbids emitting a key.
- **The logic modules.** 263 of the 363 logic functions took a closed record
  and now take an open one (a mechanical pass; record patterns match open
  rows unchanged), plus 27 view-side read helpers (`cellFace`, `entryFace`,
  …). About 25 bodies were rewritten: handlers that built a literal now
  update (`increment m = m { count = m.count + 1 }`), `Maybe`-returning steps
  keep the row inside the `Maybe`, and functions handing their argument to a
  closed helper project it (`returnLine r = summary (.returnBetween { out:
  r.out, back: r.back })`).
- **The patch idiom does not survive.** "Replace a sub-row with a constant"
  — `with patch # updated (match { l: const })`, `const (const fresh)` — *is*
  subsumption: a constant can only stand for the whole state. The logic now
  writes the field update and the view line names it: timer's
  `button @"Reset" {} # applied restarted`, stopwatch's
  `match { "Start": const beginTiming, … }`, checkout's
  `const orderPlaced` (10 view lines across timer ×6, stopwatch ×2,
  checkout ×2).

What round 2 keeps from round 1, because it comes from equal-input merges or
from the holes themselves: the restated unit and interchange laws, the
input-polymorphic statics and `with`, the structural `Eq`/`Ord` (needed only
by holey builds now — real builds infer again), the row splits, the runtime
hole guards, and the same six residual demos.

## Round 3: the checks restored, the last views fixed

| | round 2 | round 3 |
| --- | --- | --- |
| holey views compiling / running clean | 96 / 96 | **102 / 102** |
| real stack | green | green — `spago test` (the 36 exhaustive laws), bundles, all 1603 smoke checks (the holey sweep's 409 among them), `check-view-model` |
| merge output coverage (a case no handler takes) | unchecked | **checked**, as on `main` |
| row-split checks (`subStrong`, the trace forms, `subChoice`) | dropped | **restored**, except one named gap (below) |
| JavaScript in the algebra | one module (the structural key) | none |

- **`SharedVariantOutputs` is back as on `main`** (the inclusive union).
  Freeing it had let a merge emit a case no handler takes, and it had
  *hidden a real bug*: espresso-bar's "The usual" preset, a constant patch
  (`# with theUsual # updated (match { "The usual": const })`), would have
  replaced the whole model with a partial record at runtime; the restored
  check caught it, and it became a field update like the others.
- **The last ten views are fixed on the view side**, each by naming what
  only its logic said:
  - potluck, reorder and photo-gallery merged an editor (or a whole-row
    stage) beside chrome in a `RecordToRecord.do` — which writing.md
    already forbade but for potluck's named exception — and are now
    pipelines with `# shown` stages; the exception is dropped;
  - crud, reorder and order-form merged actions whose outcome cases only
    their logic named: a single-outcome action now returns its bare
    payload and its line names the case
    (`# toCase @"created" identity`), and order-form's multi-outcome
    submit is followed by its own statuses, the two branches merged by
    `+→×`.
- **Row splits restored.** `subChoice` is `main`'s, unchanged. `subStrong`
  keeps only the forward `Union f b s` — the focus is closed, the
  background inferred and unable to overlap it — so an open model row
  (the logic still a hole) is no longer stuck. The trace forms take **one
  labelled state field** — `feedback @"top" noBids`,
  `folding @"next" @"step" cartStep`, `unfolding @"resume" @"next"
  firstTicket` — so the view line names the field the loop introduces and
  the split is a `Cons`, checked and never stuck. The named gap: nothing
  rejects a model that already has a field of the state's name (the
  reverse `Union` that did cannot be solved while the row is open); the
  state is written over the input, so the loop's state wins.
- **The structural key is plain PureScript** over the ecosystem's
  `Foreign`/`Foreign.Object`; bambik's JS file is gone (one new
  dependency, `foreign`).

## Reading

The twins localised every choice a hole forces into modules no real build
imports. The weakened library of round 1 reached 96 of 102 only by removing
footprint checking; round 2 gave that back by making footprints open rows;
round 3 gave the rest back and finished the rule — 102 of 102 against the
real library, the merges' coverage checks and the row splits' checks as on
`main` but for one named gap. What it costs is written where it applies:
the code-style contract (footprints as open rows, folds as record updates,
no constant patches, no editor as a merge operand, each action's outcomes
named where the action is, trace state labelled on the view line), four
restated laws (the `×→×` unit, interchange with projections, L4's one-row
sharing, the carrier re-setting a collection item's key), and seven words
that treat a hole as absent.
