# The collection as the sequence merge

*Design note + rollout. Branch `collections-sequence-merge`.*

## The problem — one root, two symptoms

`foreach`/`foreachWith`/`dynamic`/`each`/`listOf` are the dynamic-collection
vocabulary. Their old implementation rebuilds wholesale on every fed value —
`removeAllChildren parent`, then rebuild every element from scratch
([the pre-PoC `foreach`](../src/PUI/HTML.purs)). That single design choice
produces two apparently-separate complaints:

1. **Rebuild churn.** Every element's DOM, event listeners, and MDC component
   state are destroyed and recreated on every feed, even when only one element's
   data changed (or nothing structural changed at all). This is what makes the
   per-cell-listener grids re-install N listeners per feed.
2. **Empty-array starvation.** `N = 0` builds nothing, so the collection calls
   its downstream `prop` zero times — it **never echoes on an empty array**.
   Inside a gated merge it starves the gate; as an `mvu`/`looped` last stage it
   kills the loop. The only remedy in the codebase is to wrap the collection in
   `displayed` (7 call sites) or route it through `# updates`.

Both symptoms have the *same* root: **a dynamic collection is the runtime-sized,
homogeneous generalization of a row merge, but it is implemented as an ad-hoc
effect instead of a lawful merge.** A static merge already solves both problems,
with machinery the collection simply lacks.

## The insight — collection = dynamic homogeneous merge

The four static merges combine a **fixed, compile-time, heterogeneous** set of
operands over a labeled row. A collection combines a **runtime-sized,
homogeneous** set of operands — one operand type `a`, replicated once per array
element, indexed by position (or a key) instead of a static label. Records are
labeled products; variants are labeled sums; **the collection is the third row
shape — the index-labeled homogeneous sequence.**

Under that lens the merge machinery maps straight across:

| Static heterogeneous merge (`Data.Profunctor.Row.*`) | Dynamic homogeneous collection |
|---|---|
| operands: distinct types, one per label, fixed at compile time | one operand type `a`, replicated per element, runtime-sized |
| input side — `SharedRecordInputs` (broadcast) / `OwnedVariantInputs` (dispatch) | `foreach`: the array distributed elementwise |
| output side — `OwnedRecordOutputs` (disjoint fields, `exactRow`, union) / `SharedVariantOutputs` (`expand`) | collapsed: all elements share one channel `o`; or structure-preserving: `Array o` positional |
| **gate** — per-operand `Ref` holds last *exact* contribution; withhold until every side has spoken; **retain the other side's last value** across feeds | **missing** → wholesale rebuild, no per-element retention |
| **nullary unit `pempty`** announces its `{}` at registration so `N = 0` is lawful | **missing** → empty array can't announce → starves |

The two missing rows are *exactly* the two symptoms. `pempty` **is**
`announce {}` ([PUI.purs](../src/PUI.purs)) — it emits at registration purely to
prime the gate; the collection has no `N = 0` counterpart, so it cannot announce.
And the merge gate holds each operand's contribution across feeds, so a
variant-input event handled by one operand still produces the whole record by
reusing the *other* operand's retained value — the collection throws all
per-element state away every feed.

## The design — the sequence merge as a directional row-profunctor

Promote the collection to a first-class member of the row-profunctor family, a
`Sequence` direction beside the four merges, carrying both missing pieces.

### 1. The retention gate, lifted to a vector

The merge keeps N fixed operand states in N `Ref`s. The sequence merge keeps a
**runtime vector of element instances** and, on each fed array, **reconciles**
rather than rebuilds:

- **survivors** (indices present before and after) are re-fed through their
  channel — no DOM teardown;
- **entrants** (new indices) are built and appended;
- **leavers** (removed indices) are torn down.

This is the merge gate's "retain the other side's last contribution" discipline
lifted from N-fixed to runtime-sized. In trace-quartet terms it is
`Retaining`/`Costrong` at collection granularity: **each element is a retained
Mealy instance**, its state (DOM + listeners + MDC component) resumed across
feeds instead of regenerated.

A subtlety the design must name: retention only works when elements are
**channel-fed**. `foreach :: p a o -> p (Array a) o` feeds each element its value
through `toUser`, so re-feeding a survivor updates it in place. `foreachWith`
(and its `dynamic`/`each` derivatives) is **closure-built** — the element's
content lives in the builder closure, computed at *build* time — so a changed
value genuinely needs a rebuilt element. The principled resolution is to separate
**structure** changes (need a build) from **value** changes (need a feed): a
fully-retaining collection builds structure once per key and feeds value changes
through channels. `foreach` is already channel-fed and thus retainable;
`foreachWith`'s closure form needs a keyed/`Eq`-driven diff to rebuild only the
elements whose value changed. See "Deferred," below.

### 2. The nullary announcing unit

The sequence merge has its own `pempty` — the empty-collection unit that
**announces at registration**, exactly like the record merges'
`pempty = announce {}`. This makes `N = 0` lawful: an empty collection announces
the empty structure instead of going silent, so it keeps gated merges primed and
stays live as an `mvu`/`looped` stage. Every `displayed`/`muted`/`silence`
collection workaround dissolves — the collection is unconditionally a lawful
stage by *having a unit*, precisely as a static merge needs no `displayed`
because `pempty` announces.

### 3. Two output flavors, mirroring shared-vs-owned

- **Collapsed / shared output** (`p (Array a) o`, today's `foreach`): every
  element emits onto one shared channel — the homogeneous analogue of
  `SharedVariantOutputs` (multiplex: "which element fired"). Like a
  variant-output merge, its unit is `silence` (parametricity: an empty
  collection has no `o` to fabricate), and it composes downstream through
  `# updates`/`# toCase` — which is why emitting collections never needed
  `displayed`.
- **Structure-preserving / owned output** (`p s s`, echoing the carrier — or
  `p (Array a) (Array a)`): renders each element and passes the carrier through,
  announcing on empty. This is the flavor whose unit *announces*, and it is the
  honest replacement for `displayed`-wrapped terminal collections.

`foreach`/`foreachWith`/`dynamic`/`each`/`listOf` all become derived forms of the
one direction.

## Rolled out (this branch)

The approach is the adopted mechanism, not a sketch. Vocabulary in
[HTML.purs](../src/PUI/HTML.purs):

- **Retaining `foreach`** — positional reconciliation: survivors re-fed in place
  (no DOM teardown), entrants appended, a shrink rebuilds. Same type and name, so
  `listOf` and every `foreach`-based list inherit the win transparently.
- **`foreachModel :: (s -> Array a) -> PUI Web a o -> PUI Web s s`** — the
  structure-preserving, self-announcing collection stage (channel-fed items):
  renders `proj s` and echoes the carrier `s`, so it never starves on an empty
  array. The nullary-unit flavor as a self-contained stage.
- **`foreachWithModel :: (s -> Array a) -> (a -> PUI Web {} o) -> PUI Web s s`** —
  the same self-announcing stage for **structure-from-value** collections whose
  element structure genuinely varies (so they stay closure-built).
- **`attrWith :: String -> (i -> String) -> PUI Web i o -> PUI Web i o`** — the
  value-computed attribute, the channel-fed counterpart of static `attr`/`:=`.
  This is the enabler that lets a **fixed structure** whose *values* vary be fed
  as data through the retaining `foreach` (built once, updated in place) instead
  of rebuilt by a `dynamic`/`foreachWith` closure. It splits the collection story
  cleanly: `attrWith` + `foreach` when only values change over a fixed structure;
  `foreachWith`/`dynamic`/`each`/`foreachWithModel` when structure itself varies.

### Demos retrofitted (all verified, headless Chrome / CDP)

- **Channel-fed grids/canvas** — tic-tac-toe, calculator, color-mixer, cells, and
  circle-drawer dropped their `dynamic`+`each`/`foreachWith` closure rebuild for a
  retaining `foreach` fed the structure as data (content via `text`, style/coords
  via `attrWith`, identity via `clicked … # rmap`). Verified: a move / keypress /
  preset / selection / edit / resize keeps every cell or circle DOM node — cells'
  **837 `<td>` nodes survive an edit or selection** (the headline win); no
  `data-*` anywhere.
- **Self-announcing terminal collections** — stopwatch laps (`foreachModel`),
  markdown-previewer preview and photo-gallery content pane (`foreachWithModel`)
  dropped their `# lcmap proj … # displayed`. Verified: empty input no longer
  starves and the `mvu` loop stays alive.
- **Lists** (`listOf`: todomvc, crud, inbox, weather, quiz, shopping-cart,
  movie-browser) retain automatically through the new `foreach`: grow/append and
  same-length refeed keep their `<li>` nodes and update styling in place.

`# displayed` now appears only on **non-collection** passthroughs (a static chrome
sibling in calculator/color-mixer, the `provided` pane lines in checkout/quiz).

### Deferred

- The **formal `Sequence` typeclass** + its `Co`-retraction and qualified-do sugar
  (following the `RecordToVariant`/`VariantToRecord` template). The runtime and
  vocabulary are fully adopted; lifting them to a named class member of the
  row-profunctor family is the remaining formalization.
- **Keyed reconciliation** for reorder/insert (the current reconciliation is
  positional; a `key`/`Eq` diff would retain identity under reordering, and
  de-churn the closure builders too — not needed by any current demo, whose
  collections are fixed-order or human-paced).

## Why this framing

It keeps the collection inside bambik's own direction/merge scheme rather than
importing an external abstraction: the collection is *literally* a merge, its
retention *is* the gate, its empty case *is* `pempty`. The vocabulary the library
already teaches — shared vs owned, gate, announce, `pempty`, `Retaining`/`Costrong`
— explains the collection with nothing new to learn. Contrast the
`Traversing`/`wander` framing (branch `collections-traversing-wander`).
