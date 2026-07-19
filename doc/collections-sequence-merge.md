# The collection as the sequence merge

*Design note + proof-of-concept. Branch `collections-sequence-merge`.*

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

## Proof-of-concept (this branch)

Behind the existing surface, deferring the full class elevation:

- **Retaining `foreach`** ([HTML.purs](../src/PUI/HTML.purs)) — positional
  reconciliation: survivors re-fed in place (no DOM teardown), entrants
  appended, shrink falls back to a wholesale rebuild (the one remaining churn
  case, a keyed diff away). Same type and name, so `listOf` and every
  `foreach`-based demo inherit the win.
- **`foreachModel :: (s -> Array a) -> PUI Web a o -> PUI Web s s`** — the
  structure-preserving, self-announcing collection stage: renders `proj s` as a
  retaining collection and echoes the carrier `s` every feed, so it is an
  unconditional pass-through that announces even when `proj s` is empty. This is
  the nullary-unit flavor as a self-contained stage — it dissolves the
  `# lcmap proj # displayed` idiom.

### What the PoC demonstrates (verified, headless Chrome / CDP)

- **Stopwatch** rewritten from `ul (foreach (li (text # forValue))) # lcmap
  lapLines # displayed` to `ul (foreachModel lapLines (li (text # forValue)))` —
  `displayed` dropped. Adding a lap keeps the existing lap `<li>` DOM nodes
  (tagged nodes survive → **no churn**); the readout keeps advancing and reset
  works (**loop stays alive without `displayed`**; the empty lap list still
  echoes the carrier).
- **Regression smoke** (`listOf`-based): todomvc grow/append + same-length toggle
  retain their `<li>` nodes and flip `clWhen` styling; crud selection retains all
  nodes and applies selected styling. The `foreachWith` grids (calculator,
  tic-tac-toe, color-mixer, cells) are untouched.

### Deferred (out of PoC scope)

- The `Sequence` **class** + `Co`-retraction, qualified-do sugar, and the
  `(->)`-instance verdict, following the
  `RecordToVariant`/`VariantToRecord` template.
- **`foreachWith`/`dynamic`/`each` keyed diff** — closure-built elements need an
  `Eq a`/key projection to rebuild only changed elements (and to retain structure
  under reorder/insert). This is where the grids' churn is addressed.
- Migrating the remaining `displayed`/`muted`/`silence` collection sites.

## Why this framing

It keeps the collection inside bambik's own direction/merge scheme rather than
importing an external abstraction: the collection is *literally* a merge, its
retention *is* the gate, its empty case *is* `pempty`. The vocabulary the library
already teaches — shared vs owned, gate, announce, `pempty`, `Retaining`/`Costrong`
— explains the collection with nothing new to learn. Contrast the
`Traversing`/`wander` framing (branch `collections-traversing-wander`).
