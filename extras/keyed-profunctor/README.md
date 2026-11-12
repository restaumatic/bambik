# extras/keyed-profunctor — the keyed square

**A sibling of [`extras/row-profunctor`](../row-profunctor/README.md), not a
part of it.** That root holds profunctors over *rows* — labelled products and
sums whose labels the compiler knows. This one holds the same four shapes
over a **runtime key**: collections, whose members are known only when the
data arrives. Rows are the finitary, `μ`-free fragment of the container
grammar and `Array = μ x. 1 + a × x` is one `μ` later, so the keyed square
stands beside the row shapes rather than under them.

## What it defines

One class per shape, each its strength's analogue over a key — a minimal
**unary** carrier primitive of the one formula `p a b -> p (⊗ₖ a) (⊕ₖ b)`,
where the `×`-power over the keys is the keyed array and the `+`-power one
keyed entity:

| Module | Shape | Strength | Holds |
|---|---|---|---|
| `Data.Profunctor.Keyed` | | | the floor: the formula, what each side's tensor decides, the key discipline, the three forms, and the label forms' two halves `keyed @l`/`keyRestored @l` |
| `Data.Profunctor.Keyed.Acting` | `× → ×` | `Strong` | `class Acting`/`acting`, `actedBy`, `acted @l`, `actedAt @l @t` |
| `Data.Profunctor.Keyed.Dispatching` | `+ → +` | `Choice` | `class Dispatching`/`dispatching`, `dispatched @l`, `dispatchedOn @l @c` |
| `Data.Profunctor.Keyed.Gathering` | `× → +` | `Resolving` | `class Gathering`/`gathering`, `gathered @l`, `gatheredAt @l @t` |
| `Data.Profunctor.Keyed.Accumulating` | `+ → ×` | `Retaining` | `class Accumulating`/`accumulating`, `accumulated @l`, `accumulatedOn @l @c` |

Every class is stated for an arbitrary profunctor `p`. The laws are in the
module headers and are value-tested on `PUI Effect` probes in `spago test`.

## What it stands on

Nothing here mentions `PUI`, a carrier or the DOM, and **nothing here imports
the row root**: the primitives are stated over `Tuple` and `Array` alone, and
the label forms are `dimap`s over them that read and write one field of a
record. Its imports are:

- [`extras/structural`](../structural), for the structural `Ord` the label
  forms supply for a key, so no instance is asked of its type;
- the ecosystem's `profunctor`, `profunctor-lenses` (`prop` for `actedAt`,
  `prism` for `dispatchedOn` — the strengths' own optics), `record` and
  `variant`.

The carrier instances stay behind, as the merge instances do: `PUI` holds
`class Hosting`, the shared keyed reconciler and the four generic instances,
`PUI.Web` the DOM placement. Design note:
[doc/collections-profunctor-algebra.md](../../doc/collections-profunctor-algebra.md).
