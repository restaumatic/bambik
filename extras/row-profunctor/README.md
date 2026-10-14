# extras/row-profunctor — a candidate standalone library

**This root is not an ecosystem complement, and not really bambik either.**
Its two siblings, [`extras/profunctor`](../profunctor/README.md) and
[`extras/lenses`](../lenses/README.md), claim `Data.Profunctor.*` and
`Data.Lens.*` names because they belong in those *existing* upstream
families. This root claims `Data.Profunctor.Row*` for something the ecosystem
has no home for at all: **row profunctors** — profunctors whose parameters are
row types under `Record` (the product) or `Variant` (the sum).

That is a general theory, not a UI one. It is the strongest candidate here for
extraction into **its own standalone package**, published on its own, with
bambik as merely its first client.

## Why it stands alone

Nothing in this root mentions `PUI`, a carrier, the DOM, or a user — verified
by `grep`: no module here imports `PUI`, `PUI.Web`, or any carrier type. (The
one `Effect` import is `unsafeThrow` in `Row.purs`, used for a single
unreachable branch in `splitVariant`; it returns a pure value and is not a
carrier dependency, but a `effect` package pin would ride along.) What this
root defines is a general algebra:

- **Four merge classes**, one per shape, each the label-strictified
  `(M, N)`-monoidal structure for `M, N ∈ {×, +}`; the units are borrowed
  from `Category` (`identity` at the unit object) wherever a wire fits, so
  every shape is a monoid on labelled rows.
- **The container action** (`Data.Profunctor.Acting`), one `μ` past the rows —
  rows being the finitary, `μ`-free, label-strict fragment of the same
  container grammar.

Every one of those is stated for an arbitrary profunctor `p`. The laws are in
the module headers and are **value-testable on `(->)`**, which is how the
`spago test` suite exercises them without a UI at all — the clearest evidence
that this layer does not depend on the one downstream of it.

| Module | Holds |
|---|---|
| `Data.Profunctor.Row` | the shared floor: the row-constraint vocabulary, the split `splitVariant` and `exactRow`, and every one-sided word — `atField`/`toField`, `atCase`/`toCase`, `settled` — so no shape module imports another |
| `Data.Profunctor.Row.RecordToRecord` | the `× → ×` merge, `focusField` |
| `Data.Profunctor.Row.VariantToVariant` | the `+ → +` merge, `focusCase` |
| `Data.Profunctor.Row.RecordToVariant` | the `× → +` merge, `joined` |
| `Data.Profunctor.Row.VariantToRecord` | the `+ → ×` merge, `unshown` |
| `Data.Profunctor.Acting` | the container action `class Acting`/`actedBy`, `acted @l`, `actedAt @l @t` |
| `Data.Profunctor.Row.Structural` | structural `Eq`/`Ord` of plain values through `Foreign`, for options and keys no instance closes (holes) |

## What extraction would take

Not a rewrite — the module boundary is already clean — but three things worth
knowing before cutting a package:

1. **The carrier instances stay behind.** The merge instances, `class Hosting`
   and the keyed reconciler live in `PUI`; DOM placement in `PUI.Web`. That
   split is already enforced (this root has no `PUI` import), so extraction is
   a move, not a disentangling.
2. **One dependency comes along**: `extras/lenses`, for `prismE` (the
   existential `Prism` constructor `focusCase` stands on). `extras/profunctor`
   is not imported here any more — the row layer has no row form of the
   coined strengths or co-strengths — so the packaging question is whether
   the two ship as one `purescript-row-profunctor` package or as a small
   stack. (Each root here is named after the upstream package it belongs to
   or would become, so the directory names already carry the answer's shape.)
3. **The toolchain pins are milder than they look.** These modules import only
   the *standard* `Data.Variant` API (`inj`/`on`/`case_`/`expand`/`Contractable`)
   — no `Prim.Variant` reference anywhere — so the row layer itself is not
   written in fork-only syntax. What needs the pinned PureScript fork and the
   `Prim.Variant`-patched `variant` is **calling** code: the variant row sugar
   `[ l :: T | r ]` and `.label` constructors that demos and applications use,
   which work only when `Data.Variant.Variant` *is* the compiler's built-in.
   So a standalone package would compile on stock PureScript, while its
   ergonomic surface for consumers would still want the fork until the sugar
   lands upstream. Worth measuring before publishing, not assuming.

   (Its two dependency roots are cleaner still: `extras/profunctor` and
   `extras/lenses` use no row sugar at all — positional `Tuple`/`Either`
   only — so they are fork-independent outright.)

Meanwhile `src/` holds the carrier and its vocabularies — `PUI`, `PUI.Gate`,
`PUI.Web`, `PUI.Web.*` — so the split already reads: **`src/` is the UI
library, `extras/` is the general algebra it stands on.**

## Building

Covered by the single glob `extras/**/*.purs` in the repo's `spago.dhall`.

This root matters most to a consuming application: **`Data.Profunctor.Row.RecordToRecord`
and its three siblings are what an app imports to write a `.do` merge**, so an
app missing the `extras` glob fails on its own first merge, not on some library
internal. Spago globs a git dependency as `.spago/<pkg>/<ver>/src/**/*.purs`,
hardcoded, so modules outside `src/` are otherwise never compiled. See the
skill's `bootstrap.md`.
