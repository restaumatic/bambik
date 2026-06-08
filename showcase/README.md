# Showcase: a checkout app as pure optics

The business logic of a tiny checkout flow, expressed **only as optics** — no UI, no
effects, no carrier. `p` stays abstract, so the logic is carrier-independent. It
type-checks (`spago build`); it is not meant to be *run*. Every binding is used by the
app — there is no spare vocabulary.

- [Logic.purs](./Logic.purs) — the whole showcase: four **closed-row leaf** helpers
  (one per optic family) and `checkoutFlow`, which composes them. The body has no type
  annotations: each helper pins its row to a single field/case via `Cons l a () r`, so
  the merges split the form unambiguously and the field types unify from the endpoints.

## The four optics → the four stages

Each optic family is one stage of the pipeline, packaged as a closed-row leaf helper
named after the UI widget that has its shape (the `Example.purs` idiom). Their DDD reading:

| optic | direction | DDD role | leaf (widget) | built on |
|---|---|---|---|---|
| `Lens` | × → × | Value Object accessor ("has-a") | `textInput` — show/edit a field | `editProperty` |
| `Shutter` | × → + | **Process / Saga** | `button` — read model, fire a case | `shutterE` |
| `Prism` | + → + | Value Object discriminator ("is-a") | `notification` — react to a case | `editCase` |
| `Reel` | + → × | **Entity / Aggregate** | `statusBar` — display a case | `reelE` |

## The app

`checkoutFlow` composes the helpers with the four merge do-blocks (the
`{Record,Variant}²` class matrix) and the outer `Semigroupoid.do`:

```
Record ──Lens──▶ Record ──Shutter──▶ Variant ──Prism──▶ Variant ──Reel──▶ Record
RecordToRecord.do   RecordToVariant.do    VariantToVariant.do    VariantToRecord.do
```

Two axes of composition in one definition: **merge** across a row (inside each
do-block, combining the two field/case leaves) and **flow** along the pipeline (the
outer `Semigroupoid.do`). The leaves are closed-row, so the app needs no parameters and
no annotations — the optics build all the structure.

### Why closed-row helpers (and not inline annotations)

`editProperty`/`editCase`/… are *open-rest* (`Cons l a r s`, `r` free) so they compose
onto any record — but that openness makes a merge leaf ambiguous (nothing forces the
leaf to touch only `l`). The fix isn't a typed `identity` (that pins only the focus, not
the rest); it's a leaf whose **row tail is `()`** (`Cons l a () r`). That single fact is
what the merge needs, supplied once per helper rather than at every call site.

The full optic vocabulary (the focus and wrap combinators — `focusRecord`,
`focusVariant`, `resolveProperty`, `retainCase`, `shutterWrap`, `reelWrap`, `lensE`,
`prismE`, …) is documented in [`doc/row-profunctors.md`](../doc/row-profunctors.md).
