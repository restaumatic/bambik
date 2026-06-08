# Showcase: a checkout app as pure optics

The business logic of a tiny checkout flow, expressed **only as optics** — no UI, no
effects, no carrier. `p` stays abstract, so the logic is carrier-independent. It
type-checks (`spago build`); it is not meant to be *run*. Every binding is used by the
app — there is no spare vocabulary.

- [Domain.purs](./Domain.purs) — the model: `Form` (input), `Display` (output), `Money`.
- [Logic.purs](./Logic.purs) — four optic-family sections, then the app (`checkoutFlow`).

## The four optics → the four stages

Each optic family contributes two **flow leaves**, and the family's direction *is* one
stage of the pipeline. Their DDD reading:

| optic | direction | DDD role | flow leaves (constructors) |
|---|---|---|---|
| `Lens` | × → × | Value Object accessor ("has-a") | `editProperty`, `lensProperty` |
| `Shutter` | × → + | **Process / Saga** | `shutterE`, `shutter` |
| `Prism` | + → + | Value Object discriminator ("is-a") | `editCase`, `prismCase` |
| `Reel` | + → × | **Entity / Aggregate** | `reelE` |

## The app

`checkoutFlow` composes the eight leaves with the four merge do-blocks (the
`{Record,Variant}²` class matrix) and the outer `Semigroupoid.do`:

```
Form ──Lens──▶ Form ──Shutter──▶ Variant ──Prism──▶ Variant ──Reel──▶ Display
RecordToRecord.do   RecordToVariant.do    VariantToVariant.do    VariantToRecord.do
 (normalize fields)  (lift to channels)     (route channels)       (render notes)
```

Two axes of composition in one definition: **merge** across a row (inside each
do-block, combining the two field/case leaves) and **flow** along the pipeline (the
outer `Semigroupoid.do`). The focuses are the trivial `identity`, so the app needs no
parameters — the optics build all the structure.

The full optic vocabulary (the focus and wrap combinators — `focusRecord`,
`focusVariant`, `resolveProperty`, `retainCase`, `shutterWrap`, `reelWrap`, `lensE`,
`prismE`, …) is documented in [`doc/row-profunctors.md`](../doc/row-profunctors.md).
