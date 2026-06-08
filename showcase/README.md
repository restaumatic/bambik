# Showcase: an order app as pure optics

The entire business logic of a tiny order application, expressed **only as optics** —
no UI, no effects, no carrier. Every binding is a pure optic value or a composite
built from the merge do-blocks; the profunctor `p` stays abstract, so the logic is
carrier-independent. It type-checks (`spago build`); it is not meant to be *run*.

- [Domain.purs](./Domain.purs) — the model: `Record`s (value objects, the aggregate
  entity) and `Variant`s (events, closed unions).
- [Logic.purs](./Logic.purs) — the logic in five sections: one per optic family, then
  the app (`checkoutFlow`) that composes them.

## What it covers

**The four optics** (the `{Record,Variant}²` corners), with their DDD reading. Each
family contributes a **flow leaf** to the app, plus more of its vocabulary:

| optic | direction | DDD role | flow leaf | more vocabulary |
|---|---|---|---|---|
| `Lens` | × → × | Value Object accessor ("has-a") | `editProperty` | `lensProperty`, `focusRecord`, `lensE` |
| `Shutter` | × → + | **Process / Saga** | `shutterE` | `shutter`, `resolveProperty`, `shutterWrap` |
| `Prism` | + → + | Value Object discriminator ("is-a") | `editCase` | `prismCase`, `focusVariant`, `prismE` |
| `Reel` | + → × | **Entity / Aggregate** | `reelE` | `reel`, `retainCase`, `reelWrap` |

**`checkoutFlow`** is the app — built *from the optics above*. It composes the four
flow leaves with the four merge do-blocks (the `{Record,Variant}²` class matrix) and
the outer `Semigroupoid.do`:

```
Record ──Lens──▶ Record ──Shutter──▶ Variant ──Prism──▶ Variant ──Reel──▶ Record
 RecordToRecord.do    RecordToVariant.do    VariantToVariant.do    VariantToRecord.do
```

Two axes of composition in one definition: **merge** across a row (inside each
do-block, combining the two field/case leaves) and **flow** along the pipeline (the
outer `Semigroupoid.do`). The four optic families are exactly the four stages. The
focuses are the trivial `identity`, so the app needs no parameters — the optics build
all the structure.

See [`doc/row-profunctors.md`](../doc/row-profunctors.md) for the theory behind all of this.
