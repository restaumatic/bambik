# Showcase: an order app as pure optics

The entire business logic of a tiny order application, expressed **only as optics** —
no UI, no effects, no carrier. Every binding is a pure optic value or a composite
built from the merge do-blocks; the profunctor `p` stays abstract, so the logic is
carrier-independent. It type-checks (`spago build`); it is not meant to be *run*.

- [Domain.purs](./Domain.purs) — the model: `Record`s (value objects, the aggregate
  entity) and `Variant`s (events, closed unions).
- [Logic.purs](./Logic.purs) — the logic, in five parts.

## What it covers

**The four optics** (the `{Record,Variant}²` corners), with their DDD reading:

| optic | direction | DDD role | constructors shown |
|---|---|---|---|
| `Lens` | × → × | Value Object accessor ("has-a") | `editProperty`, `lensProperty`, `focusRecord`, `lensE` |
| `Prism` | + → + | Value Object discriminator ("is-a") | `editCase`, `prismCase`, `focusVariant`, `prismE` |
| `Reel` | + → × | **Entity / Aggregate** | `reelE`, `reel`, `retainCase`, `reelWrap` |
| `Shutter` | × → + | **Process / Saga** | `shutterE`, `shutter`, `resolveProperty`, `shutterWrap` |

**The four merge do-blocks** — the full class matrix, composing optics into whole
records and variants:

| do-block | direction | shape |
|---|---|---|
| `RecordToRecord.do` | × → × | assemble a record from field leaves |
| `RecordToVariant.do` | × → + | form → event (validate) |
| `VariantToVariant.do` | + → + | dispatch + merge (route) |
| `VariantToRecord.do` | + → × | event → display (render) |

See [`doc/row-profunctors.md`](../doc/row-profunctors.md) for the theory behind all of this.
