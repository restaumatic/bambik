# Showcase: a checkout screen from row-profunctor widgets

A checkout screen assembled from the **UI widget leaves** in
[`Data.Profunctor.Row.Example`](../src/Data/Profunctor/Row/Example.purs) — `textInput`,
`checkbox`, `button`, `notification`, `modal`, `statusBar`, `eventLog` — running at that
module's concrete fake carrier `MyRowToRowProfunctor`. The whole screen **type-checks as
a real composite**, with no UI, no effects, and no hand-written optics: the widgets are
reused as-is and only *composed*.

- [Logic.purs](./Logic.purs) — `checkout`, one screen flowing through all four merge
  directions.

## How it composes

Each widget's *shape* is one of the four row-profunctor directions. `checkout` wires
them with the four **merge** do-blocks and the **flow** of `Semigroupoid.do`:

```
form ──▶ submit | cancel ──▶ placed | aborted ──▶ display
×→×          ×→+                +→+                 +→×
```

```purescript
checkout = Semigroupoid.do
  RecordToRecord.do      -- × → ×   the form
    textInput @"email"
    textInput @"cardNumber"
    checkbox  @"savePayment"
  RecordToVariant.do     -- × → +   action buttons (each fires the form)
    button @"submit"
    button @"cancel"
  VariantToVariant.do    -- + → +   turn each action into an outcome
    notification …       -- submit  → placed
    modal …              -- cancel  → aborted
  VariantToRecord.do     -- + → ×   display each outcome
    statusBar …          -- placed
    eventLog  …          -- aborted
```

| widget | shape | role |
|---|---|---|
| `textInput`, `checkbox` | Record → Record (× → ×) | an editable form field |
| `button` | Record → Variant (× → +) | reads the whole form, fires an action carrying it |
| `notification`, `modal` | Variant → Variant (+ → +) | turn an action event into an outcome |
| `statusBar`, `eventLog` | Variant → Record (+ → ×) | display an outcome |

- **merge** (each `*.do`) combines that direction's widgets;
- **flow** (`Semigroupoid.do`) threads the four stages: `Record → Variant → Variant → Record`;
- `submit`/`cancel` are *distinct actions* (not one-per-field), each carrying the
  completed form as its payload.

The `Variant → Variant` and `Variant → Record` widgets are fully polymorphic, so each
leaf carries a small annotation saying which case it handles — and those annotations also
pin the upstream `button` outputs, so the whole screen resolves to
`MyRowToRowProfunctor (Record form) (Record ())`.

The optics behind the widgets are documented in
[`doc/row-profunctors.md`](../doc/row-profunctors.md).
