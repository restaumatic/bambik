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
  VariantToVariant.do    -- + → +   process each action event
    notification @"submit"
    modal        @"cancel"
  VariantToRecord.do     -- + → ×   record each event
    statusBar @"submit"
    eventLog  @"cancel"
```

| widget | shape | role |
|---|---|---|
| `textInput`, `checkbox` | Record → Record (× → ×) | an editable form field |
| `button` | Record → Variant (× → +) | reads the whole form, fires an action carrying it |
| `notification`, `modal` | Variant → Variant (+ → +) | process an action event |
| `statusBar`, `eventLog` | Variant → Record (+ → ×) | record an event as a field |

- **merge** (each `*.do`) combines that direction's widgets;
- **flow** (`Semigroupoid.do`) threads the four stages: `Record → Variant → Variant → Record`;
- `submit`/`cancel` are *distinct actions* (not one-per-field), each carrying the
  completed form as its payload.

Every widget is **`@l`-parameterized** — `textInput @"email"`, `button @"submit"`,
`notification @"submit"`, … — so each leaf names the single field/case it handles. That
single fact (a closed row, `Cons l a () r`) lets every merge split unambiguously, so the
body needs **no type annotations at all**.

The optics behind the widgets are documented in
[`doc/row-profunctors.md`](../doc/row-profunctors.md).
