# Showcase: a checkout screen from row-profunctor widgets

A checkout screen assembled from the **UI widget leaves** in
[`Data.Profunctor.Row.Example`](../src/Data/Profunctor/Row/Example.purs) — `textInput`,
`checkbox`, `button` — running at that module's concrete fake carrier
`MyRowToRowProfunctor`. The whole screen **type-checks as a real composite**, with no UI,
no effects, and no hand-written optics: the widgets are reused as-is and only *composed*.

- [Logic.purs](./Logic.purs) — `checkout`, one screen: a form of input widgets that flows
  into two action buttons.

## How it composes

Each widget's *shape* is one row-profunctor direction; the screen wires them with two
**merge** do-blocks and the **flow** of `Semigroupoid.do`:

```purescript
checkout = Semigroupoid.do
  RecordToRecord.do      -- the form          (× → ×)
    textInput  @"email"
    textInput  @"cardNumber"
    checkbox   @"savePayment"
  RecordToVariant.do     -- the action buttons (× → +)
    button @"submit"
    button @"cancel"
```

| widget | shape | role |
|---|---|---|
| `textInput`, `checkbox` | Record → Record (× → ×) | an editable form field |
| `button` | Record → Variant (× → +) | reads the whole form, fires an action carrying it |

- **merge** (`RecordToRecord.do`) combines the field widgets into the form;
- **merge** (`RecordToVariant.do`) combines the buttons into one action channel — `submit`
  and `cancel` are *distinct actions* (not one-per-field), each carrying the completed form;
- **flow** (`Semigroupoid.do`) feeds the form into the buttons.

So `checkout :: MyRowToRowProfunctor (Record form) (Variant ( submit :: Record form, cancel :: Record form ))`
— fill the form, press a button, get the action event.

The widget vocabulary (and the `Variant → Record` / `Variant → Variant` widgets like
`statusBar`, `notification`, …) lives in `Example.purs`; the optics behind it all are
documented in [`doc/row-profunctors.md`](../doc/row-profunctors.md).
