# Showcase: a checkout screen from row-profunctor widgets

A checkout screen assembled from the **UI widget leaves** in
[`Data.Profunctor.Row.Example`](../src/Data/Profunctor/Row/Example.purs) — `textInput`,
`checkbox`, `button`, `request`, `statusBar`, `eventLog` — running at that module's
concrete fake carrier `MyRowToRowProfunctor`. The whole screen **type-checks as a real
composite**, with no UI, no effects, and no hand-written optics: the widgets are reused
as-is and only *composed*.

- [Logic.purs](./Logic.purs) — `checkout`, one screen flowing through all four
  row-profunctor directions to a result page.

## How it composes

Each widget's *shape* is one of the four row-profunctor directions, threaded by
`Semigroupoid.do` (with `RecordToRecord.do` / `VariantToRecord.do` merges for the form
and the page):

```
form ──submit──▶ request ──▶ { thankYou | failure } ──▶ page { thankYou, failure }
×→×        ×→+      +→+ (deferred)                        +→×
```

```purescript
checkout = Semigroupoid.do
  RecordToRecord.do      -- × → ×   the form
    textInput @"email"
    textInput @"cardNumber"
    checkbox  @"savePayment"
  button  @"submit"      -- × → +   the submit button fires the whole form
  request @"submit"      -- + → +   backend round-trip: response is thankYou | failure (deferred)
  VariantToRecord.do     -- + → ×   render the result page
    statusBar @"thankYou"
    eventLog  @"failure"
```

| widget | shape | role |
|---|---|---|
| `textInput`, `checkbox` | Record → Record (× → ×) | an editable form field |
| `button` | Record → Variant (× → +) | reads the whole form, fires the `submit` action carrying it |
| `request` | Variant → Variant (+ → +) | a fake backend round-trip — its response cases are *deferred* |
| `statusBar`, `eventLog` | Variant → Record (+ → ×) | render a response onto the result page |

The interesting one is **`request`**: it does *not* declare its output cases. A single
request may resolve to `thankYou` **or** `failure`, so its response variant is left
deferred and **inferred from the page below** — the `statusBar @"thankYou"` /
`eventLog @"failure"` handlers are what fix what the backend can return.

Because the backend is faked, the response *payloads* aren't determined by anything, so
the one type signature on `checkout` pins them (`thankYou` carries the placed order,
`failure` an error string). Everything else is `@l` widgets — no inline annotations.

So the screen resolves to a **checkout status / thank-you page**:

```purescript
… (Record ( email :: String, cardNumber :: String, savePayment :: Boolean ))
  (Record ( thankYou :: Record ( email :: String, cardNumber :: String, savePayment :: Boolean )
          , failure  :: String ))
```

> A deferred response can't share a `VariantToVariant.do` *merge* (the merge couldn't tell
> which response cases came from which handler), so `request` is the sole processor of
> `submit` here. Multiple merged actions (submit/cancel/…) need each handler to declare
> its output.

The optics behind the widgets are documented in
[`doc/row-profunctors.md`](../doc/row-profunctors.md).
