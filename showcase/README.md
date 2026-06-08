# Showcase: a checkout screen from row-profunctor widgets

A checkout screen assembled from the **UI widget leaves** in
[`Data.Profunctor.Row.Example`](../src/Data/Profunctor/Row/Example.purs) — `textInput`,
`checkbox`, `button`, `request`, `modal`, `statusBar`, `eventLog` — running at that
module's concrete fake carrier `MyRowToRowProfunctor`. The whole screen **type-checks as
a real composite**, with no UI, no effects, and no hand-written optics: the widgets are
reused as-is and only *composed*.

- [Logic.purs](./Logic.purs) — `checkout`, one screen flowing through all four
  row-profunctor directions to a result page.

## How it composes

Each widget's *shape* is one of the four row-profunctor directions, threaded by
`Semigroupoid.do` (with merge do-blocks for the form, the buttons, the actions, and the page):

```
form ──submit──▶ request ──▶ thankYou | failure  ┐
×→×    ──cancel──▶ modal  ──▶ cancelled           ├──▶ page { thankYou, failure, cancelled }
       ×→+         +→+                            ┘   +→×
```

```purescript
checkout = Semigroupoid.do
  RecordToRecord.do      -- × → ×   the form
    textInput @"email"
    textInput @"cardNumber"
    checkbox  @"savePayment"
  RecordToVariant.do     -- × → +   submit / cancel buttons, each firing the form
    button @"submit"
    button @"cancel"
  VariantToVariant.do    -- + → +   submit hits the backend; cancel bypasses it
    ( request :: …(Variant ( submit :: Record Form ))
                  (Variant ( thankYou :: String, failure :: String )) )
    ( modal @"cancel" @"cancelled" :: …(Variant ( cancel :: Record Form ))
                                       (Variant ( cancelled :: String )) )
  VariantToRecord.do     -- + → ×   render the result page
    statusBar @"thankYou"
    eventLog  @"failure"
    statusBar @"cancelled"
```

| widget | shape | role |
|---|---|---|
| `textInput`, `checkbox` | Record → Record (× → ×) | an editable form field |
| `button` | Record → Variant (× → +) | reads the whole form, fires its action carrying it |
| `request` | Variant → Variant (+ → +) | a fake backend round-trip — its response cases are *deferred* |
| `modal` | Variant → Variant (+ → +) | a local handler (no backend) — transforms one case into another |
| `statusBar`, `eventLog` | Variant → Record (+ → ×) | render a `String` response message onto the page |

Two things worth seeing:

- **`request` is deferred** — its definition declares no response cases
  (`forall w. … (Variant v) (Variant w)`). It's pinned *at the use site* to a contract,
  here `submit → { thankYou | failure }`. One request may resolve to several outcomes; the
  page's handlers are what fix which.
- **`cancel` bypasses the backend.** `request` processes *only* `submit`; `cancel` is
  routed to a local `modal` in the same `VariantToVariant.do` merge and turned straight
  into `cancelled` — it never reaches `request`.

Both handlers in that merge are pinned to their contract (the only annotations on the
screen); everything else is `@l` widgets. The screen resolves to a **checkout status /
thank-you page**:

```purescript
… (Record ( email :: String, cardNumber :: String, savePayment :: Boolean ))
  (Record ( thankYou :: String, failure :: String, cancelled :: String ))
```

The optics behind the widgets are documented in
[`doc/row-profunctors.md`](../doc/row-profunctors.md).
