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
`Semigroupoid.do` (with merge do-blocks for the form, the buttons, and the page):

```
form ──[submit | cancel]──▶ request ──▶ { thankYou | failure | cancelled } ──▶ page
×→×          ×→+               +→+ (deferred)                                   +→×
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
  ( request              -- + → +   the backend round-trip (deferred → pinned to a contract)
      :: MyRowToRowProfunctor
           (Variant ( submit :: Record Form, cancel :: Record Form ))
           (Variant ( thankYou :: String, failure :: String, cancelled :: String )) )
  VariantToRecord.do     -- + → ×   render the result page
    statusBar @"thankYou"
    eventLog  @"failure"
    statusBar @"cancelled"
```

| widget | shape | role |
|---|---|---|
| `textInput`, `checkbox` | Record → Record (× → ×) | an editable form field |
| `button` | Record → Variant (× → +) | reads the whole form, fires its action carrying it |
| `request` | Variant → Variant (+ → +) | a fake backend round-trip — *both* its actions and its responses are deferred |
| `statusBar`, `eventLog` | Variant → Record (+ → ×) | render a `String` response message onto the page |

The interesting one is **`request`**. Its definition declares *no* cases on either side
(`forall v w. … (Variant v) (Variant w)`): the backend takes whatever actions come in and
may answer with whatever responses. It is pinned **at the use site** to one concrete
contract — here `{ submit, cancel } → { thankYou, failure, cancelled }`. That single
annotation does three jobs:

- decides which **actions** the backend accepts (so the `submit`/`cancel` button merge resolves),
- decides which **responses** it may return (so the page's handlers line up),
- pins the faked **response payloads** (here each response is a `String` status message).

Everything else is `@l` widgets — `textInput @"email"`, `button @"submit"`,
`statusBar @"thankYou"`, … — no annotations.

So the screen resolves to a **checkout status / thank-you page**:

```purescript
… (Record ( email :: String, cardNumber :: String, savePayment :: Boolean ))
  (Record ( thankYou :: String, failure :: String, cancelled :: String ))
```

> A deferred `request` is the *sole* processor of the action variant (it dispatches the
> whole `{ submit, cancel }` set), not one leaf among several in a `VariantToVariant.do`
> merge — a merge would need each handler to declare its own output, which is exactly what
> deferring avoids.

The optics behind the widgets are documented in
[`doc/row-profunctors.md`](../doc/row-profunctors.md).
