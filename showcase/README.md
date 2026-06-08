# Showcase: a checkout screen from row-profunctor widgets

A checkout screen assembled from the **UI widget leaves** in
[`Data.Profunctor.Row.Example`](../src/Data/Profunctor/Row/Example.purs) — `textInput`,
`checkbox`, `button`, `actionButton`, `request`, `modal`, `statusBar`, `eventLog` — running at that
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
  RecordToVariant.do     -- × → +   submit fires the form; cancel fires nothing
    button       @"submit"
    actionButton @"cancel"
  VariantToVariant.do    -- + → +   submit hits the backend; cancel bypasses it
    ( request :: …(Variant ( submit :: Record Form ))
                  (Variant ( thankYou :: String, failure :: String )) )
    ( modal :: …(Variant ( cancel :: Record () ))
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
| `actionButton` | Record → Variant (× → +) | fires an action carrying nothing (`Record ()`) |
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

## The two mixed strengths: Shutter and Reel

The four merge do-blocks above all compose *same-kind* leaves. The two
**mixed-direction strengths** are what make the row profunctors more than a
record/variant calculator — and the checkout has a natural place for each.

```purescript
confirmPayment
  :: MyRowToRowProfunctor
       (Tuple (Record ( amount :: Int ))       (Record ( attempt :: Int )))
       (Either (Variant ( settled :: Record ( amount :: Int ) )) (Record ( attempt :: Int )))
confirmPayment = RecordToVariant.resolve (button @"settled")

runningTotal
  :: MyRowToRowProfunctor
       (Either (Variant ( addItem :: String )) (Record ( total :: Int )))
       (Tuple (Record ( addItem :: String ))   (Record ( total :: Int )))
runningTotal = VariantToRecord.retain (statusBar @"addItem")
```

- **`confirmPayment` is a Shutter** (`resolve`, × → +) — the **loop step**. It
  runs `button @"settled"` against the charge alongside a carried `attempt`
  state, and returns a `Step`: `Left (settled …)` = **Done** (the gateway
  settled), `Right attempt` = **Loop** (still pending, charge again). State
  enters guaranteed (the `Tuple` input) and leaves optionally (a branch of the
  `Either` output), so the iteration can *halt*. A payment-confirmation poll.
- **`runningTotal` is a Reel** (`retain`, + → ×) — the **Mealy step**. It takes
  either a fresh `addItem` command (`Left`) or a resumed `total` (`Right`) and
  always emits an output **plus** the next state (the `Tuple`): the cart never
  finishes, it just winds forward. The aggregate that folds each command into
  retained state.

Neither has a `(->)` instance — a pure function can't loop (Shutter) and can't
hold state across calls (Reel). That missing instance is exactly the
entity/value-object line drawn in the types; see the **DDD reading** in
[`doc/row-profunctors.md`](../doc/row-profunctors.md), which also documents the
optics behind every widget.
