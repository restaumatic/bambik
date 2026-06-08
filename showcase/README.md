# Showcase: a checkout screen from row-profunctor widgets

A checkout screen assembled from the **UI widget leaves** in
[`Data.Profunctor.Row.Example`](../src/Data/Profunctor/Row/Example.purs) — `textInput`,
`checkbox`, `submit`, `actionButton`, `request`, `modal`, `notification`, `statusBar`,
`eventLog` — running at that module's concrete fake carrier `MyRowToRowProfunctor`. The
whole screen **type-checks as a real composite**, with no UI, no effects, and no
hand-written optics: the widgets are reused as-is and only *composed*.

- [Logic.purs](./Logic.purs) — `checkout`, one screen flowing through all four
  row-profunctor directions to a result page.

## How it composes

Each widget's *shape* is one of the four row-profunctor directions, threaded by
`Semigroupoid.do` (with merge do-blocks for the form, the actions, the handlers, and the page):

```
form ──submit──▶ request ──▶ thankYou | failure  ┐
×→×    └─editing─▶ (loop back, prompt to fix)     ├──▶ page { thankYou, failure, editing, cancelled }
       ──cancel──▶ modal   ──▶ cancelled          ┘   +→×
       ×→+          +→+
```

```purescript
checkout = Semigroupoid.do
  RecordToRecord.do      -- × → ×   the form
    textInput @"email"
    textInput @"cardNumber"
    checkbox  @"savePayment"
  RecordToVariant.do     -- × → +   submit is a Shutter; cancel fires nothing
    submit @"submit" @"editing"
    actionButton @"cancel"
  VariantToVariant.do    -- + → +   submit hits the backend; cancel bypasses it; editing loops
    ( request :: …(Variant ( submit :: Record Form ))
                  (Variant ( thankYou :: String, failure :: String )) )
    ( notification :: …(Variant ( editing :: String )) (Variant ( editing :: String )) )
    ( modal :: …(Variant ( cancel :: Record () )) (Variant ( cancelled :: String )) )
  VariantToRecord.do     -- + → ×   the page is built from Reels
    statusBar @"thankYou"
    eventLog  @"failure"
    statusBar @"editing"
    statusBar @"cancelled"
```

| widget | shape | role |
|---|---|---|
| `textInput`, `checkbox` | Record → Record (× → ×) | an editable form field |
| `submit @done @loop` | Record → Variant (× → +) | **a Shutter** — reads the form, either fires the `done` case (Done) or loops to the `loop` case (Loop); both labels caller-chosen |
| `actionButton` | Record → Variant (× → +) | fires an action carrying nothing (`Record ()`) |
| `request` | Variant → Variant (+ → +) | a fake backend round-trip — its response cases are *deferred* |
| `notification`, `modal` | Variant → Variant (+ → +) | local handlers (no backend) — route one case to another |
| `statusBar`, `eventLog` | Variant → Record (+ → ×) | **Reels** — render a `String` message onto the page, retaining it |

## Where Shutter and Reel live: inside the leaves

The four merge do-blocks all compose *same-kind* leaves. The two
**mixed-direction strengths** — Shutter (`× → +`, `resolve`) and Reel
(`+ → ×`, `retain`) — aren't extra stages bolted onto the pipeline; they're
baked into the leaves whose row-direction *is* their shape:

- **`submit @done @loop` is a Shutter.** Built on `shutter`, it reads the whole
  form and returns a `Step`: fire the `done` case carrying the form (**Done** →
  on to the backend), or snap back to the `loop` case with a prompt (**Loop** →
  the form is returned for correction). Both output labels are caller-chosen
  (here `submit`/`editing`), and the loop channel is a real output case the page
  renders. A `× → +` action that can iterate is the canonical place a Shutter
  belongs — a stateless `button` can only fire once.
- **`statusBar` and `eventLog` are Reels.** Built on `reel`, each is a `+ → ×`
  page entity that *retains* its content across renders — a status that holds,
  a log that accumulates. The retention is the carrier's; the leaf just declares
  the Reel shape. A status/log that holds state is the canonical place a Reel
  belongs.

`request`, `modal`, and `notification` are `+ → +` (VariantToVariant) — neither
direction — so they're *not* Shutters or Reels: a Shutter's Loop branch or a
Reel's resume branch would have to conjure an output variant case from nothing,
which the type won't allow. The strengths only live where the direction matches.

Neither strength has a `(->)` instance — a pure function can't loop (Shutter)
and can't hold state across calls (Reel). That missing instance is exactly the
entity/value-object line drawn in the types; see the **DDD reading** in
[`doc/row-profunctors.md`](../doc/row-profunctors.md), which also documents the
optics behind every widget.

Other notes:

- **`request` is deferred** — its definition declares no response cases
  (`forall w. … (Variant v) (Variant w)`). It's pinned *at the use site* to a contract,
  here `submit → { thankYou | failure }`. One request may resolve to several outcomes; the
  page's handlers are what fix which.
- **`cancel` bypasses the backend.** `request` processes *only* `submit`; `cancel` is
  routed to a local `modal` in the same `VariantToVariant.do` merge and turned straight
  into `cancelled` — it never reaches `request`.

The screen resolves to a **checkout status / thank-you page**:

```purescript
… (Record ( email :: String, cardNumber :: String, savePayment :: Boolean ))
  (Record ( thankYou :: String, failure :: String, editing :: String, cancelled :: String ))
```
