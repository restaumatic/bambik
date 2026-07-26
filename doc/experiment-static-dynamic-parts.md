# Experiment: static parts, dynamic parts, and when each is known

A widget is made of **parts** — a caption, a label, an icon, an option list, a
value, a column header. Each part's content becomes known at one of two times:
at **construction**, from a PureScript value the author writes down; or at
**feed**, from the model flowing through the channel. Today the MDC vocabulary
decides that time *for the caller*: a part is either a field of the config
record (always static) or the `i` of `PUI Web i o` (always dynamic), and no
part can be both.

This note scans every MDC member, records which parts are which, and works out
what it costs to let the caller choose — including a prototype (`Slot`) that is
on this branch and passing.

## The scan

`S` = static only (a config field or hardcoded markup; cannot follow data).
`D` = dynamic only (arrives through the channel; has no static form).
`B` = **wanted both ways**, only one available. The last column names the part
that is stuck.

### Components — `×→×` editors

| Component | Static parts | Dynamic parts | Stuck (`B`) |
|---|---|---|---|
| `filledTextField` | `floatingLabel`; helper-line text (hardcoded empty) | `value`; the label's float-above class (`clDyn … isJust`) | helper/error text — no path at all |
| `debouncedTextField` | `floatingLabel`, `millis` | `value` | as above |
| `filledTextArea` | `columns`, `rows` | `value` | — |
| `checkbox` | label content — a **widget slot** (`PUI Web {} {}`), so structured but pinned at `{}` | `value` | label content: cannot read the model (`{}`, not `i`) |
| `radioButton` | `options` | selection | `options` |
| `toggleSwitch` | `label` | `value` | `label` |
| `slider` / `sliderLive` | `label`, `min`, `max`, `step` | `value` | `min`/`max` (a bound derived from the model) |
| `select` | `floatingLabel`, `options` | selection | `options` |
| `segmentedButton` | `options` | selection | `options` |
| `tabBar` | `options` (`value`/`label`/`icon`) | selection | `options` |
| `filterChip` | `label` | `value` | `label` |
| `iconToggle` | `onIcon`, `offIcon`, `label` | `value` | — **but see below**: this component *is* a dynamic icon, hand-encoded as two static ones |

### Components — `×→×` displays, `×→+` events, `+→×` statuses

| Component | Static parts | Dynamic parts | Stuck (`B`) |
|---|---|---|---|
| `indeterminateLinearProgress` | aria label | `busy` | — |
| `indeterminateCircularProgress` | aria label, 48px inline size | `busy` | — |
| `linearProgress` | aria label | `value` | — |
| `button` | `label`, `icon` | the fed record (replayed on click) | `label` (Save / Saving…), `icon` |
| `fab` | `icon`, `label` | fed record | `label` |
| `iconButton` | `icon`, `label` | fed record | `icon` |
| `menuItem` | `label` | fed record | `label` (data-driven menus) |
| `snackbar` | auto-dismiss timing | **the message** (the `event` case payload) | — the one component whose text is dynamic by construction |
| `banner` | the `"Dismiss"` action label (hardcoded) | the message | `"Dismiss"` — not even reachable for translation |

### Collection component

| Component | Static parts | Dynamic parts | Stuck (`B`) |
|---|---|---|---|
| `listOf` | `selected` — **a function of the element**, i.e. already a fed slot | the array; each item widget re-fed by key | — |

### Oculars

| Ocular | Static parts | Dynamic parts | Stuck (`B`) |
|---|---|---|---|
| `card` | `caption` | content | `caption` → **prototyped as a `Slot`** |
| `topAppBar` | `title` | content | `title` → **prototyped as a `Slot`** |
| `dialog` | `title` | open/close (opens on feed, closes on emission) | `title` (`Delete "Lunch on Thursday?"`) |
| `simpleDialog` | `title`, `confirm` | open/close | both |
| `menu` | `label` | items | `label` |
| `dataTable` | `label`, `columns` | rows (content) | `columns` (pivots, chosen columns) |
| `imageList` | `columns` | content | `columns` (responsive) |
| `layoutCell` | `span` | content | `span` (responsive) |
| `drawer` | `title`, `subtitle` | nav and content (both fed) | `title`/`subtitle` |
| `tooltip` | `text` | — | `text` |
| `cardActions`, `chipSet`, `list`, `listItem`, `dataRow`, `dataCell`, `layoutGrid` | all (pure chrome) | content | — genuinely static |
| typography (`headline1`–`6`, `subtitle1/2`, `body1/2`, `caption`, `overline`), `elevation1/10/20` | all | content | — genuinely static |

### Announcing statics

| Static | Static parts | Stuck (`B`) |
|---|---|---|
| `divider` | fixed markup | — genuinely static |
| `imageListItem` | `src`, `label` | **both** — the sharpest case, see below |

## What the scan shows

**1. The vocabulary is lopsided.** Of ~45 members, three carry a dynamic
*text* part (`snackbar`, `banner`, and `listOf`'s items); everything
presentational is static-only. Sixteen members have at least one part a real
app would want to drive from data.

**2. The escape hatches exist, one level down.** `PUI.HTML` already has the
static/dynamic pairing MDC lacks: `staticText`/`text`, `attr`/`attrWith`,
`cl`/`clWhen`/`clDyn`. Six demos use them (tic-tac-toe, cells, calculator,
circle-drawer, color-mixer, restaurant-menu). So the distinction is not new —
it is just missing from the *component* layer, where it is expressed by
*leaving the vocabulary* instead.

**3. Two workarounds are visible in the codebase.**

- `iconToggle { onIcon, offIcon, label }` is a whole component whose reason to
  exist is that `iconButton`'s icon can't follow data. A dynamic icon slot
  subsumes it.
- photo-gallery cannot build an image list from data, because
  `imageListItem { src, label }` is a static. It falls back to
  `displayed $ dynamic \m -> each (albumPhotos m) …`, which **tears down and
  rebuilds the whole list on every album change** — precisely the wholesale
  rebuild the keyed `foreach` regime exists to avoid. Two static parts cost a
  reconciled collection.

**4. Chrome sits outside the fed region.** Every demo reads
`card { caption: … } $ (pipeline # mvu seed)` — the ocular wraps the `mvu`
stage, so nothing ever feeds it. The static/dynamic boundary is not primarily a
vocabulary fact; it **coincides with the `mvu` seam**. Making chrome dynamic
means pulling it inside: `(card { caption: fed … } $ pipeline) # mvu seed`.
This is the one structural change a slot forces on a call site, and the
photo-gallery diff on this branch shows it.

## The staging question

> pass static parameters and dynamic parameters separately, and pass dynamic
> parameters later, so the static view can be viewed/tested/peeked sooner

Three times already exist, in order, and the API already separates them:

| Stage | When | What is fixed | What you can see |
|---|---|---|---|
| **construction** | PureScript application (`card { caption: "Quiz" }`) | config record | nothing yet — a value |
| **registration** | `Web`, at `body` | all chrome DOM built, MDC components created | **the static view** |
| **feed** | `with seed`, `mvu`, an upstream emission | dynamic parts filled | the live view |

So "pass the dynamic parameters later" is already the shape of the library:
statics are function arguments, dynamics arrive through the channel, and the
channel can be supplied later or not at all. Two peeks work *today*:

- `body $ widget` — registration only, no feed: the static skeleton.
- `body $ with sampleModel $ widget` — the same skeleton with sample data,
  using the ordinary `with`. No test-only machinery needed.

What was missing is not staging but **per-part choice of stage**. That is what
this experiment adds.

## The design: `Slot`

```purescript
data Slot i a
  = Absent           -- the part is not there, and neither is its DOM node
  | Pinned a         -- known at construction; renders at registration, no channel
  | Fed (i -> a)     -- known at feed; renders on the first feed
```

`Pinned a` and `Fed (const a)` display the same thing but are not the same
widget: only `Pinned` can be seen without data. Keeping them distinct is
exactly what makes a widget's static half a peekable artifact — the type
records which parts are visible before the model exists.

Call sites:

```purescript
card { caption: "Quiz" }         -- Pinned  (bare constant, lifted by the OptSlot tag)
card { caption: fed _.album }    -- Fed     (named)
card {}                          -- Absent  (default)
```

### Results (measured on this branch)

- **`card` and `topAppBar` converted; all 31 demos compile unchanged.** Static
  call sites keep their exact syntax and their exact behaviour, and the build
  produces no new warnings. The lifting rides the same `ConvertOption`
  mechanism already used for optional fields.
- **A widget stays polarity-agnostic until a call site asks for a dynamic
  part.** `Absent`/`Pinned` leave `i` free, so `card {…}` is still usable at
  any polarity; only `fed` constrains `i`. The cost is that `card` and
  `topAppBar` are no longer `Ocular`s (they read their input) — the precedent
  is `drawer`, `clWhen` and `attrWith`, which already gave up `Ocular` for the
  same reason.
- **The dynamic case must be *named*.** Lifting a bare projection
  (`{ caption: _.album }`) does not typecheck: an instance chain can only
  commit on a concrete `from`, and `_.album` is still
  `{ album :: t | r } -> t` at conversion time — "the instance head contains
  unknown type variables" (doc/type-errors.md's family). Hence `fed`. This
  turns out to be worth keeping: the config record then says out loud which
  parts wait for data.
- **`Absent` has to be a separate case, not `Fed (const Nothing)`.** Whether a
  part's *node exists* is construction-time knowledge; a function of `i` can't
  be consulted before the first feed, so folding absence into the slot would
  force every optional part's DOM node to exist. The three-way split preserves
  today's behaviour exactly.
- **Verified in the browser** (scripts/smoke/tests/photo-gallery.mjs): the fed
  title renders on the first feed, follows the model, and updates *its own text
  node in place* — the chrome is built once, so a slot is a channel, not a
  rebuild. The pinned drawer title is unaffected.
- **One dynamic slot deleted a pipeline stage.** photo-gallery's separate
  `headline2 text # forValue # forField @"album" # tapped` album heading is
  gone; the app bar names the open album instead.

### Alternatives considered

- **Twin words** (`card`/`cardWith`, the `staticText`/`text` pattern lifted to
  components). Rejected: it doubles a ~45-word vocabulary and the *combination*
  cases (a pinned caption on a dynamic-titled dialog) multiply.
- **Widget slots** (`checkbox`'s `labelContent` generalized: pass a
  `PUI Web i {}` for the part). Strictly more expressive — the part can be any
  widget, styled and conditional — and it is the right answer where a part
  needs *structure* rather than a string. The obstacle is implementation, not
  design: most MDC innards are `staticHTML` strings with nowhere to attach a
  child. `topAppBar` had to be rebuilt from element oculars for this
  experiment, and that is the per-component cost of the general version. Good
  follow-up; `Slot` is the cheap 80%.
- **Row promotion** (a config field omitted statically reappears in the input
  row, so dynamic parts become model fields and the merges carry them). Most
  bambik-native, and it needs no new data type. Rejected for now: it puts view
  concerns (a caption) into the model row, the type-level machinery is heavier
  than the `ConvertOption` lifting, and the error messages land in the worst
  place. A `fed` projection gets the same reach without touching the model.

### Follow-ups, in value order

1. `imageListItem` as a component (`PUI Web { src, label } {}`), retiring
   photo-gallery's `dynamic` rebuild — the measured win.
2. Slots for `dialog`/`simpleDialog` `title` (naming the thing being confirmed
   is the most common real-world dynamic caption) and `tooltip` `text`.
3. Dynamic `options` for `select`/`radioButton`/`segmentedButton`/`tabBar`.
   Distinct problem: the option *set* is a collection, so this is the
   collection-action question (keyed reconciliation of options), not a slot.
4. A fourth slot case — `FedOr a (i -> a)`, a placeholder shown until the first
   feed — which would make the registration-time peek complete rather than
   holey. Cheap on top of `slotted`.
5. Slots for `iconButton`'s `icon`, retiring `iconToggle`.
