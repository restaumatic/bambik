# Writing a bambik application

The rules below govern the app modules the scaffold ships — the view
module (`src/<Module>.purs`) and the logic module beside it
(`src/<Module>Logic.purs`). They are the **definitive statement of
bambik application code style** — the [Code style](#code-style) section
at the end is the strict contract, and everything before it is the
vocabulary and the shapes that contract is written in. Nothing else
restates them; other documents point here.

The demos named throughout are worked examples in the fetched library,
under `.spago/bambik/<tag>/demo/7guis/` and `demo/nguis/` — read one
when a rule needs a shape. Their directories carry a vocabulary suffix
(`counter-mdc2`, `counter-mdc3`; the 7GUIs set exists in all six —
`-mdc2`/`-mdc3`/`-shoelace`/`-fluent`/`-bootstrap`/`-html`); the
siblings are view modules over the one logic module in the unsuffixed
sibling directory (`counter/CounterLogic.purs`), differing only in the
vocabulary import and the honest catalog mapping, so read whichever
matches the app's design system.

## The pipeline

The app is one profunctor pipeline, composed with `Category.do`
(data-flow stages: each stage's output is the next stage's input, so
code order is DOM order *and* data order) and the four qualified-do row
merges (operands over one shared row):

- `RecordToRecord.do` (×→×) — all-at-once **content merges**: chrome
  beside displays (a gated rung's structured content), and bare
  type-changing selectors beside the displays that read them (potluck's
  guest line). Editors are never its operands — an editor is a whole-row
  pipeline stage (see *Component citizenship*)
- `RecordToVariant.do` (×→+) — model in, events out: button rows
- `VariantToVariant.do` (+→+) — event dispatch: backend actions
- `VariantToRecord.do` (+→×) — events in, display out: status snackbars

The merges are imported from the row modules
(`Data.Profunctor.Row.RecordToRecord` and its three siblings), not from
`QualifiedDo`. The pipeline's sugar is `QualifiedDo.Category` — bambik's
complement of qualified-do, which stops at `Semigroupoid` — imported
`as Category` (`import QualifiedDo.Category as Category`), so the block
names the structure it composes in: a category whose unit is the wire,
`identity`. Neither `do` is a monad's.

**The one runtime rule.** A record merge — and every stage built on one
— emits only once every field of its row has been fed, then re-emits on
each change; until then it withholds, and nothing downstream renders.
Seeds (`mvu seed`, `with initial`, the trace forms' first argument) are
how a row becomes known at registration. When a pane stays blank, this
is why — *When it does not propagate* below has the watchdog that names
the starving gate.

## Component citizenship

Every component is a citizen of exactly one direction and **states its
business label once, as the leaf's own type argument** — no canonical
label (`value`/`clicked`/`event`) ever appears in application code, and
adopters that need a leaf's label derive it from the closed singleton
row.

**A label is the copy it draws.** Captions are never derived from an
identifier — the library has no humanizing step — so a labelled leaf
carries its words directly (`filledTextField @"First name" {}`,
`button @"Submit order" {}`), which means the label is usually a quoted
string, since human copy is no identifier. That reaches into the model:
a leaf's label *is* the field it edits, so the business rows carry the
same quoted labels (`{ "First name" :: String }`), and the one syntax
that a quoted label rules out is the **record pun** — write the explicit
pattern instead, which is the whole cost of copy living in the row:

```purescript
-- pun is unavailable on a quoted label; bind explicitly
createPerson { "Name": name, "Surname": surname, people } = …
```

Field access (`r."Name"`), accessor sections (`_."Name"`) and update
syntax (`r { "Name" = … }`) all work unchanged.

- **editors** (`filledTextField`, `checkbox`, `slider`, ...) take the
  business field directly: `filledTextField @"Email" {}`. The label is
  stamped on the host element as its `name` attribute and **is** the
  caption, verbatim — so a field label is written as the copy it draws,
  quoted whenever human copy is no identifier (`@"First name"`,
  `toggleSwitch @"Takeaway cup" {}`). The config survives only where the
  caption genuinely *cannot* be the label — localized wording, in
  practice. Everything else goes **on** the label, punctuation and units
  included: `filledTextField @"Start date (DD.MM.YYYY)" {}`,
  `sliderLive @"Amount (€)" {}`,
  `filledTextField @"Formula (e.g. =SUM(A0:A5)*2)" {}`. **No demo passes
  a caption config**, and a `label:` repeating what the label already
  says is the smell. **Selector options** obey it too, through `choice`:
  each case states its copy once and the `{ value, label }` echo goes
  away —

  ```purescript
  dropdown @"Room" {} [ choice @"Focus pod (4 seats)", choice @"Boardroom (12 seats)" ]
  ```

  `choice @l` is a plain value, so the options are an ordinary array and
  their order is the order written — **not** the variant row's, which the
  compiler sorts alphabetically, while option order is a design decision. Where a case needs a second, different rendering (a summary
  line saying "focus pod" where the option says "Focus pod (4 seats)"),
  that is an ordinary business function over the case — the case stays
  the identity.
  An editor is a **whole-row citizen** `p { l | rest } { l | rest }` — a
  complete `×→×` stage on its own: fed the wide row it edits field `l`,
  and every emission re-attaches the other fields from the background its
  `field @l` lift retains. A form is therefore editors written as
  successive pipeline stages — never `RecordToRecord.do` operands — and
  two controls deliberately writing **one** field are simply two such
  stages in a row (tip-calculator binds an MDC slider and a native range to
  one quantity: each echoes what it is fed, so the second shows what the
  first set, and the loop's re-broadcast keeps the first current — no
  merge, no last-writer rule). The retained background
  is only as fresh as the stage's last feed, so an editor ensemble lives
  inside a loop — `mvu`, `looped`, or `bracketed` — whose re-broadcast
  keeps every sibling current; a loop-free flow wraps its editor window
  in `# looped` (order-form's form section, fed by its load action).
- **Localization.** A label is the copy in the language the application
  is written in, and it is also the model's field name, so the two never
  drift apart: a localized product keeps its rows as written and passes
  the rendered copy through the caption config the vocabularies keep for
  exactly this — `floatingLabel:` on the MDC text fields and `select`,
  `label:` elsewhere — from its copy table, keyed by the label
  (`filledTextField @"First name" { floatingLabel: t "First name" }`).
  The honest gap: `choice @l` has no caption override, so an option's
  localized copy is not yet expressible; the mechanism arrives with the
  demo that needs it, not before.
- **copy is a function, not a field** (doc/research-copy-is-a-function.md):
  a display whose content *is* copy takes the **read function** and no
  label — `text progressLine`, `text _.title` — and that function lives
  in the logic module, from the fields it needs to the words on the
  screen. The function is named at the point of use, so the view line
  answers *where is this computed* by itself, and the screen's copy is
  unit-testable in `spago test`: one pure function, no browser
  (`progressLine { "Duration": …, elapsed: 3.0 } == "3.0s / 10.0s"`).
  A whole line is one function, glue included — a prefix, a unit
  suffix, the words between two values — never several leaves with
  `staticText` between them, and never a formatter bracket in the view.
  A display that renders a **number** takes a read function too —
  `progressBar @"Elapsed" elapsedFraction`,
  `linearProgress @"Progress" quizProgress` — because a fraction is
  *derived* (a ratio of source fields), and derivation is the same act
  as formatting: `fraction = elapsed / duration.current` is no more
  state than the sentence beside it. Its label survives as the
  **accessible name only** — a bar showing 42% must announce *what* is
  42% — so it is copy, like an editor's caption, never a field
  reference. A number the model genuinely *holds* as state is still
  read by the function (`_.rating`), which is where the distinction
  lands: state is in the row, renderings are functions of it. The read function's
  signature states its footprint as an exact closed row; the stage
  hosting the display (`shown`/`shownWhen`/`shownEach`) widens it to
  the fed row, so no call site coerces. For **context-pinned rows** (a
  collection element, a pane payload) nothing changes: the row carries
  the *source* fields the producing function built, and the read
  function selects and formats them (`text _.title`,
  `text lapLine`). A **`present<App>` normalization is not a
  presentation device**: `settled` maintains invariants among *edited*
  fields (temperature-converter's `°C`/`°F`, meeting-booker's
  `seatsInRoom`, order-form's `staleDistanceForgotten`), and a model
  field exists because the app's state needs it, never because a
  display wanted a `String` — or a `Number`. Across the demos every
  surviving `# settled` sits on an editor; not one feeds a display.
  A `text` read is a **row-stating position**: the display's footprint
  is checked against the fed row, so the read must state a closed row.
  Where a sibling stage already pins it (a `clWhen` beside the leaf, a
  `foreach`/`listOf` projection above it) a bare accessor infers and is
  what to write — `text _.title`; where nothing else pins it, a display
  whose copy *is* one field takes a named closed-row read
  (`titleLine :: { title :: String, … } -> String`), and that function's
  signature is the footprint declaration, not a wrapper to delete. The
  same rule governs mechanism arguments: a feed projection that merely
  reads a field is the accessor, except at row-stating positions.
- **event emitters** (`button`, `fab`, `iconButton`, `menuItem`) are
  label-indexed at their case, and the case label **is the caption**,
  verbatim: `button @"Submit order" {}` emits `[ "Submit order" :: _ ]`
  and draws those words. So an emitter never repeats itself in a
  `label:` config — the copy goes in the type argument, and the case is
  quoted at every mention (`atCase @"Submit order"`,
  `match { "Submit order": … }`). **No demo passes an emitter `label:`.**
  When a trace form's loop case would force two buttons to share one
  case under different words, that is a signal the buttons are two
  business actions: give each its own self-describing case and adopt it
  into the loop case with `# toCases`, so the fold still sees one case
  while each button reads as what it does (checkout's
  `button @"Next" {} # toCases goneOn`). `label:` is left for a
  glyph-only face (`fab { label: Nothing }`).
  `# toCases f` fires the business outcomes `f` computes from the
  payload — `f` returns a *variant* of results, which `toCases` emits
  directly, deriving the consumed case from the emitter's row.
- **statuses** (`snackbar`, `banner`) derive their own payload case;
  adopt with `# forCase @l copyOf` for one business case (sibling
  operands each own exactly their case), or `# forCases classifier`
  when one status instance serves several mutually exclusive outcomes —
  the classifier is a **record of per-case copy functions**
  (flight-booker's `bookingLine`), the elimination being the mechanism's
  own, so no `match` appears in the status story. `forCase @l f` is the
  derived single-case convenience — `forCases { l: f }` by law. A status
  mid-pipeline — showing events that must also flow on — wraps with
  `# observed` (payment's retry toast narrates the retry loop); the
  status may consume a narrower variant than the stage carries,
  background cases pass untouched.
- **type-changing selectors** (`select`, `radioButton`,
  `segmentedButton`) carry the business label through both rows
  (`select @"Milk" cfg opts :: { "Milk" :: Maybe _ } → { "Milk" :: _ }`);
  always-selected ones take `# required`, possibly-unselected ones
  `# optional @"chosen" @"unchosen"` (both derive the label; `optional`
  takes the two state names from the application) — the model keeps a
  named two-case variant, never a `Maybe`, seeded at the unmade case
  (`"Room": .unchosen {}`, no default pick), and the stages demanding
  the bare selection adopt the made case (`# inCase @"chosen" roomOf`,
  `# provided @"complete" plan`) until the user picks
  (meeting-booker is the no-defaults showcase). An editor whose text is *derived* from
  sibling fields is a model concern, not an adopter's: keep the derived
  texts as model fields and normalize them into each other with
  `settled` (temperature-converter holds both `@"°C"` and `@"°F"` texts,
  each field's stage running `# settled fromCelsius` /
  `# settled fromFahrenheit`, so a failed parse leaves the sibling
  untouched). A label is an arbitrary
  string, so where a **symbol** is the conventional caption — `°C`, a
  currency sign — write the symbol rather than spelling it out.

**Oculars** (`card`, `dialog`, `layoutGrid`, `topAppBar`, typography,
elevations, ...) are shape-preserving decorators — wrap freely; code
order = DOM order.

A card whose content is one model sub-record is not chrome but a
**labelled group** — `group @"Customer" $ …` (MDC2/MDC3) states the
surface, the heading and the `field @l` nesting in one word: the label
is the field the group nests, the heading copy verbatim, and the
accessible group name (`role="group"`). Because it draws the surface,
the group **leads its lines like any container** (`card $`,
`confirmed cfg $`) — never trailing as a `#` chain — so the `@l` anchor
sits at the head of the block it wraps. It is fused for the same reason
the leaves are — the label does work a trailing `# field @l` cannot
(heading copy, accessible name) — so hand-spelling the trio (`card`, a
`staticText` heading, `# field @l`) is the smell `group` deletes. A card
grouping no model (a display card, a button row) stays the blind `card`
with its heading as typography (order-form's Identifier and Total
cards); a flat sub-row focus stays `# subStrong` (parcel's address
form). The bare `field @l` itself is **design-system plumbing, not
application vocabulary** (not re-exported from `PUI`): every vocabulary
editor is `field @l`-lifted inside — the plain-HTML floor's
`input @"Name" "text"` included — and sub-model nesting is `group @l`,
so a nesting no mechanism fits is a missing-vocabulary signal, never a
reason to reach for the lens.

Component configs are anonymous records whose field names belong to the
vocabulary, not to a convention — `filledTextField`'s `floatingLabel`
and `button`'s `label` differ — so read the component's signature in the
vocabulary module, or copy a demo's call, instead of guessing; a guessed
label surfaces as a `TypesDoNotUnify` on the config record.

## Pass-through stages

**Choosing a display component is a business decision about assurance.**
Being read is always the business's concern; each display is a policy for
fulfilling it, and the type records the policy's evidence: `{}` output for
unwitnessed fulfillment (readouts, toasts), an emission for a witness (a
banner's dismiss, a dialog's confirm) — and the tap law permits discarding
only the unwitnessed. Escalate assurance by escalating the component
(readout → toast → banner → dialog), and route by assurance with
`subChoice` where outcomes differ in weight (cashbox: outgoing money
demands a dialog's witness, incoming posts straight to the fold). The
full ladder and its laws: doc/displays-and-sources.md.

Two wrappers make a stage pass-through, and they are not
interchangeable:

- An **editor is pass-through natively**: it echoes each fed row and
  completes each edit from its retained background, so it sits in a
  record pipeline with no wrapper at all.
- A display **is a pipeline stage natively**. Pick the rung whose
  fulfillment policy the business wants: `content # shown` for ambient
  structured content (chrome + unit displays, registered at build,
  released per feed), `# shownWhen @l classifier` for display panes
  (attached on relevance, released always), `# inCase @l classifier`
  for an **editor pane** — a
  whole-row editor that exists only in one mode, its own `field @l` lift
  carrying the rest of the row — `item # shownEach @l proj` for keyed
  collections, `confirmed cfg $ content` where the flow
  must wait for the user's confirmation. Content slots accept only
  `{}`-output components — an editor inside fails to unify; a genuinely
  emitting assembly is discarded **in writing** with `# muted`.

So: an editor is a stage as it stands; a display stage is the gated rung
that states its policy (`(…) # shown` for a structured line,
tip-calculator's money readouts — and for **pure chrome in a pipeline**:
a blind card's caption is `(subtitle1 $ staticText "…") # shown`,
registered at build, releasing every fed row; a card whose content is a
model sub-record needs no such line — its heading is the label of
`group @l`). The rung trails like
every data concern — the line leads with the visual content, the policy
rides at its end with `#`. A live readout as a
pipeline stage is just a display whose gate opens instantly.

A terminal **collection display** — a projection rendered as a list or
grid, passing the model through — is `item # shownEach @l rowsOf` inside
its container ocular: keyed, retained, releasing the fed row per feed
(so an empty array never starves). The rows projection must be a
**named projection with a closed row**, not an accessor: the gated
rungs read a *closed* narrow row by `Union` subsumption, so an open one
leaves no instance and the error lands on the rung. This is the
row-stating exception to the delete-the-one-field-projection rule.
Stopwatch's laps list is the worked example. Where a collection's
forwarding must be written off inside a unit display (a packaged
control's `foreach`, scoreboard's summary group), the discard is
written — `# foreach @l rowsOf # muted` — never silent.

A fixed catalogue drives `listOf`/`foreach` through the mechanism's own
projection argument (`# foreach @"key" (const keyPad)`) — never an
input-annotated feed. The same closed-row rule serves `clicked`: its
content subsumes, and a multi-reader content (a leaf plus `attrWith`
decorators) states its row once, in a named closed **face** function
(`attrWith "style" cellFace` with
`cellFace :: { text :: String, header :: Boolean, sel :: Boolean } -> String`
— the row-stating exception again). An element whose whole face is
decorators sits on `blank`, the faceless leaf.

## App shape

The shape of the pipeline follows the app, not a blessed template: a
pure self-feeding loop reads `# mvu seed`, a loop-free flow reads
`# with seed`, and the two combine freely.

Worked examples, by shape:

- **smallest MVU** — counter.
- **load-fed loop** — order-form (load action → `looped` form and
  summary → events → backend dispatch → statuses); it is also the
  four-direction showcase. The loop has no seed of its own — the load
  action feeds it — and it is what keeps every editor's retained
  background current.
- **both combined** — crud (a load action feeding a `looped` form whose
  commands dispatch through write actions).
- **channel-fed structure-from-data** — cells and circle-drawer (7guis),
  tic-tac-toe and calculator (nguis): a fixed grid or canvas fed as data
  through the retaining `foreach`, each cell built once and updated in
  place via `attrWith` (value-computed attribute) + `text`, emitting its
  key via `clicked` + `toCase @l _.key`. No `data-*`, no wholesale
  rebuild. `onClickedXY` is the container-level coordinate emitter for
  canvases.
- **collections** — todomvc (`listOf` click-to-toggle plus `clWhen`
  styling), shopping-cart (`dataTable`/`dataRow`/`dataCell` over
  `foreach`, catalogue fed by `listOf`'s projection argument), reorder
  (keyed reconciliation and the `edited` collection editor), potluck
  (`acted`, the gather gate as UX).
- **panes** — quiz (`provided` panes over multi-stage pipelines,
  both adopting cases of one `quizPhase` classifier).
- **effects and time** — password-generator (`button @l` →
  `action`/`atCase` → `updated`), stopwatch (`every` with
  pause-by-`Nothing`), color-mixer (`sliderLive` driving an `attrWith`
  swatch).
- **structure-from-value** — markdown-previewer: a recursive `PUI Web`
  tree built by `(dynamic …) # shown`, because the structure genuinely
  varies per block.
- **the floor and the plain-HTML end** — helloworld (bare minimum),
  restaurant-menu (no design system at all: element oculars +
  `staticText` merged as `{} → {}` chrome, data via `each`, look
  supplied by page CSS).
- **one focused combinator each** — auction (`feedback`), checkout
  (`folding`), payment (`iterate`), ticket-dispenser (`unfolding`),
  parcel (`subStrong`), cashbox (`subChoice`), departures
  (`dispatched`), scoreboard (`accumulated`).

## Conditional visibility

Conditional visibility is **case adoption**, never an in-UI predicate —
and never a `Maybe`. The vocabulary has exactly one visibility
primitive, `provided @l classifier` (its display rung
`shownWhen @l classifier`, its editor rung `inCase @l classifier`): the
argument is a business function classifying the situation into a
variant, and the pane exists while the variant sits at case `l`, fed
that case's payload.

When the model field is itself a payload-carrying variant, the pane
adopts it directly — `# provided @"serving" identity # atField
@"display"` (ticket-dispenser), or through a closed accessor at a
row-stating position (`# shownWhen @"Dine in" fulfillmentOf`;
`_.fulfillment` would leave the rung's row unsolved there). When the
state is *derived*, one classifier derives it: every case named, each
case carrying exactly the payload its pane displays — checkout's
`checkoutStep` (`cart { item }`, `shipping { address }`,
`payment { card }`), calculator's `readout` (`sound { entry }` /
`faulty {}`), scoreboard's `standing` (`led`/`unled`), quiz's
`quizPhase` (`asking`/`finished`), inbox's `readState`
(`unread`/`read`) and `messageView` (`reading`/`browsing`),
signup-form's two classifiers (which replaced five `Maybe`
projections). Two panes can then never both be on screen — which two
separate "should this be visible?" tests can always accidentally allow
— and each view line names the state it renders, not the business
condition behind it.

**A `Maybe` a pane depends on is a two-case state with unnamed cases.**
`if … then Just … else Nothing`, a `match` with `Nothing` on every other
case, the negation of a sibling pane's `Maybe`, a stored `Maybe` field
read out by a projection — each hides from the view line which state
the pane shows. Name the cases: order-form's distance is
`[ estimated :: { km, to }, unknown :: {} ]`, not `Maybe`, so
`staleDistanceForgotten` is a `match` and the summary pane is
`# shownWhen @"estimated" distanceOf`; a selector left unmade is
`# optional @"chosen" @"unchosen"`, so meeting-booker's panes are
`# shownWhen @"rated" ratedRoom` and `# provided @"complete" plan`
with no `Maybe` anywhere in the booking; checkout's wizard buttons
adopt `onward`/`back` off `onwardFrom`/`previousOf`. `Maybe` stays
below the UI — an `index`/`find` lookup, an `Aff` result — and a
classifier converts it at the boundary (inbox's `messageView` turns
`find`'s `Maybe` into `reading`/`browsing`). The one `Maybe` a demo row
still carries is potluck's `"Dish"`: the type-changing selector's *input
protocol* (`Cons l (Maybe a)`) used bare inside `acted`, because the
gather gate must wait for a genuine pick and `# optional`'s `unchosen`
echo would open it — the leaf protocol showing through, not view-model
state, and the allow-list of `scripts/check-view-model.mjs` names it.

A pane whose content only exists sometimes is exactly this — for
*displays*. An **editor** that exists only in one mode is not a payload
to fold back by hand: it is a whole-row citizen with gated existence,
`# inCase @l classifier` (`shownWhen`'s editor sibling), and its lens
already re-attaches the rest of the row — so
`# provided @l paneOf # updated setField` with an identity
`setField` is the smell this rung deletes. The mode-of-a-live-editor
case — a variant editor's per-selection panes — is exactly that inside
the `bracketed` loop: the selection component, then each pane
`# inCase @l selectionOf` (order-form's three fulfillment panes over one
`selection` classifier; flight-booker's return date
`# inCase @"return" tripType`; meeting-booker's attendees slider
`# inCase @"chosen" roomOf`, a bounded quantity living in the model
whose bounds the room dropdown re-scopes with `# settled seatsInRoom`).
What the edit *does to the rest of the row* is then a `settled`
normalization on the same stage when it is an invariant of the state —
meeting-booker's `seatsInRoom` (a room never holds more than its capacity),
circle-drawer's `resizeSelected` (the selected circle's radius is the
slider's diameter; `undo`/`redo` clear the selection, so the invariant holds
through them). An editor folded as an event with `updated` is the smell in
both cases. And because an editor echoes every fed row, `settled f` runs
on every loop turn, not only on the edit — so `f` must be **idempotent**, a
statement true of every model value, never a reaction to the edit.
Order-form's distance estimate shows the difference: "forget the
estimate when the address is edited" would wipe it on the next
re-broadcast; "an estimate belongs to the address it was made for"
(`staleDistanceForgotten`, the estimate carrying its `to` address) is an
invariant, and the edit drops the estimate as a consequence.

`clWhen` stays predicate-driven: it toggles a class (styling), not
visibility, and is deliberately last-element-only.

## Modals

`dialog`/`simpleDialog` open on feed and close on emission. Feed them
selectively (`# provided @l` off a model state, or behind an event case via
`atCase`), put the deciding emitters inside — their emission closes the
dialog and flows on — and keep echoing displays off the content's final
stage, since an echo would close the dialog on open. Cashbox is the
worked example.

`drawer`'s nav slot is live: nav and content are sibling stages over the
same types, so a selectable nav merges its selector with static chrome
in one `RecordToRecord.do`.

## Collections

Collection items may hold stateful stages (whole-row editors,
`updated`) — refs are per-instance.

`foreach @l` (keyed by the row's materialized identity field; `listOf`
index-keys internally) **retains** items: it reconciles *by key* —
matched keys re-fed in place, new built, absent removed, DOM reordered
only when the key sequence changed — so a channel-fed item keeps its DOM
and state across feeds. Fixed-key grids never rebuild, growing lists
append, and a reordered list moves each node with its key, so focus and
local state follow the item.

The closure builders (`dynamic`/`each`) rebuild per value,
since their content lives in the builder closure. Reach for them only
when an element's *structure* genuinely varies with the data (markdown
blocks); when only *values* change over a fixed structure, feed the
structure as data through `foreach` and compute per-element attributes
with `attrWith`. Durable state still belongs in the model, with
`listOf`'s click-replay folding it back.

A **collection editor** is `edited @l` — `foreach`'s editor form. The
key is a **label**, not a function, and the element editor's output row
**excludes** it: each emission's key is re-attached as the edit's return
address, so an element structurally cannot change its key.
It folds every element emission back into the array by key, emitting the
whole updated array immediately, input-primed. An element whose merge
covers less than the full row simply subsumes — `edited` reads the
element row narrow, so the id is never passed through and there is
no call-site widening. (The same rule at a linear pipeline's `×→+`
polarity flip is `# armed`: the emit stage reads the sub-row its
emitters replay.) The result is a first-class
`Array a → Array a` stage: nest it in a form under `group @l` (reorder's
`group @"Setlist" $ list …`, the group leading like any container) or
feed it straight to `# mvu`.

Rows need stable identity (an id field) — the key is both the
reconciliation identity and the return address of each edit, so an array
of bare strings cannot be edited in place. Add, remove and reorder are
array-level concerns: sibling `updated` stages over the enclosing model,
not part of the element. Reorder is the worked example (in-row rename
via `edited`, Rotate and Shuffle as sibling action stages).

## Separation of concerns

Organize the code (by inlining and extracting) until every function
belongs to exactly one of two classes:

1. **UI wiring** — lives inline in the entry function, or is unavoidably
   standalone like a UI-component-builder function for `dynamic`/`each`.
   Anything that mentions PUI types, variants-as-events, DOM wiring.
2. **Pure business** — standalone functions over the model and plain
   data: model transformers, formatters, parsers, evaluators, Aff
   actions. No PUI types, no UI vocabulary in their signatures
   (variant rows as *derived states* — a classifier's result — are
   business data; variants-as-events are UI).

### The two classes are two modules

The classes live in separate modules, and the dependency between them is
one-way:

- The **view module** (`<App>.purs`) exports the single entry function
  and keeps the UI-wiring functions that survive the one-liner rule
  (UI component builders, reusable sub-forms). It imports the design-system
  vocabulary, the library's combinators, and the logic module.
- The **logic module** (`<App>Logic.purs`) exports the business
  functions and the named business values (seed models, tick periods,
  default payloads). It depends only on the **domain** — `Prelude`,
  plain data (`Data.Array`, `Data.Maybe`, `Data.Variant`, ...), and the
  effect types business actions live in (`Effect`, `Aff`) — never
  `PUI`, `PUI.Web.*`, a design-system module, or the row merges. A
  business function that seems to need a UI component type is misdrawn: the
  UI component-shaped part is view. (Business optics — `Shutter`/`Reel` — stay
  the location-exempt algebra usable below the UI; see
  [Wiring](#wiring).)

The dependency arrow makes the design-system choice a view concern by
construction: **vocabulary siblings are view modules over the exact
same logic module**. In the demos, `counter-mdc2/CounterMDC2.purs`,
`counter-mdc3/CounterMDC3.purs` and the other four vocabulary siblings
all import `CounterLogic` from the unsuffixed sibling directory
`counter/`, and the siblings' diff is the vocabulary import plus the
honest catalog mapping, nothing else.
Anything that would differ between twins is presentation by definition
and belongs in the view module — a logic module that would vary with the
design system has presentation hiding in it.

An app whose business class is empty (helloworld) stays a single view
module; the logic module appears with the first business function.

**File order**: within the view module the one entry function comes
first, followed by the standalone UI-wiring functions; the logic module
holds the business functions over the model, seed first.

### What to inline (delete the named glue)

- **Update dispatchers.** A named handler that merely `match`es cases
  becomes an inline `match { … }` at the update stage, each case's body
  extracted as its own business function. Applied point-free, `match`
  curries correctly: `updated` wants `payload -> model -> model`, which
  is exactly what a `match` of such handlers gives. A `match` whose
  branches all discard their payload over an emitter fed the row it
  acts on is glue too: the stage is `# applied f`
  (`button @"Add" {} # applied addTodo`), the label stated once.
- **Event constructors.** A wrapper function that injects a payload into
  a case is unnecessary: a channel-fed cell replays its own value on
  click and `toCase @l` introduces the case, closing the row itself.
- **Named one-liner UI components.** A UI component function whose whole body is one
  pipeline expression — the named toast is the archetype
  (`submittedToast = snackbar # forCase @"orderSubmitted"
  submittedLine`) — is glue: inline the expression at its pipeline
  position and delete the function (see the Layout rule). The copy
  function's business name already says what shows.

### What to extract (name the business)

Each case lambda inside the old dispatcher becomes a standalone pure
function in the logic module, named for the business action, in the
Mealy step's own shape `payload -> state -> state` (see
[Code style](#business-functions)). Existing model-to-model functions already belong to
the business class — leave them standalone, in the logic module.

The model row is spelled once, at the seed and the merges; every
business helper states its own exact narrow footprint as a closed row,
never the whole model. Values that legitimately live in the logic
module — seed models, tick periods, default payloads — are named there
in business language.

### Type-inference gotchas (both hit in practice)

- **Introduce an output case with `toCase @l`, not an annotated lambda.**
  At a collection site the item's bare output becomes a business case
  through `toCase`, which closes the row itself — no inline variant
  sugar, no annotation, and the label shows up in tracing. A channel-fed
  cell and the container coordinate emitter `onClickedXY` both produce a
  **bare** payload, so both take `toCase @l` rather than an inline
  injection lambda.
- **Ignored button payloads still pin rows.** A button emission's
  payload row is inferred *from the handler*. A handler that discards
  the payload with a plain `const` leaves the row free, the whole merge
  becomes ambiguous, and the error surfaces at a sibling stage.
  Composing instead — applying the business function to the payload
  snapshot and `const`-ing the result — pins the row while staying
  point-free.

### Boundary cases

- UI-component-builder functions (for `dynamic`/`each`) are UI but too
  large to inline — they stay standalone in the view module, and that is
  fine: they are *purely* UI-related.
- Caption and validation formatters are pure business — keep them, in
  the logic module. Toast copy lines (`row -> String`) are business by
  the same signature test, and being design-system-blind they share
  across twins like everything else there.
- **A handler with a phantom payload parameter is a smell**: the
  payload it never reads is UI (the event) smuggled into an otherwise
  pure business function. Strip it — the business function is
  model-to-model — and absorb the event in the inline dispatch. Note a
  unlabeled button leaves its case ambiguous — the label is the leaf's type argument, so
  the dispatch is a one-case match that applies the business function to
  the payload snapshot, which also pins the button's row.

## Code style

The definitive contract for application code. Each rule is strict: code
that breaks one is wrong even if it compiles and behaves. They build on
the structural rules above — anonymous view-model types, a view module
over a logic module, a single exported entry function.

**The anchor invariant.** Every view line names exactly one semantic
anchor, and the anchor's kind says what the line is:

- a **field** — the `@l` on an editor, selector or labelled group: the
  label *is* the model field the line edits (`filledTextField @"First name" {}`,
  `dropdown @"Room" {} […]`, a sub-form's `group @"Customer" $ …`, the
  plain-HTML floor's `input @"Name" "text"`);
- a **case** — the `@l` on an emitter, pane or status adoption: the
  label *is* the business case the line emits or shows
  (`button @"Submit order" {}`, `# shownWhen @"estimated" distanceOf`,
  `# forCase @"registered" welcomeLine`);
- a **named read function or bare accessor** — a display's content,
  living in the logic module (`text balanceLine`, `text _.title`);
- **nothing** — chrome: statics and oculars write nothing, so they
  name nothing (`card`, `(subtitle1 $ staticText "…") # shown`).

The mapping is **line ↔ named symbol** — a field or case of the model,
or a function of the logic module — deliberately not line ↔ field: a
display's sentence is a function, never a field (*copy is a function,
not a field*); an ocular has no model, so a label on it would name
nothing (guardrail L3 keeps the vocabulary that way); and a trailing
mechanism spans fields under the line's one anchor while naming its own
business argument (`# settled seatsInRoom` riding the `@"Room"` line —
the mechanism-argument doctrine, [Wiring](#wiring)), so nothing on a
line is anonymous. Reading the view is then reading the model: an
editor line answers *which state*, an emitter or pane line *which
case*, a display line *where is this computed*, a chrome line *nothing
to trace*. The rules below — and the citizenship rules above — are this
invariant instantiated case by case, and the development loop it
induces — view first, logic module written to its names — is
[Writing order](#writing-order) below.

### Layout

- **Comments are deliberately absent** — code should read on its own.
- **Imports are 100% explicit (including `Prelude`)** — code is honest
  about its dependencies. Add and remove the names each change touches.
- **View and logic are separate modules, and the dependency is
  one-way.** The view module imports the logic module and the design
  system; the logic module imports only the domain — never `PUI`,
  `PUI.Web.*`, a design-system module, or the row merges. Design-system
  twins are two view modules importing the exact same logic module, so
  anything that differs between twins is view by definition.
- **One-liner `PUI Web`-returning functions are inlined.** A named
  UI component function whose whole body is a single pipeline expression is
  indirection: write the expression at its use site —
  `snackbar # forCase @"orderSubmitted" submittedLine` sits directly in
  the status merge — and delete the function with its annotation. The
  named business argument (`submittedLine`) carries the meaning, and its
  closed signature pins the row the annotation used to pin. A standalone
  UI component function earns its name only by genuinely spanning lines: a
  `dynamic`/`each` builder, or a reusable sub-form lifted as a
  citizen (parcel's `addressForm`).
- **Each UI-related line leads with the visual concern with `$` plumbing
  and trails with the data concern with `#` plumbing.** No data word
  ever leads a line — an emitter's replay payload trails like every
  other data concern (`button @l { … } # with patch`; `with` is
  output-polymorphic, so it seeds record pipelines and `×→+` emitters
  alike), and `# with {}` is written inline when the payload is the
  informationless unit, since naming `{}` is ceremony.
- **Closing parens and trailing `#` chains never start a line.** A
  trailing chain is written on one line (never one `#` per line) and
  rides at the end of the UI component's last content line — close the paren
  inline and continue. When a bracketed UI component nests, the enclosing
  levels' closers and chains cascade onto that same final line. The one
  exception is the app-level closer: the last UI line stays
  `) # mvu seed` / `) # with seed` on its own.
  **A cascading closer is spaced from the chain it closes over**, so each
  level reads as one `) # chain` unit rather than the paren fusing onto
  the previous level's last word:
  `… ) # settled commit ) # feedback noBids`, not `… ) # settled commit) # feedback noBids`.
  **Precedence caveat:** `#` (`infixl 1`) binds tighter than `$`
  (`infixr 0`), so where the chain must apply to the *whole element* —
  `foreach` multiplying an ocular-wrapped UI component — the paren must open
  *before* the ocular, never after its `$`, which would put the chain
  inside the element (one container around the collection instead of one
  per item). The `lcmap`-only adopter (`forProperty`) is safe either
  side of a shape-preserving ocular.
- **The architecture is readable off the types.** The application is a
  compass walk written as one pipeline — load → form (×→×) → live
  summary → events (×→+) → dispatch (+→+) → statuses (+→×) — closed by
  `mvu seed` / `with seed` to `PUI Web {} model`. If the top-level types
  do not tell that story, the structure is wrong, not the types.
  Indirection layers, UI component registries and config objects that assemble
  UIs reflectively are out.

### Types and values

- **No nominal types in UI.** A view-model type is one-off and specific
  to this UI, so it earns no name: no `data`, no `newtype`, no `type`
  synonym for anything a UI component displays, emits, or is configured with.
  **A view-model row consists of records, variants, primitives and
  `Array` — nothing else.** Anonymous record rows for all-at-once,
  anonymous variant rows for one-at-a-time, `{}` for unit payloads
  (never `Unit`), `String`/`Number`/`Int` at the leaves, and `Array` as
  the single container — the one recursion rows cannot express, which
  the collection algebra (`foreach`/`acted`/`edited`) is built on. No
  `Maybe`: it is `[ just :: a, nothing :: {} ]` with the cases unnamed,
  and every one of them has a name the business already uses
  (`selected :: [ picked { index }, none ]`, `opened :: [ message { id },
  none ]`, `approval :: [ approved { attempt }, pending ]`,
  `operation :: [ pending { key }, none ]`). No `Boolean` **unless a
  Boolean editor edits it** — a `checkbox`/`toggleSwitch`/`filterChip`/
  `iconToggle` over `"Decaf"` or `"Include a Teams link"` is honest; a
  flag nobody edits as a Boolean is a phase with two unnamed states
  (`status :: [ unread, read ]`, `[ active, completed ]`,
  `kind :: [ header, cell ]`, `drag :: [ adjusting, settled ]`,
  `line :: [ winning, plain ]`), and a styling test over it is a named
  predicate (`clWhen isCompleted "todo-done"`, `listOf { selected:
  highlighted }`), never a bare accessor. The rule is mechanically
  checkable (`scripts/check-view-model.mjs`): `:: Maybe` and `:: Boolean`
  may appear as a field in a logic module only on the allow-list of
  Boolean-editor labels (plus potluck's leaf-protocol exception). The
  library's own rows obey it too: a bounded quantity's `step` is
  `[ discrete :: Number, continuous :: {} ]`, and `checkbox @l @c @n`
  edits a two-case variant the application names. `Maybe` keeps its place
  *below* the UI — `index`/`find`, parsers, `Aff` results — and a
  classifier converts it at the boundary. Role names
  live on **values** (`mvu plannedTrip`, `with emptyCanvas`) and on
  business function names, never on types. Nominal types belong below
  the UI — a directly recursive type (a formula AST) or an ecosystem API
  (`Aff`, `Either`, `Milliseconds`) — and enter only as rows projected
  by business functions. The visible price is repetition: a row several
  business functions share is spelled out in each signature
  (flight-booker's itinerary variant, eight times), and it is paid
  knowingly — the shape *is* the interface, and a name would hide it
  from the reader who has to know it.
- **Business literals never hide in UI code.** A component parameter is
  presentation config iff the design system owns it; if the business
  owns it, it is model data riding the canonical row. Sliders and
  ratings edit a **bounded quantity** `{ current, min, max, step }`, so
  bounds arrive from the seed, may change while the app runs, and an
  editor cannot invent its own. What legitimately stays config or seed
  content — tick periods, default payloads, seed models — is a named
  top-level definition in **business language** (`smallestLoan`,
  `tickPeriod`, `gameStart`, `roomTemperature`), never lifecycle
  language: `initial`, `default` and `seed` are the smell's second form,
  as is naming the entry function `main`. UI code keeps only
  presentation — labels, captions, icons, styles, structure; layout
  numerics (a textarea's `rows`, a grid's `columns`) stay UI.
- **A composed line is one function.** A displayed line that
  concatenates any copy with any value — a prefix, a unit suffix, glue
  between two fields — is **one** named function in the logic module,
  read at the leaf, never a view-side merge of `staticText` pieces and
  display leaves:

  ```purescript
  headlineSmall (text balanceLine) # shown
  ```

  with `balanceLine :: { balance :: Number } -> String` beside the rest
  of the business logic. The copy around the value is part of the
  sentence the user reads, and the sentence is the testable unit:
  composing it in the view splits one assertion across a logic test and
  an untestable markup run. So `staticText` never appears in the same
  text run as a display leaf — it survives only for wholly static copy
  (a heading, a standalone note, a caption merge labelling an editor at
  the plain-HTML floor). Copy stays out of view code entirely except
  where a leaf's label *is* the copy.
- **A label is read back, never restated.** A case label *is* the copy
  it draws (`choice @l` states it once, at the case), so a `match`
  whose branches merely echo their case labels — verbatim or re-cased —
  is the label read in disguise: write the label as the exact copy the
  line needs, casing, prefixes and units included
  (`choice @"with oat milk"`, `choice @"less than a month"`,
  `choice @"cash"`), and read it back with `caseText` from
  `Data.Variant.Case` — a domain module, importable from logic and view
  alike — verbatim where the presentation field is derived
  (`present<App>` writing `dishText = caseText r."Dish"`) and inside
  copy lines (`caseText duration`). A map that does real work — shortening
  (meeting-booker's `roomText`), glyphs, per-case sentences — stays a
  named copy function; never keep one just to change case. A
  **case-invariant affix is not part of the copy**: it factors out of
  the labels, stated once — in the caption (`@"Duration (min)"` over
  `choice @"15"`/`@"30"`/`@"60"`; `@"Roast"`, `@"Plan"`,
  `@"Flight type"` likewise) or as line glue
  (`caseText roast <> " roast"`) — while an affix that varies per case
  stays in the labels (`"with whole milk"`/`"no milk"`, where the
  `with` disappears at `no`). The test is mechanical: if factoring the
  affix needs no conditional, factor it.
- **Business emissions carry bare data, never UI copy.** Toast and
  banner copy lives in named copy functions from the logic module,
  handed to the status adopter in place
  (`snackbar # forCase @"registered" welcomeLine`); the event carries
  the order, the outcome, the reason — the data, not the sentence.
  Validation results are payloads, not strings destined for a particular
  UI component.

### Business functions

- **Exact footprints.** Every business function states its footprint as
  a closed narrow row — what it reads ∪ writes, never the whole model.
  The reading stages (`updated`/the gated displays/`edited`/`acted`/
  `settled`) absorb the widening, so rows are read narrow while
  payloads stay exact; never coerce a row at the call site. A handler
  that reads nothing is not a transformer but a **constant patch**
  (`beginTiming :: { phase :: [ halted :: {}, timing :: {} ] }`, dispatched
  with `const (const beginTiming)`, or carried by the button itself:
  `button @"Reset" {} # with nothingElapsed # updated (match { "Reset": const })`).
- **One record of data per business function.** Several record
  parameters that travel together are one row in disguise — merge them
  and let the field labels name the roles that positional currying
  loses:

  ```purescript
  returnBetween :: { out :: Date, back :: Date } -> Maybe Itinerary
  returnBetween { out, back } = …            -- never  returnBetween out back
  ```

  A **fold handler is the one carve-out**, because its two records are
  not one row in disguise: the payload is an occurrence (`+`), the
  retained state is knowledge (`×`), and `updated`'s Mealy step keeps
  them apart — so a handler takes the step's own shape,
  `payload -> state -> state`, each record exact:

  ```purescript
  # updated (match { refunded: applyRefund })
  applyRefund :: { amount :: Number } -> { balance :: Number } -> { balance :: Number }
  applyRefund { amount } { balance } = { balance: balance - amount }
  ```

  The payload row is the case's exact payload — a collection element
  emitting a wider row narrows it at `toCase` with a named projection
  (movie-browser's `# toCase @"favored" favoriteMark`); the state row is
  what the handler writes, read from the model by subsumption. An
  emitter that carries **no payload of its own** — a button, `fab` or
  `menuItem` fed the row it acts on, replaying it on click — is not a
  Mealy step but a state transformer, and takes the rung that says so:
  `button @"Add" {} # applied addTodo` with `addTodo :: { … } -> { … }`,
  the case untouched and unread (counter's `# applied increment`,
  todomvc's `# applied clearCompleted`, inbox's
  `fab @"Compose" { icon: "edit" } # applied composeMessage`). Inside a
  `match`, `const <<< f` is that same transformer where several such
  emitters share one stage (circle-drawer's `"Undo": const <<< undo,
  "Redo": const <<< redo`). The remaining degenerate shapes are spelled
  with `const`: state-only `const f` (`const recordLap`),
  replace-with-payload `const` (`"Reset": const`), neither
  `const (const patch)`. Scalar and `Array` payloads (a key, an
  operator symbol, a fetched list) take the same shape positionally;
  they are not rows.
- **A handler carries no field it does not touch.** Its row is exactly
  its reads ∪ writes — a field that only rides through is a smell — and
  every field of a `match`'s shared row is written by *some* branch.
  When a branch would carry fields only its siblings touch, pick by
  separability: **separable emitters** group into stages by patch row
  (undo/redo split from the canvas click, so they shed the field only
  the click reads); **inseparable branches** — one dialog's two
  outcomes, one backend stream — keep the shared row, the carried field
  being a sibling's write; **disjoint footprints** mean the events or
  the model want redesign until the branches genuinely share. An
  **identity handler** is the smell at its purest: the event was never
  model data, so the honest wiring is a gated display stage, a display
  interaction, not an `updated` fold. Bounded quantities ride whole even
  where a handler replaces only `current`.
- **Lossy conversions live in the model, not in leaf brackets.** An
  editor's bracket must round-trip; a lossy normalization is `settled`
  on the whole-row stage, where the loop makes it a transaction — never
  hidden inside a component's `dimap` (cells'
  `filledTextField @"Formula …" {} # settled commit`, and
  temperature-converter's two text fields normalizing each other).

### Wiring

- **Application code never imports `Data.Profunctor`.** Speak the
  vocabulary: the adopters, the merges' qualified-do, and the mechanisms
  with their projection arguments — `provided @l classifierOf`, `foreach @l
  rowsOf`, `listOf opts rowsOf`, `dispatched envelopeOf`,
  `toCase @l payloadOf`, `forCase @l copyOf`,
  `toCases outcomeOf`, `forCases lineOf`, `forProperty`,
  `settled normalize`, `bracketed stateOf caseOf`, with `identity`
  saying verbatim. Every raw `lcmap`/`rmap`/`dimap` an application would
  write has one of those homes. A shape none of them fit is a
  missing-vocabulary signal to report — never a reason to import the
  module one floor down. Business optics (`Shutter`/`Reel`) in business
  code *below* the UI are exempt by location.
- **Visibility is business logic.** Conditional visibility is case
  adoption — `provided @l`/`shownWhen @l`/`inCase @l` over a stored
  variant field or a classifier that derives one — never an in-UI
  predicate, never a predicate hidden in a projection, and never a
  `Maybe`: a state a pane depends on is a variant with every case named,
  so exclusivity holds by construction and the view line names the state
  it shows. Where the model field itself is the variant **and the context
  pins the row**, the bare accessor says it (`# provided @"serving"
  identity # atField @"display"`); at row-stating positions a closed
  accessor or classifier stays, its signature being the footprint
  declaration. `clWhen` stays predicate-driven — it toggles styling, not
  existence.
- **State lives in the model or in the algebra's loops. Nowhere else.**
  No FFI stashes, no module-level `Ref`s, no reading the DOM back as
  state, no window globals. The model under `mvu` holds the entity; a
  UI component's private state is a residual threaded by the trace forms.
- **Lean on the design system's defaults; write no custom chrome.**
  Reach for a stock component and its built-in look before any style
  attribute. Surfaces (`card`, `elevation*`), typography, lists, grids
  and the components' own spacing already carry the design language, so
  a flex or border wrapper is a smell: drop the presentational `div` and
  let the components flow inline (buttons and fields are `inline-flex`,
  a `listOf` already scrolls, block typography stacks). The minimal look
  is the intended one. Custom styling is a last resort for genuinely
  data-driven graphics — an SVG canvas, a colour swatch — never for
  layout the design system already gives you. Every avoided style string
  is code you don't write.

## Writing order

The anchor invariant makes view-first the natural order: every view
line names its obligations, so the view is written first and the logic
module is written to its names. With the watch build running
(`npm run watch`, [building.md](building.md)) the loop is:

1. **Write the view as the compass walk**, each line naming its
   anchors. Field and case anchors (`@l`) never fail — they are
   type-level symbols that *define* the row as you write it. Every
   term-level obligation — a read function, a handler, a classifier,
   an action, the seed — enters as a typed hole (`text ?countLine`,
   `# mvu ?start`) for the compiler to speak first.
2. **Let the compiler type the pinned obligations.** A hole whose row
   is pinned from outside reports its full inferred type, with
   substitution suggestions. The **seed** is the showcase:
   `# mvu ?start` reports the model row accumulated from every anchor
   written so far — the view computes the model, and the hole spells
   it out — and suggests any in-scope value of that row. Every
   **exact-payload position** reports too:
   `snackbar # forCase @"Book" ?line` comes back as
   `{ name :: String } -> String`, suggesting `_.name`.
3. **Decide the subsumed footprints yourself; the signature is the
   decision.** At every subsuming position — a display read under
   `shown`/`shownWhen`/`shownEach`, a handler under
   `updated`/`applied`, a classifier, a `settled` normalizer — the
   sub-row is deliberately the function's own statement, so the
   compiler cannot fill the hole: the stage reports an ambiguous
   `Union`, and the hole reports beside it (compiler
   `0.15.16-variant.7` and later), its type wrapped in the unsolved
   constraint it shares unknowns with:

   ```
   Hole 'increment' has the inferred type

     Union t0 t1
       ( count :: Int
       | t2
       )
      => Record t0 -> Record t0
   ```

   Read it as: your function is `Record t0 -> Record t0` for a
   sub-row `t0` of the fed row `( count :: Int | t2 )` — *which*
   fields is a business decision the compiler refuses to make. State
   it: write the function in the logic module under its closed-row
   signature and both messages dissolve together.
4. **Work one declaration at a time.** Module checking stops at the
   first failing declaration, so holes in a later top level wait
   their turn — but a bambik app is one pipeline in one declaration,
   and within it every hole reports together with the ambiguities:
   the app's whole obligation list is one compile away. Fill the
   footprint decisions as they arise, the seed hole last — it then
   reports the finished model row, and one business-named value
   (`freshCount`, `plannedTrip`) closes the app. The *Type-inference
   gotchas* above name two more places an ambiguity surfaces away
   from its own line.
5. **Compile-green is not done.** The knowledge gates are invisible
   to the compiler — finish by running it (below).

A vocabulary twin inverts the loop: its logic module already exists
verbatim, so the view is written against known signatures and holes
are rarely needed.

## When it does not propagate

The compiler proves the wiring; it does not prove data reaches the
screen. A blank pane or a stale readout is almost always a **knowledge
gate withholding**: a merge emits only once every operand has spoken, so
one unfed sibling silences the whole record.

Two aids diagnose this in the browser, and reading them is part of
writing the app, not an afterthought:

- **The starvation watchdog** is on by default. A gate that withholds
  and is never fed within 3s prints one `console.warn` naming the gate,
  the exact missing fields, and the fix — `seeded`/`announce`, or the
  seed argument of `feedback`/`folding`/`unfolding`. An unprimed gate is
  therefore a named failure, not a blank screen. Opt out with
  `window.__bambikNoWarn = true`.
- **The emission trace** is `window.__bambikTrace = true`, also settable
  with `localStorage.setItem("bambik-trace", "true")`. It logs every
  propagation decision — stage-to-stage flow, `looped` re-feeds and
  swallowed echoes, and gate-withheld emissions with the sibling fields
  they wait for, the otherwise-invisible ones — as `console.debug`, so
  enable the Verbose log level in DevTools. The labels the trace prints
  are the ones adoption introduced (`toCase @l`, the emitter's own `@l`), which is
  the practical reason to name cases rather than inject them inline.

An unprimed *entry* needs neither: `body` demands input `{}`, so a
forgotten seed is a compile error at the mount point naming the
unsupplied fields. Supplying it is what `with initial` / `mvu seed` do.

## Finish by running it

A module that compiles is not a delivered change. Every piece of writing
ends the same way as bootstrapping does: with the app **running in dev
mode** — `npm run watch` and `npm run dev` in the background, the page at
`http://127.0.0.1:8000/` open and exercised, no console warnings from the
starvation watchdog — and the URL handed back to the developer. The
knowledge gates above are the reason: their withholding is invisible to
the compiler and obvious on screen. See [building.md](building.md).

## Reference

The API and its semantics are documented in the source module headers —
read them, not a summary. Paths are inside the fetched library,
`.spago/bambik/<tag>/`:

- `src/PUI.purs` — the core type, pipeline semantics, and the
  combinators: `mvu`/`with`/`looped`/`updated`/`applied`/`settled`/`action`,
  the adopter family re-exports (`atCase` among them), and the collection
  combinators `foreach @l`/`edited @l`/`acted @l`/`dispatched`/
  `accumulated`. The gated display family lives in `PUI.Web.HTML`
  (`shown`/`shownWhen`/`shownEach`) and the
  design systems (`confirmed`).
- `src/PUI/Web/HTML.purs` — HTML vocabulary, `body`, element oculars,
  `attrWith` for channel-fed structure-from-data, the builders
  `dynamic`/`each` for structure-from-value, and the
  `clicked`/`onClickedXY` events. SVG oculars are in
  `src/PUI/Web/SVG.purs`.
- `src/PUI/Web/MDC2.purs` — the MDC2 component and ocular catalog, plus
  the editors' `dimap` round-trip contract. The sibling design systems
  sit beside it (`MDC3.purs`, `Shoelace.purs`, `Fluent.purs`,
  `Bootstrap.purs`) — same two-sorted vocabulary, switch by switching
  the import.
- `extras/row-profunctor/Data/Profunctor/Row/` — the four merges, the
  adopters, the trace forms and the business optics; laws in the module
  headers. (The library keeps its carrier-agnostic algebra outside `src/`
  under `extras/`, which is why the app's `spago.dhall` carries a second
  source glob — see bootstrap.md.)
- [vocabulary.md](vocabulary.md) — the situation-indexed index into this
  file and the headers: what the screen needs → the word → where its rule
  is stated. [walkthrough.md](walkthrough.md) reads one mid-size demo
  (flight-booker) line by line.
