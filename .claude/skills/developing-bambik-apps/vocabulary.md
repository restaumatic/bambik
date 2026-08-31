# Which word, when

An index, not a rulebook. Every row points at the section of
[writing.md](writing.md) or the module header that states the rule; nothing
here says anything those do not. Read writing.md once; come back here when you
know what the screen needs and not yet what the word for it is. Module paths
are inside the fetched library, `.spago/bambik/<tag>/` — `HTML.purs` is
`src/PUI/Web/HTML.purs`, `PUI.purs` is `src/PUI.purs`, and the four direction
modules are `extras/row-profunctor/Data/Profunctor/Row/*.purs`.

## The two kinds of `do` — neither is a monad's

| You are writing | Block | What flows | Demo |
| --- | --- | --- | --- |
| stages in sequence — an editor after an editor, a display after a form, a fold after a button | `Category.do` (`import QualifiedDo.Category as Category`) | each stage's output is the next one's input; code order = DOM order = data order | every demo — start with counter |
| chrome and displays reading **one record** together | `RecordToRecord.do` (×→×) | the record broadcast to every operand; `staticText`/`text @l` only — never an editor | checkout's step lines |
| several buttons over one record | `RecordToVariant.do` (×→+) | record in, one case out per emitter | cashbox |
| one stage per event case | `VariantToVariant.do` (+→+) | each case to its own stage (backend actions) | order-form's dispatch |
| one status per outcome | `VariantToRecord.do` (+→×) | cases in, statuses out | order-form's snackbars |

Rule of thumb: things that *follow* each other → `Category.do`; things that
*share one value* → a merge, named by the shape of what goes in and out
(record `×`, variant `+`). Stated in: writing.md *The pipeline*; the four
direction module headers.

## Showing data

| The screen needs | Write | Demo | Stated in |
| --- | --- | --- | --- |
| one field, formatted | a derived presentation field (`present<App>` in logic, `# settled present<App>` trailing the pipeline), shown verbatim: `text @"countText"`, the block `# shown` | counter: `headline4 (text @"countText") # shown` | writing.md *displays are verbatim*; doc/research-presentation-model.md |
| a sentence, or text mixed with typography and several fields | a derived sentence field, shown verbatim inside an ocular over `RecordToRecord.do` of `staticText` chrome and `text @l` | order-form's summary | writing.md *Composed lines split at the field boundary* |
| pure chrome inside a pipeline (a card's caption) | `(subtitle1 $ staticText "…") # shown` | order-form | writing.md *Pass-through stages* |
| content that exists only in one state | `content # shownWhen @l classifierOf` — the one visibility primitive: one classifier names every state, each case carrying its pane's payload; a projection never decides, and no pane is gated on a `Maybe` | flight-booker's three `bookingState` panes; checkout: `# shownWhen @"placed" orderStatus`; calculator: `# shownWhen @"faulty" readout` | writing.md *Conditional visibility* |
| an **editor** that exists in one mode | `editor # inCase @l classifier` | flight-booker's return date; meeting-booker's slider | writing.md *Conditional visibility* |
| a list, displayed | `item # shownEach @l rowsOf` inside its container ocular | stopwatch's laps | writing.md *Pass-through stages*, *Collections* |
| a display inside a collection item, reading the item's own field | `text @l # forProperty` | todomvc's title; cells | RecordToRecord.purs (`forProperty`) |
| a display fed a whole value, not a record | a derived presentation field on that row, read verbatim (`text @l`, `# forProperty` when the row is wider) | inbox: `text @"unreadText"` | writing.md *displays are verbatim* |
| a live readout that should settle before it redraws | `stage # debounced { ms }` | flight-booker's itinerary line | PUI.purs (`debounced`) |
| the flow must wait for the user's confirmation | `confirmed cfg $ content` (MDC2/MDC3) — the modal leads like any container | cashbox | writing.md *Modals* |
| a value-computed attribute (style, coordinates, colour) | `attrWith "style" f` on the element | calculator, cells, color-mixer | HTML.purs (`attrWith`) |
| a class that depends on the value | `# clWhen predicate "class"` | todomvc | HTML.purs (`clWhen`) |
| structure that genuinely varies with the value | the `dynamic` / `each` builders | markdown-previewer | HTML.purs; writing.md *Collections* |

## Editing

| The screen needs | Write | Demo | Stated in |
| --- | --- | --- | --- |
| a field of the model, edited | the leaf with the field as its label: `filledTextField @"First name" {}`, `checkbox @l {}`, `slider @l {}` | every form | writing.md *Component citizenship* |
| a group of fields editing a sub-record | `( Category.do … ) # field @"customer"`; a reusable sub-form `# subStrong` | order-form; parcel | RecordToRecord.purs (`field`, `subStrong`) |
| an invariant between fields — editing one implies the other | `editor # settled normalize` | temperature-converter; meeting-booker's `seatsInRoom` | PUI.purs (`settled`); writing.md *Conditional visibility* |
| a selection that always has a value | `select @l {} [ choice @"…", … ] # required` | flight-booker | RecordToRecord.purs (`required`) |
| a selection that may still be unmade | `dropdown @l {} […] # optional @"chosen" @"unchosen"` — the field is a named two-case variant, seeded `.unchosen {}`; consumers adopt the made case | meeting-booker | PUI.purs (`optional`) |
| a bounded quantity | the model holds `{ current, min, max, step }`; `sliderLive @l {}` edits it | timer, circle-drawer | writing.md *Code style → Types and values* |
| two controls editing **one** field | two successive stages over it, `slider @l {}` then `rangeInput @l` | tip-calculator | writing.md *Component citizenship* |
| a variant with an editor per case | `( Category.do selector; pane # inCase @l selection; … ) # bracketed stateOf caseOf` | order-form's fulfillment | writing.md *Component citizenship*; VariantToVariant.purs (`bracketed`) |

## Events into state

| The screen needs | Write | Demo | Stated in |
| --- | --- | --- | --- |
| a button that changes the model | `button @"Count" {} # applied increment` — `increment :: state -> state`, the click's payload unread | counter; todomvc's Add | PUI.purs (`applied`) |
| an event whose payload the model folds in | `… # toCase @"picked" _.key # updated (match { picked: handler })` | quiz, tic-tac-toe | PUI.purs (`updated`) |
| the handler's shape | `payload -> state -> state`, both records exact | cashbox: `applyRefund :: { amount } -> { balance } -> { balance }` | writing.md *Code style → Business functions* |
| … several payload-less buttons sharing one stage | `const <<< f` per branch | circle-drawer: `"Undo": const <<< undo, "Redo": const <<< redo` | same |
| … the payload replaces the state | `const` | timer's Reset | same |
| … the payload is ignored | `const f` | stopwatch: `const recordLap` | same |
| … a constant patch | `const (const patch)`, or carried on the button: `button @l {} # with patch` and `const` | checkout; cashbox | same |
| a clicked collection element naming itself | `… # toCase @"picked" _.key` (whole payload: `identity`) | todomvc, cells | RecordToVariant.purs (`toCase`) |
| a button whose *outcome* the business computes | `button @l {} # toCases outcomeOf` | checkout's Next/Back; signup-form | RecordToVariant.purs (`toCases`) |
| one event case routed to its own stage | `stage # atCase @l` inside `VariantToVariant.do` | order-form; reorder | VariantToVariant.purs (`atCase`) |
| some event cases intercepted, the rest passing straight | `( VariantToVariant.do … ) # subChoice` | cashbox | VariantToVariant.purs (`subChoice`) |
| a whole button group made an emit stage | `( RecordToVariant.do … ) # armed` | order-form | RecordToVariant.purs (`armed`) |

## Effects and time

| The screen needs | Write | Demo | Stated in |
| --- | --- | --- | --- |
| an `Aff` action on an event, with a busy indicator | `indeterminateLinearProgress @"busy" # action (match { "Book": submit })`; `blank # action …` where the vocabulary has no indicator | flight-booker; reorder; order-form's distance estimate (inside a `looped` form the button is the occurrence — an action fed by the broadcast would re-run on every turn) | PUI.purs (`action`) |
| an action at load, before any input | `indeterminateLinearProgress @"busy" # action loadOrder` as the first stage, the app closed with `# with {}` | order-form | writing.md *App shape* |
| a periodic step | `every tickPeriod tick` as a stage | stopwatch, scoreboard | PUI.purs (`every`) |
| narrate an event as it passes, without consuming it | `status # observed` | payment's retry toast | PUI.purs (`observed`) |

## Statuses — events shown

| The screen needs | Write | Demo | Stated in |
| --- | --- | --- | --- |
| one status line per outcome case | `snackbar # forCases (match { orderSubmitted: submittedLine })` in `VariantToRecord.do` — the subset match keeps sibling operands' cases theirs | order-form | VariantToRecord.purs (`forCases`) |
| one status for a whole classified variant | `snackbar # forCases bookingLine` | flight-booker | VariantToRecord.purs (`forCases`) |

## Collections

| What comes in → what goes out | Write | Demo | Stated in |
| --- | --- | --- | --- |
| the whole array → each element's own event | `item # foreach @"id" rowsOf` (keyed by a model field) | cells, tic-tac-toe, crud (plain HTML) | PUI.purs (`foreach`); writing.md *Collections* |
| the whole array → the whole array, decided jointly (withheld until every element spoke) | `item # acted @"name"` | potluck | PUI.purs (`acted`) |
| the whole array → the whole array, edited in place | `editor # edited @"id"` | reorder | PUI.purs (`edited`) |
| one `{ key, value }` at a time → tagged per-element output | `item # dispatched envelopeOf` | departures | PUI.purs (`dispatched`) |
| one `{ key, value }` at a time → the growing array | `item # accumulated envelopeOf` | scoreboard | PUI.purs (`accumulated`) |
| a selectable list (MDC2) | `listOf { selected: _.done } rowsOf item # toCase @l _.key` | todomvc, crud | MDC2.purs (`listOf`) |
| a collection display that passes the model through | `item # shownEach @l rowsOf` | stopwatch | HTML.purs (`shownEach`) |

## App shapes

| The app is | Write | Demo | Stated in |
| --- | --- | --- | --- |
| a model, edited and folded, redrawn on every change | `pipeline # mvu seed` | counter and most demos | writing.md *App shape* |
| a pipeline with no loop of its own, seeded | `pipeline # with initial`; a form section inside it `# looped` | order-form; restaurant-menu | writing.md *App shape* |
| a wizard whose step state loops silently | `# folding @"next" stepSeed` | checkout | RecordToVariant.purs (`folding`) |
| a state field that loops output → input, invisible outside | `# feedback stateSeed` | auction | RecordToRecord.purs (`feedback`) |
| an event that retries itself | `# iterate` | payment | VariantToVariant.purs (`iterate`) |
| a counter that resumes where it left off | `# unfolding @"resume" seed` | ticket-dispenser | VariantToRecord.purs (`unfolding`) |

## Where `identity` still appears

In a projection slot `identity` means "the whole value, verbatim": `toCase @l
identity` (the emitter's whole payload is the case payload — cashbox),
`foreach @l identity` (the fed value *is* the array — potluck). `shown` and
`forProperty` take no projection, so it never appears with them. Stated in:
writing.md *Code style → Wiring*.

## The one runtime rule

A record merge, and every stage built on one, emits only once every field of
its row has been fed; until then it withholds and nothing downstream renders.
Seeds (`mvu seed`, `with initial`, the trace forms' first argument) are how a
row becomes known at registration. A pane that stays blank is a gate
withholding — the 3s watchdog prints the gate and its missing fields to the
console. Stated in: writing.md *The pipeline* and *When it does not
propagate*.
