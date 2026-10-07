# Which word, when

A lookup index: from what the screen needs to the word, a demo that uses
it, and where to read. It states no rules — those are in
[writing.md](writing.md) — and no component contracts — those are in the
module headers. `×` is a record, `+` a variant; the other terms are defined
in writing.md *Terms*.

Where to read:

- **writing.md *Section*** — the rule.
- **a module name** — its header and per-word docs: `npx spago docs --open`
  in the app, or the source under `.spago/bambik/v0.1.6/src/`
  (`PUI.Web.MDC2` is `src/PUI/Web/MDC2.purs`).
- **a demo name** — `.spago/bambik/v0.1.6/demo/7guis/<name>-<ds>/` or
  `demo/nguis/<name>-<ds>/`, its view model module in the unsuffixed sibling
  directory.

Words not listed here — typography, surfaces, icons, the other selectors
and emitters of a catalogue — are in the design-system module header
(`PUI.Web.MDC2`, `PUI.Web.MDC3`, `PUI.Web.Shoelace`, `PUI.Web.Fluent`,
`PUI.Web.Bootstrap`, or `PUI.Web.HTML` for plain HTML).

## Composing

| You are writing | Word | Demo | Read |
| --- | --- | --- | --- |
| stages one after another | `Semigroupoid.do` (`import QualifiedDo.Semigroupoid as Semigroupoid`) | counter | writing.md *The pipeline* |
| displays and chrome reading one record side by side | `RecordToRecord.do` — `div $ RecordToRecord.do` | espresso-bar, loan-calculator, meeting-booker, shopping-cart | `Data.Profunctor.Row.RecordToRecord` |
| several emitters over one record | `RecordToVariant.do` | cashbox, stopwatch, crud | `Data.Profunctor.Row.RecordToVariant` |
| one stage per event case | `VariantToVariant.do` | crud, cashbox, reorder | `Data.Profunctor.Row.VariantToVariant` |
| one status per outcome case | `VariantToRecord.do` | flight-booker, order-form | `Data.Profunctor.Row.VariantToRecord` |
| what a composition guarantees | — | — | writing.md *What the laws guarantee* |
| a pane stays blank | — | — | writing.md *When it does not propagate* |

## App shape

| The app is | Word | Demo | Read |
| --- | --- | --- | --- |
| mounted | `body $ …`, imported from the design-system module | every demo | writing.md *App shape* |
| a model edited and folded | `# looped @( count :: Int ) # with freshCount`, the model row declared on the knot | counter | writing.md *App shape* |
| seeded, with no loop of its own | `# with @{ … } invitation` | potluck | writing.md *App shape* |
| a form section looping inside it | `# looped` | order-form | writing.md *App shape* |
| an action that retries until it succeeds | the retry inside the action's `Aff` (`chargeFlaky`), the outcome one case | payment | writing.md *Business functions* |
| a wizard's step, a running maximum, a counter | a model field, folded or normalized (`snackbar @"Next" steppedOnLine # fold stepTo`, `# settled raiseTop`, `snackbar @"Take a number" ticketTakenLine # fold issue`) | checkout, auction, ticket-dispenser | writing.md *Types and values* |

## Showing data

| The screen needs | Word | Demo | Read |
| --- | --- | --- | --- |
| a value, formatted | `headline4 (text countLine) # shown` | counter | writing.md *Components*; `PUI.Web` |
| a field verbatim | `text _.request.city`; `text _.entry` | weather; calculator | writing.md *Components* |
| a sentence composed from several fields | `( headline6 $ text balanceLine ) # shown` | cashbox | writing.md *Code style* → *Types and values* |
| a case label read as copy | `caseText` (`Data.Variant.Case`), in the view model module | order-form, potluck, espresso-bar | writing.md *Code style* → *Types and values* |
| a number as a bar, gauge or stars | `linearProgress @"Elapsed" elapsedFraction`; `progressBar @"Seats taken" seatOccupancy` | timer; meeting-booker | the design-system module |
| fixed copy | `(headline4 $ staticText @"Create account") # shown` | signup-form | writing.md *Components* |
| a record-reading group of displays | `# shown` | loan-calculator | writing.md *Stages* |
| a pane for one state of the model | `# shownWhen @"faulty" readout` | calculator, flight-booker, checkout | writing.md *Conditional visibility* |
| a list, displayed | `ul $ ( li $ text lapLine ) # shownEach @"number" lapRows` | stopwatch | writing.md *Collections* |
| a card that edits nothing | `card $ body1 (text summaryLine) # shown` | order-form, product-review | writing.md *Components* |
| a readout that settles before it redraws | `# debounced summarySettleTime` | order-form, flight-booker | `PUI` |
| a hint on hover | `# tooltip @"You must accept the terms of service to sign up"`; `# tooltipWith loyaltyNote` | signup-form; espresso-bar | the design-system module |
| a value-computed attribute | `attrWith "style" keyFace` | calculator, cells, circle-drawer | `PUI.Web` |
| a fixed attribute or class | `"style" := "…"`; `cl "dish"` | cells; restaurant-menu | `PUI.Web` |
| a class that depends on the value | `# clWhen isCompleted "todo-done"` | todo-list | `PUI.Web` |
| structure that varies with the value | `dynamic documentView`; `each items …`; `el ("h" <> show h.level)` | markdown-previewer | `PUI.Web` |
| an element with nothing in it | `static (span >>> cl "dish-dots")` | restaurant-menu, reorder | `PUI` |
| a leaf with no face | `blank` | circle-drawer, color-mixer | `PUI` |

## Editing

| The screen needs | Word | Demo | Read |
| --- | --- | --- | --- |
| a text field | `filledTextField @"First name" {}` | order-form | writing.md *Components* |
| a text field checked as the user pauses | `debouncedTextField @"Username" {} usernameSettleTime` | signup-form | the design-system module |
| a yes/no over a two-case field | `checkbox @"Terms" @"accepted" @"declined" {} (…)` | signup-form, espresso-bar | the design-system module |
| a switch | `toggleSwitch @"Takeaway cup" {}` | espresso-bar | the design-system module |
| a selection that always has a value | `select @"Flight type" {} (choice @"one-way" <+> choice @"return")` | flight-booker | writing.md *Components* |
| a selection owed but not yet made | `dropdownUnpicked @"Room" @"chosen" {} […]`; `segmentedButtonUnpicked @"Dish" @"chosen"` | meeting-booker; potluck | writing.md *Components* |
| a selection the user may leave unmade | `dropdownOptional @"Catering" @"ordered" @"none" {} […]` | meeting-booker | writing.md *Components* |
| a bounded quantity | `sliderLive @"Duration" {}`; `slider @"Split between" {}` | timer; tip-calculator | writing.md *Code style* → *Types and values* |
| two controls on one field | `slider @"Tip percentage" {}` then `rangeInput @"Tip percentage"` | tip-calculator | writing.md *Components* |
| a labelled group over a sub-record | `group @"Customer" $ Semigroupoid.do …` | order-form, potluck, reorder | writing.md *Components* |
| a reusable sub-form over a flat sub-row | `addressForm # subStrong` | parcel | `PUI` |
| an editor that exists in one state | `# inCase @"return" _."Flight type"` | flight-booker, meeting-booker | writing.md *Conditional visibility* |
| an invariant among edited fields | `# settled fromCelsius` | temperature-converter, meeting-booker | writing.md *Stages* |
| a variant field with an editor per case | `# bracketed @"Mode" fulfillmentState fulfillmentCase` | order-form | writing.md *Stages* |

## Events into state

| The screen needs | Word | Demo | Read |
| --- | --- | --- | --- |
| a button stepping the model | `button @"Count" {}` and `snackbar @"Count" countedLine # fold increment` | counter, todo-list | writing.md *Stages* |
| a button with a fixed payload | `button @"Take a deposit" { icon: "savings" } # with customerDeposit` | cashbox | writing.md *Stages* |
| an event folded into the model | `snackbar @"Cell claimed" cellClaimedLine # fold claimCell`, each fold opened by its status, the loop's folds merged in `VariantToRecord.do` | tic-tac-toe, cashbox | writing.md *Code style* → *Business functions* |
| a clicked element naming itself | `clicked @"claimed" _.key (…)` | tic-tac-toe, calculator, cells | `PUI.Web` |
| a click position on a canvas | `onClickedXY @"picked"` | circle-drawer | `PUI.Web` |
| an emitter shown in one state | `# provided @"confirming" _.deletion` | inbox, stopwatch, quiz | writing.md *Conditional visibility* |
| two buttons feeding one loop case | `button @"Next" {} # toCase @"next" goneOn` | checkout | `PUI` |
| an event case routed to its stage | `# atCase @"Create"` | crud, reorder | `PUI` |
| some cases intercepted, the rest passing | `( VariantToVariant.do … ) # subChoice` | cashbox | `PUI` |
| an event carrying something of its own | `listOf @"toggled" … # joined @"toggled"`, handled by `toggleTodo :: { event, model } -> model` | todo-list, cells, stopwatch | writing.md *Stages* |
| a button group fed the record it replays | `( RecordToVariant.do … ) # armed` | order-form, espresso-bar, signup-form | writing.md *Stages* |
| a menu of presets | `menu @"Presets" ( RecordToVariant.do menuItem @"The usual" {} … )` | espresso-bar, inbox | the design-system module |

## Effects, time, statuses

| The screen needs | Word | Demo | Read |
| --- | --- | --- | --- |
| an `Aff` action on an event, folded by its outcomes | `( Semigroupoid.do { indeterminateLinearProgress # action createPerson; VariantToRecord.do { … } } ) # atCase @"Create"` | crud | writing.md *Business functions* |
| … several `Aff`s with one outcome row | `VariantToVariant.do { indeterminateLinearProgress # action rotateAction # atCase @"Rotate"; … # atCase @"Shuffle" }`, the block's outputs one row | reorder | writing.md *Business functions* |
| … opened by its outcome statuses | `( VariantToRecord.do { snackbar @"Flight booked" bookedLine; snackbar @"Booking rejected" rejectedLine } ) # action @( … ) submit # atCase @"Book"` | flight-booker | writing.md *Business functions* |
| an action at load | `indeterminateLinearProgress # action loadOrder` then `snackbar @"Order loaded" orderLoadedLine # fold identity` | order-form, crud | writing.md *App shape* |
| an action's outcome cases | named by its `Aff` (`Aff [ "Person created" :: model, "Person not created" :: model ]`), each folded by its status | crud | writing.md *Business functions* |
| a periodic occurrence | `ticks @"Clock ticked" tickPeriod # replaying @"Clock ticked" identity` and `blankStatus @"Clock ticked" # fold tick` | timer, stopwatch, scoreboard | `PUI` |
| a status per outcome case | `snackbar @"booked" bookedLine` | flight-booker, order-form | writing.md *Components* |
| narrate an event while passing it on | `snackbar @"Charge card" chargingLine # observed` | payment, inbox | `PUI` |
| confirm before the flow continues | `confirmed @"Refund" @"Refund the customer?" $ …` | cashbox | writing.md *Modals* |
| a dialog of choices | `dialog @"Delete the last message?" $ RecordToVariant.do …` | inbox | writing.md *Modals* |
| an informational dialog | `simpleDialog @"Got it" @"About this dashboard" (…)` | weather | writing.md *Modals* |
| discard an assembly's output deliberately | `# muted` | scoreboard, order-dashboard | writing.md *Stages* |

## Collections

| What comes in → what goes out | Word | Demo | Read |
| --- | --- | --- | --- |
| the array → each element's event | `# foreach @"key" cells` | tic-tac-toe, cells, shopping-cart | writing.md *Collections* |
| the array → the array, decided jointly | `# acted @"name"` | potluck | writing.md *Collections* |
| the array → the array, edited in place | `# edited @"id"` | reorder | writing.md *Collections* |
| one `{ key, value }` at a time → tagged output | `# dispatched arrival` | departures | writing.md *Collections* |
| one `{ key, value }` at a time → the array | `# accumulated goal` | scoreboard | writing.md *Collections* |
| a selectable list (MDC2, MDC3) | `listOf @"opened" @"id" { selected: highlighted } _.messages (…)` | inbox | writing.md *Collections* |
| a selectable list elsewhere | `clicked @"picked" _.key (…) # foreach @"key" entries` | crud (html) | writing.md *Collections* |

## View model not written yet

| You need | Word | Read |
| --- | --- | --- |
| the type a missing function must have | a typed hole, `?countLine` | writing.md *Writing order* |
| the view model module's signatures, all at once | a typed hole for every imported value; the compiler's last message lists them, nothing unknown | writing.md *Writing order* |
| the model row | `# looped @( … ) # with seed` (counter, inbox); `# looped @( … )` after a load action folded in (crud, order-form) | writing.md *Types and values* |
| a derived row, where it is introduced | a classifier's first pane `# shownWhen @l @( … ) f` (checkout), `# action @( … ) f` (flight-booker), `# foreach @k @( … ) proj` (color-mixer), `# with @{ … } seed` (potluck) | writing.md *Types and values* |
| the view running before its view model exists | `hole` (`PUI.Web`) | writing.md *Writing order* |
| to know the app is finished | no hole left | writing.md *Writing order* |
