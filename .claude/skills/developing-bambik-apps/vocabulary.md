# Which word, when

A lookup index: from what the screen needs to the word, a demo that uses
it, and where to read. It states no rules — those are in
[writing.md](writing.md) — and no component contracts — those are in the
module headers. `×` is a record, `+` a variant; the other terms are defined
in writing.md *Terms*.

Where to read:

- **writing.md *Section*** — the rule.
- **a module name** — its header and per-word docs: `npx spago docs --open`
  in the app, or the source under `.spago/bambik/<tag>/src/`
  (`PUI.Web.MDC2` is `src/PUI/Web/MDC2.purs`).
- **a demo name** — `.spago/bambik/<tag>/demo/7guis/<name>/` or
  `demo/nguis/<name>/`, every design system's view and the view model in
  one directory.

Words not listed here — typography, surfaces, icons, the other selectors
and emitters of a catalogue — are in the design-system module header
(`PUI.Web.MDC2`, `PUI.Web.MDC3`, `PUI.Web.Shoelace`, `PUI.Web.Fluent`,
`PUI.Web.Bootstrap`, or `PUI.Web.HTML` for plain HTML).

## Composing

| You are writing | Word | Demo | Read |
| --- | --- | --- | --- |
| stages one after another | `Semigroupoid.do` (`import QualifiedDo.Semigroupoid as Semigroupoid`) | counter | writing.md *The pipeline* |
| a display among stages | `headline4 $ text countLine` on its own line, where it sits on the screen | counter, tip-calculator, stopwatch | writing.md *The pipeline* |
| displays and chrome one after another in an element | `div $ Semigroupoid.do …` | espresso-bar, loan-calculator, meeting-booker, shopping-cart | writing.md *The pipeline* |
| several emitters over one record | `RecordToVariant.do` | cashbox, stopwatch, crud | `Data.Profunctor.Row.RecordToVariant` |
| a stage on one event case | `( … ) # atCase @"Create"`, the stage owning the case in the fold block | crud, cashbox, reorder | writing.md *Stages* |
| one status per outcome case | `VariantToRecord.do` | flight-booker, order-form | `Data.Profunctor.Row.VariantToRecord` |
| what a composition guarantees | — | — | writing.md *What the laws guarantee* |
| a compile error between two rows | — | — | writing.md *When it does not compile* |
| a pane stays blank | — | — | writing.md *When it does not propagate* |

## App shape

| The app is | Word | Demo | Read |
| --- | --- | --- | --- |
| mounted | `body $ …`, imported from the design-system module | every demo | writing.md *App shape* |
| a model edited and folded | `# looped @( counted :: Int ) # with freshCount`, the model row declared on the knot | counter | writing.md *App shape* |
| seeded, with no loop of its own | `# with @{ … } invitation` | potluck | writing.md *App shape* |
| a form section looping inside it | `# looped` | order-form | writing.md *App shape* |
| an action that retries until it succeeds | the retry inside the action's `Aff` (`chargeFlaky`), the outcome one case | payment | writing.md *Business functions* |
| a wizard's step, a running maximum, a counter | a model field, folded or normalized (`fold @"Next" stepOnward`, `# settled raiseTop`, `fold @"Take a number" issue`) | checkout, auction, ticket-dispenser | writing.md *Types and values* |

## Showing data

| The screen needs | Word | Demo | Read |
| --- | --- | --- | --- |
| a value, formatted | `headline4 $ text countLine` | counter | writing.md *Components*; `PUI.Web` |
| a field as it is stored | `textAt @"title"`; `textAt @"entry"` | todo-list; calculator | writing.md *Code style* |
| a sentence composed from several fields | `headline6 $ text balanceLine` | cashbox | writing.md *Code style* → *Types and values* |
| a case label read as copy | `caseText` (`Data.Variant.Case`), in the view model module | order-form, potluck, espresso-bar | writing.md *Code style* → *Types and values* |
| a number as a bar, gauge or stars | `linearProgress elapsedFraction`; `progressBar seatOccupancy` | timer; meeting-booker | the design-system module |
| fixed copy introducing what follows — a heading, a caption | `headline4 $ staticText "Create account"` as a block line; inside one element, `label $ Semigroupoid.do { staticText "°C "; input @"celsius" "text" }` | signup-form; temperature-converter (HTML) | writing.md *Code style* → *Types and values*; `PUI.Web` |
| fixed copy on its own — a note | `listItem $ staticText "…"` | photo-gallery | writing.md *Components* |
| a rule between things | `divider $ …`, `hr $ …` — shown only while both sides render something | meeting-booker, photo-gallery, restaurant-menu | writing.md *Code style* → *Types and values*; `PUI.Web` |
| a pane for one state of the model | `# inCase @"problem" bookingState`; `# inCaseAt @"serving" @"display"` — displays and editors alike | flight-booker, potluck; ticket-dispenser, checkout, calculator | writing.md *Conditional visibility* |
| a list, displayed | `ul $ ( li $ text lapLine ) # foreach @"number" lapRows` | stopwatch | writing.md *Collections* |
| a card that edits nothing | `card $ body1 $ text summaryLine` | order-form, product-review | writing.md *Components* |
| a readout that settles before it redraws | `# debounced itinerarySettleTime` | flight-booker | `PUI` |
| a hint on hover | `# tooltip "You must accept the terms of service to sign up"` | signup-form, espresso-bar | the design-system module |
| a value-computed attribute | `attrWith "style" keyFace` | calculator, cells, circle-drawer | `PUI.Web` |
| a field as an attribute | `attrAt @"x" "cx"` | circle-drawer | writing.md *Code style* |
| a fixed attribute or class | `"style" := "…"`; `cl "dish"` | cells; restaurant-menu | `PUI.Web` |
| a class that depends on the value | `# clWhen isCompleted "todo-done"` | todo-list | `PUI.Web` |
| structure that varies with the value | `dynamic documentView`; `each items …`; `el ("h" <> show h.level)` | markdown-previewer | `PUI.Web` |
| an element with nothing in it | `static (span >>> cl "dish-dots")` | restaurant-menu, reorder | `PUI` |
| an element whose whole face is its decorators | `… $ identity` — the wire, decorated (`circle >>> attrWith "fill" circleFill $ identity`) | circle-drawer, color-mixer | writing.md *Stages* |

## Editing

| The screen needs | Word | Demo | Read |
| --- | --- | --- | --- |
| a text field | `filledTextField @"First name"` | order-form | writing.md *Components* |
| a text field checked as the user pauses | `debouncedTextField @"Username" usernameSettleTime` | signup-form | the design-system module |
| a yes/no over a two-case field | `checkbox @"I accept the terms of service" @"accepted" @"declined"` | signup-form, espresso-bar | the design-system module |
| a switch | `toggleSwitch @"Takeaway cup"` | espresso-bar | the design-system module |
| a selection that always has a value | `select @"Flight type" (choice @"one-way" <+> choice @"return")` | flight-booker | writing.md *Components* |
| a selection owed but not yet made | `dropdownUnpicked @"Room" @"chosen" […]`; `segmentedButtonUnpicked @"Dish" @"chosen"` | meeting-booker; potluck | writing.md *Components* |
| a selection the user may leave unmade | `dropdownOptional @"Catering" @"ordered" @"none" […]` | meeting-booker | writing.md *Components* |
| a bounded quantity | `sliderLive @"Duration"`; `slider @"Split between"` | timer; tip-calculator | writing.md *Code style* → *Types and values* |
| two controls on one field | `slider @"Tip percentage"` then `rangeInput @"Tip percentage"` | tip-calculator | writing.md *Components* |
| a labelled group over a sub-record | `group @"Customer" $ Semigroupoid.do …` | order-form, potluck, reorder | writing.md *Components* |
| a reusable sub-form over a sub-record | `group @"Address" addressForm` | parcel | writing.md *Components* |
| an editor that exists in one state | `# inCaseAt @"return" @"Flight type"` | flight-booker, meeting-booker | writing.md *Conditional visibility* |
| an invariant among edited fields | `# settled fromCelsius` | temperature-converter, meeting-booker | writing.md *Stages* |
| a choice whose cases carry their own fields | `tabBar @"Mode" (…)`, each case's field `# inCaseAt @"Dine in" @"Mode"` | order-form | writing.md *Conditional visibility* |
| an effect's outcome folded into the model it lands on | `( Semigroupoid.do { button …; … # action estimateDistance # atCase … } ) # joined` | order-form | writing.md *App shape* |

## Events into state

| The screen needs | Word | Demo | Read |
| --- | --- | --- | --- |
| a button stepping the model | `button @"Count"` and `fold @"Count" increment` | counter, todo-list | writing.md *Stages* |
| a button with a fixed amount | `button' @"Take a deposit" { icon: "savings" }` and `fold @"Take a deposit" takeDeposit`, the amount baked into the handler | cashbox | writing.md *Business functions* |
| an event folded into the model | `fold @"Customer refunded" refundStandard`; a status is a fold with a face and tells only what an action returned (`snackbar @"Person created" personCreatedLine identity`); the loop's folds merged in `VariantToRecord.do` | cashbox, crud | writing.md *Code style* → *Business functions* |
| a clicked element naming itself | `clicked @"Cell claimed" @"key" (…)` | tic-tac-toe, calculator, cells | `PUI.Web` |
| a click position on a canvas | `onClickedXY @"picked"` | circle-drawer | `PUI.Web` |
| an emitter shown in one state | `# providedAt @"halted" @"phase"`; `# provided @"finished" quizPhase` | stopwatch; quiz, inbox | writing.md *Conditional visibility* |
| an event replacing the model whole | `reset @"New game" openingPosition`; on a status, `snackbar @"Order placed" orderPlacedLine (const usualOrder)` | tic-tac-toe, quiz, shopping-cart, espresso-bar | `PUI` |
| an outcome that is the model | `snackbar @"Card charged" cardChargedLine identity`; `capture @"Forecast fetched"` where nothing needs telling | crud, order-form, payment, weather, password-generator, reorder | `PUI` |
| two buttons stepping one field | `fold @"Next" stepOnward`, `fold @"Back" stepBack` — two cases, two folds, never one button with a caption parameter | checkout | writing.md *Components* |
| an event case routed to its stage | `# atCase @"Create"` | crud, reorder | `PUI` |
| an event intercepted by a dialog | `( Semigroupoid.do { confirmed "Refund" "Refund the customer?" $ …; fold @"Customer refunded" refundStandard } ) # atCase @"Refund a customer"` — the dialog owns the button's case in the fold block | cashbox | writing.md *Stages* |
| an event carrying something of its own | `listOf @"Todo toggled" @"key" … # joined`, handled by `toggleTodo :: { event, model } -> model` | todo-list, cells, calculator | writing.md *Stages* |
| a button group fed the record it replays | `RecordToVariant.do` of emitters as a stage, each button replaying the row the block is fed | order-form, espresso-bar, signup-form | writing.md *Stages* |
| a menu of presets | `menu "Presets" ( RecordToVariant.do menuItem @"The usual" … )` | espresso-bar, inbox | the design-system module |

## Effects, time, statuses

| The screen needs | Word | Demo | Read |
| --- | --- | --- | --- |
| an `Aff` action on an event | `( Semigroupoid.do { indeterminateLinearProgress # action createPerson; VariantToRecord.do { … its outcome statuses, each with its fold } } ) # atCase @"Create"` in the fold block, the action typed at exactly its outcomes | crud, reorder | writing.md *Business functions* |
| … opened by its outcome statuses | `( VariantToRecord.do { snackbar @"Flight booked" bookedLine; snackbar @"Booking rejected" rejectedLine } ) # action @( … ) submit # atCase @"Book"` | flight-booker | writing.md *Business functions* |
| an outcome told, the model kept | the action `joined` with its button in the event ensemble, its status folding `kept` (`snackbar @"Order submitted" submittedLine kept`) | order-form, flight-booker, incident-report | writing.md *Business functions* |
| an action at load | `indeterminateLinearProgress # action loadOrder` then `snackbar @"Order loaded" orderLoadedLine identity` (or `capture @"People loaded"` where nothing needs telling) | order-form, crud | writing.md *App shape* |
| an action's outcome cases | named by its `Aff` (`Aff [ "Person created" :: model, "Person not created" :: model ]`), each folded by its status | crud | writing.md *Business functions* |
| a periodic occurrence | `ticks @"Clock ticked" tickPeriod` and `fold @"Clock ticked" tick` | timer, stopwatch | `PUI` |
| a status per outcome case | `snackbar @"Flight booked" bookedLine` | flight-booker, order-form | writing.md *Components* |
| confirm before the flow continues | `confirmed "Refund" "Refund the customer?" $ …` | cashbox | writing.md *Modals* |
| a fact the platform knows and the app cannot seed — position, connectivity, the time, the colour scheme, a chosen file | `geolocated @l`, `online @l`, `clock @l period`, `colorScheme @l` (`PUI.Web`), `filePicked @l` (`PUI.Web.HTML`), merged `RecordToRecord.do` | here-and-now, incident-report | writing.md *The pipeline*; `PUI.Web` (`source`) |
| a dialog of choices | `dialog "Delete the last message?" $ RecordToVariant.do …` | inbox | writing.md *Modals* |
| an informational dialog | `simpleDialog "Got it" "About this dashboard" (…)` | weather | writing.md *Modals* |

## Collections

| What comes in → what goes out | Word | Demo | Read |
| --- | --- | --- | --- |
| the array → each element shown, the row passing on | `# foreach @"number" lapRows`; a stored array `# actedAt @"key" @"cells"` | stopwatch; cells | writing.md *Collections* |
| the array → each element's event | `clicked @"Key entered" @"key" ( … ) # anyOf @"key" @( key :: String ) (const keyPad)`; a stored array `# anyOfAt @"key" @"cells"` | calculator, tic-tac-toe, shopping-cart, color-mixer; cells | writing.md *Collections* |
| the array → the array, decided jointly or edited in place | `# acted @"name"`; `# acted @"id"`; a stored array `# actedAt @"id" @"Setlist"` | potluck; reorder; reorder (HTML) | writing.md *Collections* |
| one `{ key, value }` at a time → tagged output | `# dispatched @String @{ code :: String, status :: String } arrival` | departures | writing.md *Collections* |
| one `{ key, value }` at a time → the array | `# accumulated @String @{ team :: String, points :: Int } goal` | scoreboard | writing.md *Collections* |
| a selectable list (MDC2, MDC3) | `listOfAt' @"Message opened" @"id" @"messages" { selected: highlighted } (…)` | inbox | writing.md *Collections* |
| a selectable list elsewhere | `clicked @"Person picked" @"key" (…) # anyOf @"key" …` | crud (HTML, Bootstrap, Fluent, Shoelace) | writing.md *Collections* |

## View model not written yet

| You need | Word | Read |
| --- | --- | --- |
| the type a missing function must have | a typed hole, `?countLine` | writing.md *Writing order* |
| the view model module's signatures, all at once | a typed hole for every imported value; the compiler's last message lists them, nothing unknown | writing.md *Writing order* |
| the model row | `# looped @( … ) # with seed` (counter, inbox); `# looped @( … )` after a load action folded in (crud, order-form) | writing.md *Types and values* |
| a derived row, where it is introduced | a classifier's first pane `# inCase @l @( … ) f` (flight-booker), `# action @( … ) f` (flight-booker), `# foreach @k @( … ) proj` (stopwatch), `# anyOf @k @( … ) proj` (color-mixer), `# with @{ … } seed` (potluck) | writing.md *Types and values* |
| the view running before its view model exists | `hole` (`PUI.Web`) | writing.md *Writing order* |
| to know the app is finished | no hole left | writing.md *Writing order* |
