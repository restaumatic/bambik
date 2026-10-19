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
| a stage on one event case | the leaf names it: `action @"Shuffle" …`, `confirmed @"Refund a customer" …`, `fold @"Take a deposit" …` — no placement word | reorder, cashbox, espresso-bar | writing.md *Stages* |
| one status per outcome case | `VariantToRecord.do` | order-form | `Data.Profunctor.Row.VariantToRecord` |
| what a composition guarantees | — | — | writing.md *What the laws guarantee* |
| a compile error between two rows | — | — | writing.md *When it does not compile* |
| a pane stays blank | — | — | writing.md *When it does not propagate* |

## App shape

| The app is | Word | Demo | Read |
| --- | --- | --- | --- |
| mounted | `body $ …`, imported from the design-system module | every demo | writing.md *App shape* |
| a model edited and folded | `# looped # with freshCount`, the model row the seed's signature | counter | writing.md *App shape* |
| seeded, with no loop of its own | `# with invitation` | potluck | writing.md *App shape* |
| a form section looping inside it | `# looped` | order-form | writing.md *App shape* |
| an action that retries until it succeeds | the retry inside the action's `Aff` (`chargeFlaky`), the outcome one case | payment | writing.md *Business functions* |
| a wizard's step, a running maximum, a counter | a model field, edited, folded or normalized (`stepper @"step" …`, checkout's own selector, `# settled raiseTop`, `fold @"Take a number" issue`) | checkout, auction, ticket-dispenser | writing.md *Types and values* |

## Showing data

| The screen needs | Word | Demo | Read |
| --- | --- | --- | --- |
| a value, formatted | `headline4 $ text countLine` | counter | writing.md *Components*; `PUI.Web` |
| a field as it is stored | `textAt @"title"`; `textAt @"entry"` | todo-list; calculator | writing.md *Code style* |
| a sentence composed from several fields | `headline6 $ text balanceLine` | cashbox | writing.md *Code style* → *Types and values* |
| a case label read as copy | `caseText` (`Data.Variant.Case`), in the view model module | order-form, potluck, espresso-bar | writing.md *Code style* → *Types and values* |
| a number as a bar, gauge or stars | `linearProgress elapsedFraction`; `progressBar seatOccupancy` | timer; meeting-booker | the design-system module |
| fixed copy introducing what follows — a heading, a caption | `headline4 $ staticText "Create account"` as a block line; inside one element, `label $ Semigroupoid.do { staticText "°C "; input @"Celsius" "text" }` | signup-form; temperature-converter (HTML) | writing.md *Code style* → *Types and values*; `PUI.Web` |
| fixed copy on its own — a note | `listItem $ staticText "…"` | photo-gallery | writing.md *Components* |
| a rule between things | `divider $ …`, `hr $ …` — shown only while both sides render something | meeting-booker, photo-gallery, restaurant-menu | writing.md *Code style* → *Types and values*; `PUI.Web` |
| a pane for one state of the model | `# inCase @"won" gameOutcome`; `# inCaseAt @"serving" @"display"` — displays and editors alike | tic-tac-toe, potluck; ticket-dispenser, checkout, calculator | writing.md *Conditional visibility* |
| a list, displayed | `ul $ ( li $ text lapLine ) # foreach @"number" lapRows` | stopwatch | writing.md *Collections* |
| a card that edits nothing | `card $ body1 $ text summaryLine` | order-form, product-review | writing.md *Components* |
| a hint on hover | `# tooltip "You must accept the terms of service to sign up"` | signup-form, espresso-bar | the design-system module |
| a value-computed attribute | `attrWith "style" keyFace` | calculator, circle-drawer | `PUI.Web` |
| a field marked invalid (styled red by the page) | `# attrWith "aria-invalid" startDateInvalid`, the function answering `"true"`/`"false"` | flight-booker | `PUI.Web` |
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
| a text field with a format or an example under it | `filledTextField' @"Start date" { hint: "DD.MM.YYYY" }` | flight-booker, cells, todo-list | writing.md *Components* |
| a text field checked as the user pauses | `debouncedTextField @"Username" usernameSettleTime` | signup-form | the design-system module |
| a yes/no over a two-case field | `checkbox @"Accept the terms of service" @"accepted" @"declined"` | signup-form, espresso-bar | the design-system module |
| a switch | `toggleSwitch @"Takeaway cup"` | espresso-bar | the design-system module |
| a selection that always has a value | `select @"Flight type" $ VariantToVariant.do` over one `option @"…"` per case | flight-booker | writing.md *Components* |
| one option of a selector | `radio @"Light"`, `segment @"Small"`, `option @"Oat milk"`, `tab @"Latte"` (`tab' @l { icon }`), one per line under the selector's block | espresso-bar | writing.md *Components* |
| a selection owed but not yet made | `dropdownUnpicked @"Room" @"chosen" […]`; `segmentedButtonUnpicked @"Dish" @"chosen"` | meeting-booker; potluck | writing.md *Components* |
| a selection the user may leave unmade | `dropdownOptional @"Catering" @"ordered" @"none" […]` | meeting-booker | writing.md *Components* |
| a bounded quantity | `sliderLive @"Duration"`; `slider @"Split between"` | timer; tip-calculator | writing.md *Code style* → *Types and values* |
| a bounded quantity read in a unit | `sliderLive' @"Amount" { unit: "€" }` | loan-calculator, auction | writing.md *Components* |
| two controls on one field | `slider @"Tip percentage"` then `rangeInput @"Tip percentage"` | tip-calculator | writing.md *Components* |
| a labelled group over a sub-record | `group @"Customer" $ Semigroupoid.do …` | order-form, potluck, reorder | writing.md *Components* |
| a reusable sub-form over a sub-record | `group @"Address" addressForm` | parcel | writing.md *Components* |
| an editor that exists in one state | `# inCaseAt @"return flight" @"Flight type"` | flight-booker, meeting-booker | writing.md *Conditional visibility* |
| an invariant among edited fields | `# settled fromCelsius` | temperature-converter, meeting-booker | writing.md *Stages* |
| a choice whose cases carry their own fields | `tabBar @"Mode" (…)`, each case's field `# inCaseAt @"Dine in" @"Mode"` | order-form | writing.md *Conditional visibility* |
| an effect's outcome folded into the model it lands on | `( Semigroupoid.do { button …; … # action @"Estimate distance" estimateDistance } ) # joined @"Distance estimated"`; two outcomes `# joined2 @"Order submitted" @"Total missing"` | order-form | writing.md *App shape* |

## Events into state

| The screen needs | Word | Demo | Read |
| --- | --- | --- | --- |
| a button stepping the model | `button @"Count"` and `fold @"Count" increment` | counter, todo-list | writing.md *Stages* |
| a button with a fixed amount | `button' @"Take a deposit" { icon: "savings" }` and `fold @"Take a deposit" takeDeposit`, the amount baked into the handler | cashbox | writing.md *Business functions* |
| an event folded into the model | `fold @"Customer refunded" refundStandard`; a status is a fold with a face and tells only what an action returned, its case label its whole copy (`snackbar @"Card charged" identity`); the loop's folds merged in `VariantToRecord.do` | cashbox, crud | writing.md *Code style* → *Business functions* |
| a clickable cell of a grid | `tile @"Cell claimed" @"key" (…)`, styled outside by `attrWith "style" cellFace` | tic-tac-toe, calculator, color-mixer | `PUI.Web.MDC3` |
| a cell edited on double click | `doubleClickTile @"Cell double-clicked" @"key" $ textAt @"text"` | cells | `PUI.Web.MDC3` |
| a cell edited in place, committed when left | `inPlaceInput @"Formula entered" @"Formula" # providedAt @"editing" @"mode"` — focused when shown; Enter, Tab or a click elsewhere commits | cells | `PUI.Web.HTML` |
| a click position on a canvas | `onClickedXY @"picked"` (primary button) | circle-drawer | `PUI.Web` |
| the pointer's position over a canvas | `onHoveredXY @"Canvas hovered" identity # joined @"Canvas hovered"`, merged beside the click source | circle-drawer | `PUI.Web` |
| a right click (context menu) on a canvas | `onContextMenuXY @"Canvas context menu" identity`, the popup a pane at the stored point | circle-drawer | `PUI.Web` |
| an emitter shown in one state | `# providedAt @"halted" @"phase"`; `# provided @"bookable" bookingState` | stopwatch; flight-booker, quiz, inbox | writing.md *Conditional visibility* |
| an event replacing the model whole | `reset @"New game" openingPosition`; on a status, `snackbar @"Order placed" (const usualOrder)` | tic-tac-toe, quiz, shopping-cart, espresso-bar | `PUI` |
| an outcome that is the model | `snackbar @"Card charged" identity`; `capture @"Forecast fetched"` where nothing needs telling | order-form, payment, weather, password-generator, reorder | `PUI` |
| two buttons changing one field | `fold @"Start" beginTiming`, `fold @"Stop" haltTiming` — two cases, two folds, never one button with a caption parameter | stopwatch | writing.md *Components* |
| an event intercepted by a dialog | `confirmed @"Refund a customer" $ body1 $ text refundLine` between the emitters and the folds — the button's case captions the confirm, which re-emits it, its fold (`fold @"Refund a customer" refundStandard`) among the folds | cashbox, inbox | writing.md *Modals* |
| an event carrying something of its own | `listOf @"Todo toggled" @"key" … # joined @"Todo toggled"`, handled by `toggleTodo :: { event, model } -> model` | todo-list, cells, calculator | writing.md *Stages* |
| a button group fed the record it replays | `RecordToVariant.do` of emitters as a stage, each button replaying the row the block is fed | order-form, espresso-bar, signup-form | writing.md *Stages* |
| a menu of presets | `menu "Presets" ( RecordToVariant.do menuItem @"The usual" … )` | espresso-bar, inbox | the design-system module |

## Effects, time, statuses

| The screen needs | Word | Demo | Read |
| --- | --- | --- | --- |
| an `Aff` action on an event | `( Semigroupoid.do { indeterminateLinearProgress # action @"Shuffle" shuffleAction; capture @"Setlist shuffled" } )` in the fold block, what follows owning the action's outcomes | reorder, espresso-bar | writing.md *Business functions* |
| … whose outcomes are told | `indeterminateCircularProgress # action @"Charge card" chargeFlaky`, then `snackbar @"Card charged" identity`; one status per outcome where there are several | payment; order-form | writing.md *Business functions* |
| the load before the knot | `indeterminateLinearProgress # load loadOrder`, then `snackbar @"Order loaded" identity` | order-form | writing.md *Business functions* |
| an outcome told, the model kept | the action `joined @l` with its button in the event ensemble, once per outcome, its status folding `kept` (`snackbar @"Order submitted" kept`) | order-form | writing.md *Business functions* |
| an action at load | `indeterminateLinearProgress # load loadOrder` then `snackbar @"Order loaded" identity` (or `capture @l` where nothing needs telling) | order-form | writing.md *App shape* |
| an action's outcome cases | named by its `Aff` (`Aff [ "Total missing" :: {}, "Order submitted" :: {} ]`), each folded by its status | order-form | writing.md *Business functions* |
| a periodic occurrence | `ticks @"Clock ticked" tickPeriod` and `fold @"Clock ticked" tick` | timer, stopwatch | `PUI` |
| a status per outcome case | `snackbar @"Card charged" identity` — the label is the copy; a reason is a sibling case, told by its own status or, when the user must read it, `acknowledged @"Total missing" $ …` | payment, order-form | writing.md *Components* |
| a message that stays while a state holds | `( banner $ staticText "Inbox zero!" ) # inCase @"empty" inboxState` — a display under a pane, not a status | inbox | writing.md *Components* |
| confirm before the flow continues | `confirmed @"Refund a customer" $ …`, the case captioning the confirm beside a Cancel, then `fold @"Refund a customer" refundStandard`; its content may read the payload (`confirmed @"Book" $ … text bookingLine`) | cashbox, inbox, flight-booker | writing.md *Modals* |
| a fact the platform knows and the app cannot seed — position, connectivity, the time, the colour scheme, a chosen file | `geolocated @l`, `online @l`, `clock @l period`, `colorScheme @l` (`PUI.Web`), `filePicked @l` (`PUI.Web.HTML`), merged `RecordToRecord.do` | here-and-now, incident-report | writing.md *The pipeline*; `PUI.Web` (`source`) |
| a message the user must close | `acknowledged @"About this dashboard" $ …` — the case consumed when its "OK" closes it, the row less it going on; its content may read the payload (`acknowledged @"Report filed" $ … text referenceLine`) | weather, order-form, incident-report | writing.md *Modals* |

## Collections

| What comes in → what goes out | Word | Demo | Read |
| --- | --- | --- | --- |
| the array → each element shown, the row passing on | `# foreach @"number" lapRows`; a stored array `# actedAt @"key" @"cells"` | stopwatch; cells | writing.md *Collections* |
| the array → each element's event | `tile @"Key entered" @"key" ( … ) # anyOf @"key" (const keyPad)`; a stored array `# anyOfAt @"key" @"cells"` | calculator, tic-tac-toe, shopping-cart, color-mixer; cells | writing.md *Collections* |
| the array → each element's edit | `( … iconToggle @"Favorite" … ) # anyEdit @"Favorite changed" @"title" @( … ) visibleMovies # joined @"Favorite changed"` | movie-browser | writing.md *Collections* |
| the array → the array, decided jointly or edited in place | `# acted @"name"`; `# acted @"id"`; a stored array `# actedAt @"id" @"Setlist"` | potluck; reorder; reorder (HTML) | writing.md *Collections* |
| one `{ key, value }` at a time → tagged output | `# dispatched arrival` | departures | writing.md *Collections* |
| one `{ key, value }` at a time → the array | `# accumulated goal` | scoreboard | writing.md *Collections* |
| a selectable list (MDC2, MDC3) | `listOfAt' @"Message opened" @"id" @"messages" { selected: highlighted } (…)` | inbox | writing.md *Collections* |

## View model not written yet

| You need | Word | Read |
| --- | --- | --- |
| the type a missing function must have | a typed hole, `?countLine` | writing.md *Writing order* |
| the view model module's signatures, all at once | a typed hole for every imported value; the compiler's last message lists them, the model's tails left unknown for you to fill | writing.md *Writing order* |
| a knotless form, no seed | each editor's introducing form, its blank an init of the row above it: `filledTextFieldFrom @"Email" blankEmail` (MDC3, also `debouncedTextFieldFrom`, `radioButtonFrom`, `selectFrom`, `checkboxFrom`) | signup-form (MDC3) | writing.md *App shape*; `PUI` (`introduced`) |
| the model row | the seed's signature, `# looped # with seed` on the view (counter, inbox); the load's `Aff` when a load stands before the knot (order-form) | writing.md *Types and values* |
| a derived row | the signature of the function that brings it: a classifier's cases, a projection's element row, an action's outcomes (`# action @l f`, closed by the statuses or `joined` after it — payment) | writing.md *Types and values* |
| the view running before its view model exists | `hole` (`PUI.Web`) | writing.md *Writing order* |
| to know the app is finished | no hole left | writing.md *Writing order* |
