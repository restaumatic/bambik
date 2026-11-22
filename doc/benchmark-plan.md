# Benchmarking bambik on Diner — working plan

A working file: what is decided, what is done, what is next. Part A (Diner
at scale) is the benchmark application; Part B is the harness that measures
the library on it.

## What the code says (2026-11-20)

- **Every model change re-feeds everything attached.** The keyed reconciler
  feeds every surviving element unconditionally (`reconcileKeyed`,
  src/PUI.purs) and every `text f` recomputes `f` on the row it is fed. A
  1 Hz clock therefore costs (attached rows) × (cost of the derived
  functions) per second.
- **Derived functions are recomputed per line.** A station that reads
  `paidOrders` on four lines filters and sorts four times per feed.
- **Detached panes are not fed.** Only the open station pays; a tab switch
  pays one full feed on attach.
- **No vocabulary has pagination**: a page is a model field and a derived
  slice.

## Decisions (Eryk, 2026-11-20)

1. Tables are data: 40 of them, picked from a tile grid.
2. The clock ticks every second; the screen shows minutes.
3. The pager is the app's (`pager` in `Diner.Design`), no new
   vocabulary word.
4. The simulated backend has latency and jitter, no failure injection yet.
5. The load's progress indicator is indeterminate (`load` reports only
   started and ended).

## Part A — Diner at scale (done 2026-11-20)

- [x] `Diner.Service`, the stand-in server: the day's schema, a pure seeded
      generator (`dayAt load time`, 5 ms for the standard load, 60 ms for
      10 000 orders), `Aff`s answering after a jittered latency. Standard
      load: 304 dishes in five courses, 42 tables, 33 open orders (3
      taking, 20 cooking, 10 ready) at tables drawn by lot, 219 paid,
      placed over the last 24 hours.
- [x] Startup: `indeterminateLinearProgress # load openService`, then
      `capture`, before the knot; the service opens on the first free table.
- [x] Time in the model: `now` folded from `ticks` every second; tickets and
      bills carry the minutes waited, coloured once late (15 min) and
      overdue (30 min).
- [x] Pagination: `perPage` and the pages turned are model fields
      (`turned`, one entry per station, valid for the search it was turned
      at); every station lists a derived slice and ends in the shared
      `pager` of `Diner.Design`; stock is edited through its slice
      (`actedWithin`); one search field narrows the stockroom and the menu.
- [x] Dishes in groups: every dish belongs to a course (five of them); the
      stockroom and the menu are each browsed by group (a tab bar, "all"
      first) or searched by name across every group — the search field on
      top, the groups under it only while nothing is searched for — each
      with a group and a search of its own; a page turned stands while
      group and search do.
- [x] The hall: tables as tiles in four colours with a sign each and a
      legend under them, the served table's line saying which order it
      waits for and since when; the menu as tiles to tap (`surfaceTile`
      in `Diner.Design`, dimmed and deaf when sold out), 24 to a page
      (`perPage` is `{ list, tiles }`).
- [x] Prices: a dish's price is edited in the stockroom (`priceField` in
      `Diner.Design`), kept as written with a comma before the grosze and
      read by the view model, a price that cannot be read taking the dish
      off the menu; amounts are shown with their thousands set apart
      (`18 311,50 zł`); an order line carries the
      price its dish had when it was put on the order, so bills, receipts
      and sales do not move when a price does. The statistics are browsed
      by group and searched like the stockroom and the menu, on their own
      state, and their totals are those of the dishes listed.
- [x] The kitchen: three tickets to a row on a desktop, a ticket's head on
      one line, its lines drawn tight. The pass asks before a bill is
      taken as paid, the kitchen before an order is passed on as ready. A
      menu tile says beside its dish's name how much of it is on the order
      being taken (`×3`) and takes the colour of a table that is ordering.
- [x] Every list in a stated order: stock and menu by name, tickets and
      bills the longest waiting first, receipts the latest placed first,
      sales the best selling first; open orders sit at tables drawn by lot,
      so a table's number says nothing of its order's; amounts are set
      against the right edge of their column.
- [x] Compact and mobile: tables of two to four columns, the hall's tables
      as a tile grid, controls in one cell; `test/fits.mjs` asserts no
      sideways scroll of the page or of any table at 360, 768 and 1280 px,
      per station.
- [x] 39 part tests green (`npm run part -- diner`, 70 s); the L15 stack
      green.
- [ ] Fixtures at 10× and 100× of the standard load: moved to B2, where
      their shape is decided (`Diner.Fixtures.busy…` are the 1× ones).

What Part A changed outside demo/diner/:

- `PUI.Web.MDC3.dataTable'`: a full-width table tightens its cells under
  600px (a container query, as `layoutGrid` has), cells take stacked
  paragraphs without their margins. Only diner writes `fullWidth: true`.
- scripts/part-test.mjs: `resize(width)` for a test.
- `select'`'s unavailable options lost their demo with diner's table
  select; the smoke test moved to the MDC3 law bench, whose `select'` row
  now rules an option out by the row it is fed.

What Part A found, before any measurement:

- **No silent way to read the clock on a tick.** `ticks` emits the row; an
  `action` that would read the wall clock must be opened by a progress
  indicator, and `clock` is a source of local hours, minutes and seconds
  with no date. Diner therefore counts seconds, as timer and stopwatch do,
  and its clock falls behind when the browser throttles the page's timers.
  B4 measures it; a silent action slot or an instant-valued source is the
  vocabulary question behind it.
- **A station's pager reads the station field.** The page line and the
  page buttons are one component for six lists because `listed` matches on
  `"Stanowisko"`; a part mounted alone needs its seed at its own station.

## Part B — the benchmark (built 2026-11-21)

`npm run bench` (scripts/bench/, its manual the header of run.mjs): seven
scenarios, about 18 minutes in full, `-- quick` shortened, `-- <scenario>`
selected, `-- compare` a run against bench/baseline.json. Every run writes
bench/results/<date>-<commit>.json (every sample summarised) and a markdown
summary beside it; **bench/baseline.md is the committed run to read**.

Mounts are compiled into .bench/ against a copy of output/ and bundled
unminified: a station alone on a day of a given load
(`Diner.Fixtures.busyWith load perPage station`), the six stations under
their tab bar (the bench's own module, for the walk), or the application
as it starts. **Scale is a compiled fixture, never a knob in the
application.** The whole application is measured only as it starts, at the
standard load; the scale sweep is per station.

Every number is attributed to a layer:

| Layer | What it is | Measured by |
| --- | --- | --- |
| view model | the pure functions of `Diner` | node, importing output/Diner (`viewmodel`), and the profile's share |
| carrier | feeds, gates, the keyed reconciler, the view's own composition | CPU profile of the bundle, self time by module; a library frame counts to the nearest frame above it that is not one |
| design system | `@material/web` and Lit | the same profile |
| browser | style, layout, native DOM | `Performance.getMetrics` deltas, and the profile's `(program)` |

What each scenario does:

| Scenario | Does |
| --- | --- |
| `viewmodel` | every read a station's lines make per feed, timed in node on days of 1× to 64× |
| `responsiveness` | everything a user does at each station, 30 times with real input, at full and at quarter speed, then under a profile; a second passing at each station, a station switched to, the start |
| `scale` | each station alone at 1×, 4×, 16×, 64×: drawn, a second passing (a clock line of the bench's own), one thing done; then with pagination off at 1× and 4× |
| `walk` | 3 000 seeded random steps over the six stations, each applied to a reference model with the application's own folds (output/Diner) and the station on screen compared, whole, with what the read functions say of the reference |
| `races` | twelve unawaited clicks, digits typed across ticks, clicks during an effect, the last page's only row leaving, a dialog confirmed twice |
| `soak` | four minutes of random clicks under the clock sampled after forced collections; what one doing leaves behind; 900 station switches against a baseline; the clock's rate and a frozen page |
| `size` | synthetic applications of 6, 12, 24, 48 stations: compile time, bundle bytes, drawn, fed, clicked |

Not done, against the plan above it replaced: simulated order arrivals (the
application has none), `check-determined` time on the synthetic
applications, accelerated time in the soak (it runs four real minutes), a
pass/fail budget subset for CI.

## Findings (baseline of 2026-11-21, bench/baseline.md)

i7-11850H, Chrome 158 headless. "At ¼" is 4× CPU throttle, the stand-in for
a mid-range phone. No library change was made in this work; each finding
that asks for one names it.

**Holds.**

- **Correctness.** 3 000 random steps of real input: after every one the
  station on screen read exactly what the view model says. All six races
  hold. No exception and no console warning in any scenario.
- **Responsiveness on the standard day.** Of 38 kinds of doing, 35 are
  within budget at ¼: a click costs 4–6 ms at full speed (13–22 ms at ¼), a
  page turned 4–20 ms, a key in a field 2–14 ms. The start is 46 ms to the
  progress bar and 53 ms more to draw the day (181 and 222 ms at ¼).
- **Pagination does its work.** DOM nodes stay flat from 1× to 64× at every
  paged station; 900 station switches leave nodes and listeners unchanged.
- **Bundle.** 499 KB minified (95 KB gzipped) at 6 stations, 857 KB (108 KB)
  at 48: the floor is the design system and the runtime.

**1. Rows that leave a keyed list are never freed — carrier, a leak.**
A page turned leaves behind, for good: 110 DOM nodes in the report (its ten
rows), 2 446 nodes and 266 listeners in the kitchen, 4 620 nodes, 490
listeners and 0.7 MB in the stockroom. Four minutes of random clicks took
the page from 8 674 to 1 418 490 nodes and from 6.6 to 284 MB. A heap
snapshot names the retainer: in `reconcileKeyed` (src/PUI.purs) the
subscription closure made for each entrant (`inst.subscribe \b -> onEmit k
slot b`) shares its closure context with `oldByKey`, the map of the
previous entries, so every generation of rows keeps the one before it
alive. *Change asked:* build that closure where `oldByKey` is not in scope.
*Fixed 2026-11-21* (`enteringKeyed`, 75c164ef): a page turned leaves no
node and no listener behind at any station.

**2. Every read is recomputed by every line that asks — the idiom's cost.**
A station's lines each call their own read function on each feed, and
nothing is shared between them. Statystyki asks for the day's sales seven
times per feed (the table, both totals, the search state, the pager's
three): 5.6 ms in node on the standard day, 7.6 ms per second in the
browser (33 ms at ¼ — over the 16 ms budget), 58% of that station's
profile. It grows linearly with the day (exponent 1.06): 712 ms per second
at 64×, 88% of it the view model.

**3. A feed costs the whole attached model, not the change — carrier.**
A second passing, or any click, feeds every attached row again: 0.02–0.08
ms per row per feed (a stock row 0.063–0.074, a menu tile 0.032–0.034, a
report row 0.023–0.024). So with pagination off the stockroom's second
costs 21 ms at 304 rows and 95 ms at 1 216. And since every read is over
the whole day, even the paged stations grow: "a portion more" in the
stockroom is 4 ms on the standard day and 90 ms at 64× (growth 0.73).

**4. A row of controls is expensive to draw — design system.**
A stock row (two text fields, three icon buttons, a dialog) is 474 DOM
nodes with its shadow trees and 1.8–2.1 ms to draw: 304 rows 0.7 s, 1 216
rows 2.3 s. A report row is 0.2 ms. The design system is 35% of the
stockroom's profile, and with `md-tabs`' `scrollToTab` the stockroom's
group pick is over budget (108 ms at the 95th percentile at ¼). That
scroll is the catalogue's own cost of a tab picked: leaving the active tab
unwritten on a feed was tried and changed nothing (2026-11-21).

**5. Every pane is built at the start — carrier.**
A station that is not open is still built, into a detached fragment: the
synthetic application's nodes grow with its stations (966 at 6, 3 696 at
48) with one station open, and drawing it grows with them (44 → 78 ms).
The cost is small at Diner's six stations; it is the start's to pay, not
the first visit's.

**6. Compile time grows faster than the application — compiler.**
The view and view model of a synthetic application: 0.3 s at 6 stations,
0.7 at 12, 2.2 at 24, 8.7 at 48 — the last doubling costs four times.

**7. The clock is not the wall's — vocabulary.**
`ticks` waits its full period after each beat is handled, so the clock runs
at 0.9969 of real time with the hall open (11 s lost per hour, more where a
second costs more), and a page frozen for six seconds loses them. Diner
counts seconds because an `action` cannot read the wall clock without a
progress indicator and `clock` carries no date. *The drift is fixed
2026-11-21* (3ab462cb): a beat is due at whole intervals from registration,
and the clock keeps the wall's rate. Seconds a frozen page sleeps through
are still lost, and that is the application's to mend, by reading the wall
clock instead of counting beats.

**8. Over budget at ¼ on the standard day.** A group picked in the
stockroom (108 ms, finding 4), switching to Statystyki (106 ms, finding 2),
a second passing at Statystyki (36 ms, finding 2).

## What remains, read together (2026-11-21)

Findings 2, 3 and 8 are one thing: the wire carries whole values and no
stage knows what changed, so every feed makes every attached line call its
read on the whole model and write the result. Where the time goes says
which remedy matters: at 64× the view model is 47–89% of every station's
profile but the hall's (20%), so at scale the reads dominate, not the
carrier. Three changes were weighed; **none is made**, by decision
(2026-11-21).

**A. Keyed rows skip an unchanged element.** A row whose element equals
the one it was last fed, and which has not emitted since, is not fed
again. Spiked in `enteringKeyed` (a dozen lines, structural equality) and
measured, then taken out: a second passing with everything on screen went
from 20.2 to 12.4 ms in the stockroom (304 rows), 96.0 to 56.9 (1 216
rows), 12.8 to 8.5 in the hall (346 tiles), 7.6 to 2.9 in the kitchen (80
tickets); nothing moved on the standard paged day; the 3 000-step walk and
Diner's 39 part tests held. It breaks one law test, which pins today's
behaviour (`acted laxity: the two-stage form re-feeds the untouched
q-element on every gather`), so adopting it restates that. A reference
check before the structural one would make unchanged rows nearly free
where a read returns the same records. It helps long unpaged lists only
and leaves finding 2 as it is.

**B. The same rule on the field lens.** `focusField` — behind `group @l`
and every label-indexed editor — would skip its inner stage while the
field is the reference it was last fed and the stage has not emitted
since; A is then the keyed case of one rule, *a strength does not re-feed
an unchanged focus*. A station under a field that excludes `now` would
cost nothing on a tick (Statystyki's 33 ms per second at ¼). It asks for
one more law — feeding a component the value it was last fed changes
nothing — and for Diner's model regrouped (`{ now, day }`), the kitchen
reading both. Not measured.

**C. Reads computed once.** B stops needless feeds, not seven lines
recomputing the sales on a real change. Three routes: (1) the
application's own — derived data is a model field kept by the folds that
change it, no library change; (2) a presenting stage, one function from
the model to the station's view-model row run once per feed, its lines
reading fields by label — a narrowing word, which reopens a decision
taken against them; (3) footprints in the types — closed, L18 records the
open-row footprint tried and withdrawn.

**The rest.** Finding 4 is the catalogue's two text fields per row;
recycling a leaver's nodes for an entrant is unsound (a row holds dialogs
and effects in flight), and one editable row at a time is Diner's design
to choose. Finding 5 can be mended — keyed rows already build late — but
moves the cost to the first visit and changes when a pane's
registration-time announcements fire; Diner's start is 100 ms. Finding 6
is the compiler's and bites one module of two dozen stations, which parts
already avoid.

The growth left after finding 1's fix is not a leak in the DOM: nodes
swing between 12 000 and 17 000 with what is on screen and do not trend;
the heap's 0.2 MB a minute is untraced, and the walk adds orders to the
model as it goes.

## Log

- 2026-11-20: plan agreed; Part A done and pushed (5b1f877e, d807c07b).
- 2026-11-21: Part B built; baseline run saved (bench/baseline.json, .md).
- 2026-11-21: finding 1 fixed (`enteringKeyed`, src/PUI.purs) and the
  baseline replaced by the run after it. A page turned now leaves no node
  and no listener behind at any station; the same four minutes of random
  clicks end at 16 979 nodes and 10.1 MB (were 1 418 490 and 284 MB). What
  is left and not yet traced: about 310 nodes, 46 listeners and 0.2 MB a
  minute over the second half of that run, and 5–18 KB of heap per page
  turned or pick. The findings above keep the numbers of the first
  baseline (`git show 2a800c94:bench/baseline.md`).
- 2026-11-21: the clock's drift fixed (finding 7, 3ab462cb); the bench
  counts a clock's beats by lineage, since such a clock waits less than
  its period after a beat that took time; baseline replaced by the run
  after both. Changes A, B and C weighed above and not made.
  Read the new baseline with two cautions. A second passing at Magazyn
  reads 3.1–3.3 ms in three runs since the clock change against 2.3
  before, cause not found. And a count at the 95th percentile of five or
  six samples is their largest: the runs disagree on which doings are over
  budget at ¼ beyond the three of finding 8 (a stockroom page turned and
  the switches to Magazyn and Sala come and go). At 64× a tick of
  Statystyki outlasts the test clock's period, so its 1 089 ms spans more
  than one beat and is not the 733 of the first baseline.
  Next, when taken up: A with a reference check first and its law test
  restated; B spiked on Diner and measured; C's first route in Diner for
  Statystyki — each a separate change through the guardrails' gates,
  re-measured with `npm run bench -- compare`.
- 2026-11-22: the clock taken out of Diner's model (be5f9e20, 51bba2b3):
  decision 2 above is superseded. The time is read where it is drawn, by
  `timestamped @"time"` on the four lines that show time passed and
  `timestamping @"time"` on the outcome of the order sent, so a beat feeds
  those lines and never turns the knot. Baseline replaced by the run
  after it (`git show eb718d3a:bench/baseline.md` for the one before). A
  second passing in the application: 0.28–0.77 ms at full speed (was
  1.2–7.9), 0.66–1.68 at ¼ (was 3.4–32.2), every station within the 16 ms
  budget; on a station alone it no longer grows with the day (Statistics
  at 64×: 0, was 1 089 ms; Hall 0.45, was 91). The walk and the soak read
  as before. That run read Wydawka dearer — every bill on screen drawn in
  71 ms (was 53), switched to at ¼ in 35 (was 26) — and it is not: the
  first was one sample and the second five. Measured again on the same
  build, 15 and 36 times: drawn in 49 ms (46–51), 70 at 4×; switched to
  in 5.7 ms, 24.0 at ¼. A row's own line reading the time costs nothing
  that shows, and `timestamped` is left as it is. The bench now draws an
  unpaged station five times, not once, and switches to each station
  twenty times, the first visit left out. Untouched, and
  still what A, B and C above are about: a real change at Statystyki runs
  its seven aggregations, a stockroom page turned and a group picked are
  over 100 ms at ¼, and so is the switch to Statystyki.
- 2026-11-22: the measure changed, so no number before this entry
  compares with one after it. A doing was timed as script, style and
  layout with the layout forced; it is now everything the main thread
  does from the doing to the end of the frame that shows it, painting
  and compositing included (`rest` in the split), with a frame in which
  nothing is done measured beside them as the floor (1 ms, 2.6 at ¼).
  A trace of a station switched to showed the old measure counting a
  third of the work: Magazyn 16 ms measured, 61 done. Baseline replaced
  by the run under the new measure (`git show f4b3d99b:bench/baseline.md`
  for the last under the old), made on 199b9603 before `revealing` and
  `awaited` went into Diner. Over the 100 ms budget at ¼ now, by the
  95th percentile: a stockroom group picked (199), a stockroom page
  turned (167), a kitchen order passed on and confirmed (165), the
  switches to Magazyn (136), Statystyki (134) and Sala (123), a kitchen
  page turned (120), and a key in the stockroom's search (125, median
  29). A third to a half of each is painting and compositing: a station
  that is not open is out of the document, so a switch styles, lays out
  and paints it anew.
