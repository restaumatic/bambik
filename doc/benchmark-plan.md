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

## Part B — the benchmark

`scripts/bench/`, run by `npm run bench` (`-- <scenario>` filters, `--
quick` for a short pass). Built on scripts/smoke/cdp.mjs and chrome.mjs and
on the part-test mount (`body $ part # looped # with seed`, compiled into a
scratch directory against a copy of output/), so **scale is a compiled
fixture, never a knob in the app**. A fixture mounts one station: the
knot's body is written in `Diner.View` itself (Eryk's decision, inlined
2026-11-20), so the whole application runs only as it starts, at the
standard load (the mount of `test/View.test.mjs`). Startup, the tick with
every station built and the cross-station walk are measured there; the
scale sweep of B2 is per station. If a whole-application sweep proves
necessary, the bench writes its own mount — the six stations under the tab
bar and the clock — into its scratch directory.

Every number is attributed to one of three layers, measured separately:

| Layer | What it is | Measured by |
| --- | --- | --- |
| view model | the pure functions of `Diner` (slices, copy, folds) | node, importing `output/Diner/index.js`, no browser |
| carrier | feeds, gates, the keyed reconciler, scopes | CPU profile of an unminified bundle, bucketed by module |
| browser | custom-element upgrade, style, layout | `Performance.getMetrics` deltas (script, style, layout, nodes) |

### B1. Responsiveness

- Scripted real input (CDP mouse and key events, as the part tests send
  them) per station: add a dish, change a quantity, plate a line, mark
  ready, settle a bill, turn a page, switch station, type in the search
  field, edit a stock count.
- Per interaction: `Performance.getMetrics` before and after (ScriptDuration,
  RecalcStyleDuration, LayoutDuration, Nodes, JSHeapUsedSize), plus
  `performance.now()` around the dispatch. Frames are not relied on:
  headless sessions can run frameless.
- At CPU throttle 1× and 4× (`Emulation.setCPUThrottlingRate`); 30
  repetitions; p50, p95, max.
- Startup: navigation → progress indicator on screen → load resolved →
  first station rendered.
- Tick: cost of one second passing, per open station.
- Budgets (4× throttle): interaction p95 under 100 ms, tick p95 under 16 ms,
  load-to-rendered under 1 s. A budget missed is a finding, not a failure
  of the run.

### B2. Large data

- Fixtures at 1×, 10×, 100× of the standard load (to 10 000 orders, 1 000+
  dishes) in `Diner.Fixtures`, built with `Diner.Service.dayAt`, each
  scenario of B1 at each scale.
- The same with pagination off (`perPage` is a model field: a fixture sets
  it to the whole collection) to read the carrier's raw cost: ms per row mounted, ms per
  attached row per feed.
- Reported: the fitted growth exponent per scenario; anything superlinear
  flagged with its layer.

### B3. Correctness under stress

- **Random walk with an oracle.** A seeded PRNG picks real clicks across
  the stations for thousands of steps. The same steps run on a reference
  model in node — the pure folds imported from `output/Diner` — and after
  each step the open station's visible text is compared with the copy
  functions applied to the reference model. A divergence reports the step
  and the seed; the run is replayable.
- **Invariants** on the reference model and on the screen: portions in
  stock plus portions on open orders are conserved, order numbers are
  unique, the takings equal the sum of the paid totals, no order is lost
  between stages.
- **Races**: click bursts with no wait between them, clicks while
  `placeOrder` runs, a tick landing between a click and its fold, typing in
  a stock field while ticks land (the value and the caret must survive),
  turning a page while its last row leaves.
- Zero exceptions, zero console warnings (the starvation watchdog's
  included).

### B4. Reliability

- Soak: ticks, random input, simulated order arrivals and 1 000 station
  switches, for 10 minutes of accelerated time; every N steps force a GC
  (`HeapProfiler.collectGarbage`) and sample heap, DOM nodes and event
  listeners.
- Reported: the slope of each series (expected zero within noise) and the
  counts after the switch cycles against the baseline before them.
- A hidden page (timers throttled) coming back: the clock and the screen
  agree again within one tick.

### B5. Application size

- A generator writes synthetic views of 6, 12, 24 and 48 stations with wide
  fold blocks (each station a copy of a Diner station under fresh labels).
- Reported against station count: `purs compile` time for the view and the
  view model, `check-determined` time, bundle bytes (raw and gzip), mount
  time, per-feed cost with one station open.

### B6. Output

- One JSON per run under `bench/results/` (commit, date, machine, Chrome
  version, every sample) and a markdown summary printed to the terminal.
- A committed baseline; `npm run bench -- compare` prints the deltas beyond
  noise. Not part of the L15 gate (slow, noisy); a small budget subset can
  be pass/fail later.

### B7. Findings

Each finding is a measured number with its layer. Candidates the code
already suggests: survivors re-fed unconditionally, derived functions
recomputed per line, custom-element upgrade cost per row. A library change
is proposed separately and passes the guardrails' gates; none is made
inside the benchmark work.

## Order of work

1. Part A.
2. B1 with the attribution layers.
3. B2.
4. B3.
5. B4.
6. B5.
7. B7.

## Log

- 2026-11-20: plan agreed; Part A done, uncommitted. Next: B1.
