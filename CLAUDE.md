# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Bambik is a prototype PureScript library implementing **Profunctor User Interfaces** - a novel approach to declarative Web UI development using Material Design Components. The key insight is that profunctors unify optics (data structure navigation) and arrows (data flow), making them ideal for composable UI development.

## Build Commands

Do `export PATH=$PWD/node_modules/.bin:$PATH` and then `spago build` (tests: `spago test`). Note the repo builds with the forked PureScript compiler pinned in `package.json` (variant row sugar `[ l :: T | r ]`, `.label` constructors — see doc/variant-sugar.md), so `npm install` first.

## Building & Deploying Demos

1. Verify the forked compiler: `node_modules/.bin/purs --version` must report `0.15.16 [development build ...]`; if it shows stock `0.15.15`, run `npm install` (stock purs fails with "Module Prim.Variant was not found").
2. Bundle one-off (the npm `demo-*` scripts are watch-mode, not for deploys): `spago bundle-app --main Main --to demo/<d>/bundle.js --path "demo/<d>/**/*.purs"` for each of `1`, `2`, `mdc`.
3. Deploy: `npm run deploy-demo-1|deploy-demo-2|deploy-demo-mdc` — each scps the demo dir to host `xyz` (root@erykciepiela.xyz, see `~/.ssh/config`) at `/var/www/html/bambik/demo/`.
4. Verify: `http://erykciepiela.xyz/bambik/demo/<d>/` returns 200 (plain HTTP only).

## Architecture

### Core Type

```purescript
newtype UI m i o = UI (m { toUser :: New i -> Effect Unit, fromUser :: (New o -> Effect PropagationStatus) -> Effect Unit })
```

- `i` - input type (data model to display)
- `o` - output type (data model to capture)
- `toUser` - pushes model updates to UI
- `fromUser` - captures user interactions

### Key Source Files

- **src/UI.purs** - Core UI profunctor type with class instances `Profunctor`, `Strong`, `Choice`, `Semigroupoid`, `Category` (`identity` is the echo wire), the four row merges (`RecordToRecord`, `RecordToVariant`, `VariantToRecord`, `VariantToVariant`), the two mixed strengths (`Resolving`, `Retaining`), and the **trace quartet** — `Costrong`/`Cochoice` (ecosystem duals of `Strong`/`Choice`: state feedback and iteration; knowledge-gated) and the coined `Coresolving`/`Coretaining` (terminating fold, productive unfold; each co-strength is its strength's retraction, `co (strength g) ≅ g` once the state channel is primed) — plus the leaf combinators: `silence` (the silent widget), `announce` (the announcing constant — one registration emission of a value; seeds fields/cases and primes gated traces, generalizing the units' `{}` announcement), `seeded` (the seeded echo wire — `identity`'s pass-through plus one registration emission of the seed; primes gated traces from inside their inner chain) and `looped` (the `×`-diagonal **self-trace**: feed a widget its own emissions, re-entrancy-guarded — primitive because a gated `unfirst` cannot self-feed; wrapped around a record merge it supplies sibling cross-feed, with per-operand retention falling out of the merge gates). `synced` and `latch` are DELETED — ensembles are now `dimap`-bracketed `looped` record merges
- **src/Web.purs** - DOM monad (`Web = StateT DOM Effect`) and primitive elements (`text`, `input`, `button`, `div`, etc.)
- **src/MDC.purs** - Material Design 2 components (the full https://m2.material.io catalog as far as MDC Web implements it), a two-sorted vocabulary. **Components** are widgets with a model interface, each a citizen of exactly one row direction — `×→×` editors (`filledTextField @l`, `filledTextArea @l`, `checkbox @l`, `toggleSwitch @l`, `slider @l`, `filterChip @l`, `iconToggle @l`, and the type-changing `radioButton @l`, `select @l`, `segmentedButton @l` — `Maybe`-selection in, bare selection out; `tabBar @l` is the same-type selector, `Cons l a () s` with unconditional echo and `MDCTab` activation — the `looped`-ensemble citizen) and the `{ busy } → {}` displays `indeterminateLinearProgress`/`indeterminateCircularProgress`; the `×→+` events `button @l`, `fab @l`, `iconButton @l`, `menuItem @l`; the `+→×` statuses `snackbar @l` (auto-dismissing) and `banner @l` (own dismiss action) — no scalar or polymorphic component interfaces (raw scalar leaves are private; `Web` primitives stay scalar for optic positions). **Oculars** (`card { caption }`, `dialog`, `menu { label }`, `chipSet`, `list`/`listItem`, `dataTable { columns }`/`dataRow`/`dataCell`, `imageList`, `layoutGrid`/`layoutCell`, `topAppBar { title }`, `drawer` (permanent, nav is `{} → {}` chrome), `tooltip { text }`, typography, elevations) are shape-preserving decorators with no model of their own; `divider`/`imageListItem` are announcing statics (`{} → {}` chrome with a face). Internally the live leaf is `field @l`-lifted (preferred over raw `property` for annotation-free inference; runtime-exactness is enforced by the merge gates' `MergeableRecords` trim) with hand-fused chrome (abstract labels can't flow through the merges' `Nub`), while all-chrome groups have concrete rows and stay literal `RecordToRecord.do` merges of announcing chrome (`staticText`/`staticHTML`/`pempty` at `{} → {}`); code order = DOM order
- **src/Data/Profunctor/** - `Cont` (CPS profunctor) + the `Row/` layer; everything else was dissolved or deleted
- **src/Data/Profunctor/Row/** - Row profunctors over `Record`/`Variant`: four direction modules, each carrying its **direction class** — the binary merge plus its nullary unit `pempty`, the genuine per-carrier primitives — with qualified-do sugar (`bind`/`discard`). Everything kept is demo-reachable; laws are stated in the module headers. Type variables follow the photographic schema: focus `f`, background `b`, shot `s` (`Cons l f b s`), reality `r`.
  - **`RecordToRecord.purs`** (×→×) — merge `recordToRecord` (`InclusiveRows` inputs, `ExclusiveRows` outputs; gated on `UI`); over ecosystem `Strong`: `focusRecord` (sub-record focus, background carried), `property` (the type-changing field lens); over bare `Profunctor`: `field` (`property`'s closed-singleton form — the merge-operand/nesting shape, `dimap`-only, annotation-free under the merges; runtime-exactness is enforced by the merges themselves — their `MergeableRecords` evidence trims every operand emission to its declared output row before the gates' left-biased `Record.union`, so lens-rebuilt emissions carrying stale sibling fields can't shadow fresh values); `tapped` (a `Strong`-derived display tap: shows the value flowing through a pipeline stage and passes it on — the display's echo triggers the forwarding; honest over displays only); over ecosystem `Costrong`: `feedback` (the ×-trace at row granularity — a state sub-record loops from output to input; coerce-split like `focusRecord`, so pipeline stages only, and on `UI` the state must be primed by the widget's first emission).
  - **`VariantToVariant.purs`** (+→+) — merge `variantToVariant` (`ExclusiveRows` inputs — one handler per case; `InclusiveRows` outputs); over ecosystem `Choice`: `case_` (the value-level case prism, via `prismE`) and `splitVariant` (the dispatch helper `reelWrap` and `iterate` share); over ecosystem `Cochoice`: `iterate` (the `+`-diagonal trace at row granularity — `again` cases loop back into the input, `done` cases exit; retry/wizard flows).
  - **`RecordToVariant.purs`** (×→+) — strength `Resolving`/`resolve :: p a b -> p (Tuple a c) (Either b c)` (a loop step: `Left` = Done, `Right` = Loop; `UI`-only instances) and its co-strength `Coresolving`/`coresolve` (ties the loop: a terminating fold) with its row form `folding @w` (case `w` continues the fold silently, `done` cases exit — the accumulating-wizard shape); merge `recordToVariant` (ungated broadcast); over `Resolving`: `resolveProperty`, `propertyToCase`, `shutterWrap`, the **`Shutter`** optic with `shutter`/`shutterE`; plus `recordToCase` (introduce an output case; plain `Profunctor`, the unit-pinned merge by law).
  - **`VariantToRecord.purs`** (+→×) — strength `Retaining`/`retain :: p a b -> p (Either a c) (Tuple b c)` (a Mealy/coroutine step; `UI`-only instances — a stateless function can't retain state) and its co-strength `Coretaining`/`coretain` (ties the state channel: a productive unfold/generator) with its row form `unfolding @w` (value fields pass, state fields resume as case `w`); merge `variantToRecord` (gated like ×→×, retaining the other side's last contribution); over `Retaining`: `retainCase`, `caseToProperty`, `caseToRecord` (Mealy reducer), `reelWrap`, the **`Reel`** optic with `reel`/`reelE`.
  - **`Row.purs`** (module `Data.Profunctor.Row`) — the shared floor: the row-constraint vocabulary (`InclusiveRows`/`ExclusiveRows`/`DispatchableVariants`) + the two `dimap`-only widening reshapings (`widenRecordInput`/`widenVariantOutput`) the `UI` merge instances build on.
  - **`Row/Example.purs`** + **`showcase/App.purs`** — a phantom carrier with label-indexed widget signatures, and a four-direction pipeline written against it; **`test/Main.purs`** — value-level `(->)` tests (`focusRecord`/`property`/`recordToCase`) plus merge unit laws, gating, and the trace quartet and its row forms (`unfirst`/`unleft`/`coresolve`/`coretain`/`looped`/`iterate`/`feedback`/`folding`/`unfolding`) on the `UI` carrier via a probe harness; **`test/HelloShutterReel.purs`**/**`BusinessOptics.purs`**/**`RestaurantReel.purs`**/**`EntityEventExample.purs`** — `Shutter`/`Reel` as business optics.

### Composition Patterns

- `Semigroupoid.do` (qualified-do) - Data flow pipelines
- `RecordToRecord.do` / `RecordToVariant.do` / `VariantToVariant.do` / `VariantToRecord.do` (qualified-do) - the four row merges
- label-indexed components - every MDC component is a citizen of one direction (`filledTextField @l` ×→×, `button @l` ×→+, `snackbar @l` +→×), so pipeline stages are written directly from components; `field`/`focusRecord` nest sub-composites into larger aggregates
- variant editing - **record-shaped editor state**: the model keeps the variant, the editor keeps every payload; `dimap` brackets the variant in (a state function seeding absent payloads) and out (a projection on the selection), around `looped RecordToRecord.do { selection component; shownWhen panes }` — retention via the merge gates, consistency via the self-trace re-broadcast; unit-payload variants need only the bracket around one selection component
- `tapped` - live views as pipeline stages (slider readouts, summary lines, data tables): display every emission flowing through, pass it on

### Separation of Concerns

- **Business Logic** - Row-shaped models, structural as far as readable (anonymous Record rows for all-at-once, anonymous Variant rows for one-at-a-time; a named alias only for the top aggregate) plus plain functions and Aff actions (demo/1/Main.purs); business optics (Shutter/Reel) where state/loop semantics are needed (test/BusinessOptics.purs)
- **Design System** - Oculars in Web.purs and MDC.purs
- **Composition** - UI elements compose orthogonally to the row combinators

## Demo Structure

- **demo/1/** - Full MDC-based order form as the four-direction row pipeline: load action → `×→×` form (nested record merges; variant editors as `dimap`-bracketed `looped` ensembles of `tabBar`/`segmentedButton` + `shownWhen` panes) → `tapped` live summary → `×→+` event buttons → `+→+` backend dispatch → `+→×` status snackbars
- **demo/2/** - Plain HTML demo: the whole page as one `×→×` merge — announcing statics around a minimal record merge over plain `input`s, drained into `silence`
- **demo/helloworld/** - Simple intro example
- **demo/mdc/** - The full-catalog MD2 showcase: every MDC component in one four-direction pipeline — type-changing form (`SettingsIn → SettingsOut`: radio/select/segmented consume seeded `Maybe`-selections), the shipping variant as a `dimap`-bracketed `looped` ensemble (`tabBar` + `shownWhen` panes), `tapped` slider readout, live data-table/summary views, and a shape-agnostic `tapped` status-log line over the status *variant*, button/FAB/icon-button/menu events, a two-step publish **wizard as a `folding @"next"` stage** (the step state loops silently as the `next` case, primed by `announce`) whose flaky publish **retries via `iterate`** (`Cochoice`: the failed attempt re-emits `publish` with attempt+1), a **session-peak line via `feedback`** (`Costrong`: the `peak` field loops output→input inside the Sliders card, `seeded`-primed, invisible in the stage's outer type), an **activity meter via `unfolding`** (`Coretaining`: a `tapped` arm on the status variant counts events through `retain identity`, the count re-entering as the `resume` case), both progress displays in dispatch, banner + snackbar statuses; page chrome all oculars (`topAppBar` over a permanent `drawer` with a static nav `list`, cards in a `layoutGrid`)

## Key Dependencies

- `profunctor-lenses` - Profunctor-based optics
- `qualified-do` - Syntax sugar for profunctor composition
- `material-components-web` - MDC JavaScript library
