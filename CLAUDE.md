# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Bambik is a prototype PureScript library implementing **Profunctor User Interfaces** - a novel approach to declarative Web UI development using Material Design Components. The key insight is that profunctors unify optics (data structure navigation) and arrows (data flow), making them ideal for composable UI development.

## Build Commands

Do `export PATH=$PWD/node_modules/.bin:$PATH` and then `spago build` (tests: `spago test`). Note the repo builds with the forked PureScript compiler pinned in `package.json` (variant row sugar `[ l :: T | r ]`, `.label` constructors — see doc/variant-sugar.md), so `npm install` first.

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

- **src/UI.purs** - Core UI profunctor type with class instances `Profunctor`, `Strong`, `Choice`, `Semigroupoid`, `Category` (`identity` is the echo wire), the four row merges (`RecordToRecord`, `RecordToVariant`, `VariantToRecord`, `VariantToVariant`), and the two mixed strengths (`Resolving`, `Retaining`) — plus the leaf combinators: `silence` (the silent widget), `synced` (mutually synced sibling editors with cross-feed and a re-entrancy guard), `latch` (seeded per-case payload retention)
- **src/Web.purs** - DOM monad (`Web = StateT DOM Effect`) and primitive elements (`text`, `input`, `button`, `div`, etc.)
- **src/MDC.purs** - Material Design Components, a two-sorted vocabulary. **Components** are widgets with a model interface, each a citizen of exactly one row direction — `×→×` editors (`filledTextField @l`, `filledTextArea @l`, `checkbox @l`, type-changing `radioButton @l`) and the `{ busy } → {}` display `indeterminateLinearProgress`; the `×→+` event `button @l`; the `+→+` selector `switch @l` (`latch` inside); the `+→×` status `snackbar @l` — no scalar or polymorphic component interfaces (the raw scalar button is private; `Web` primitives stay scalar for optic positions). **Oculars** (`card { caption }`, `dialog`, typography, elevations) are shape-preserving decorators with no model of their own. Internally the live leaf is `property @l`-lifted with hand-fused chrome (abstract labels can't flow through the merges' `Nub`), while all-chrome groups have concrete rows and stay literal `RecordToRecord.do` merges of announcing chrome (`staticText`/`staticHTML`/`pempty` at `{} → {}`); code order = DOM order
- **src/Data/Profunctor/** - `Cont` (CPS profunctor) + the `Row/` layer; everything else was dissolved or deleted
- **src/Data/Profunctor/Row/** - Row profunctors over `Record`/`Variant`: four direction modules, each carrying its **direction class** — the binary merge plus its nullary unit `pempty`, the genuine per-carrier primitives — with qualified-do sugar (`bind`/`discard`). Everything kept is demo-reachable; laws are stated in the module headers. Type variables follow the photographic schema: focus `f`, background `b`, shot `s` (`Cons l f b s`), reality `r`.
  - **`RecordToRecord.purs`** (×→×) — merge `recordToRecord` (`InclusiveRows` inputs, `ExclusiveRows` outputs; gated on `UI`); over ecosystem `Strong`: `focusRecord` (sub-record focus, background carried), `property` (the type-changing field lens), `field` (its closed-singleton form — the merge-operand/nesting shape).
  - **`VariantToVariant.purs`** (+→+) — merge `variantToVariant` (`ExclusiveRows` inputs — one handler per case; `InclusiveRows` outputs); over ecosystem `Choice`: `case_` (the value-level case prism, via `prismE`) and `splitVariant` (the dispatch helper `reelWrap` shares).
  - **`RecordToVariant.purs`** (×→+) — strength `Resolving`/`resolve :: p a b -> p (Tuple a c) (Either b c)` (a loop step: `Left` = Done, `Right` = Loop; `UI`-only instances); merge `recordToVariant` (ungated broadcast); over `Resolving`: `resolveProperty`, `propertyToCase`, `shutterWrap`, the **`Shutter`** optic with `shutter`/`shutterE`; plus `recordToCase` (introduce an output case; plain `Profunctor`, the unit-pinned merge by law).
  - **`VariantToRecord.purs`** (+→×) — strength `Retaining`/`retain :: p a b -> p (Either a c) (Tuple b c)` (a Mealy/coroutine step; `UI`-only instances — a stateless function can't retain state); merge `variantToRecord` (gated like ×→×, retaining the other side's last contribution); over `Retaining`: `retainCase`, `caseToProperty`, `caseToRecord` (Mealy reducer), `reelWrap`, the **`Reel`** optic with `reel`/`reelE`.
  - **`Row.purs`** (module `Data.Profunctor.Row`) — the shared floor: the row-constraint vocabulary (`InclusiveRows`/`ExclusiveRows`/`DispatchableVariants`) + the two `dimap`-only widening reshapings (`widenRecordInput`/`widenVariantOutput`) the `UI` merge instances build on.
  - **`Row/Example.purs`** + **`showcase/App.purs`** — a phantom carrier with label-indexed widget signatures, and a four-direction pipeline written against it; **`test/Main.purs`** — value-level `(->)` tests (`focusRecord`/`property`/`recordToCase`) plus merge unit laws and gating on the `UI` carrier via a probe harness; **`test/HelloShutterReel.purs`**/**`BusinessOptics.purs`**/**`RestaurantReel.purs`**/**`EntityEventExample.purs`** — `Shutter`/`Reel` as business optics.

### Composition Patterns

- `Semigroupoid.do` (qualified-do) - Data flow pipelines
- `RecordToRecord.do` / `RecordToVariant.do` / `VariantToVariant.do` / `VariantToRecord.do` (qualified-do) - the four row merges
- label-indexed components - every MDC component is a citizen of one direction (`filledTextField @l` ×→×, `button @l` ×→+, `switch @l` +→+, `snackbar @l` +→×), so pipeline stages are written directly from components; `field`/`focusRecord` nest sub-composites into larger aggregates
- `synced` - mutually synced sibling editors of one value (broadcast + cross-feed, re-entrancy-guarded); `latch` seeds and retains per-case payloads inside it

### Separation of Concerns

- **Business Logic** - Row-shaped models, structural as far as readable (anonymous Record rows for all-at-once, anonymous Variant rows for one-at-a-time; a named alias only for the top aggregate) plus plain functions and Aff actions (demo/1/Main.purs); business optics (Shutter/Reel) where state/loop semantics are needed (test/BusinessOptics.purs)
- **Design System** - Oculars in Web.purs and MDC.purs
- **Composition** - UI elements compose orthogonally to the row combinators

## Demo Structure

- **demo/1/** - Full MDC-based order form as the four-direction row pipeline: load action → `×→×` form (nested record merges, variant case panes) → `×→+` event buttons → `+→+` backend dispatch → `+→×` status snackbars
- **demo/2/** - Plain HTML demo: the whole page as one `×→×` merge — announcing statics around a minimal record merge over plain `input`s, drained into `silence`
- **demo/helloworld/** - Simple intro example

## Key Dependencies

- `profunctor-lenses` - Profunctor-based optics
- `qualified-do` - Syntax sugar for profunctor composition
- `material-components-web` - MDC JavaScript library
