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

- **src/UI.purs** - Core UI profunctor type with class instances `Profunctor`, `Strong`, `Choice`, `Semigroupoid`, `Category`, the four row merges (`RecordToRecord`, `RecordToVariant`, `VariantToRecord`, `VariantToVariant`), and the two mixed strengths (`Resolving`, `Retaining`) — plus the leaf combinators: `silence` (the silent widget), `synced` (mutually synced sibling editors with cross-feed and a re-entrancy guard), `latch` (seeded per-case payload retention)
- **src/Web.purs** - DOM monad (`Web = StateT DOM Effect`) and primitive elements (`text`, `input`, `button`, `div`, etc.)
- **src/MDC.purs** - Material Design Components **based on row profunctors**: compounds are label-indexed (`filledTextField @l :: Cons l String () s => … -> UI Web { | s } { | s }`, `checkbox @l`, type-changing `radioButton @l`) so they slot into app-level record merges directly; internally the live leaf is `property @l`-lifted with hand-fused chrome (abstract labels can't flow through the merges' `Nub`, so skolem-labeled operands can't merge — and decoration is implementation technique anyway), while all-chrome groups (button content, progress bars) have concrete rows and stay literal `RecordToRecord.do` merges of announcing chrome (`staticText`/`staticHTML`/`pempty` at `{} → {}`); code order = DOM order
- **src/Data/Profunctor/** - `Cont` (CPS profunctor) + the `Row/` layer; everything else was dissolved or deleted
- **src/Data/Profunctor/Row/** - Row profunctors over `Record`/`Variant`. Each of the four direction modules is organized in three layers: **strength** (the unary power — ecosystem `Strong`/`Choice` on the diagonals, module-defined `Resolving`/`Retaining` on the mixed directions) → **direction class** (the binary merge plus its nullary unit `pempty`, the genuine per-carrier primitives) → **free functions over the strength** (everything else — no row-focus classes; laws pinning the unary to the merge are stated in the module headers: identity-pinned on the diagonals, silence-pinned on the mixed directions). Type variables follow the photographic schema: focus `f`, background `b`, shot `s` (`Cons l f b s`), reality `r`.
  - **`RecordToRecord.purs`** (×→×) — merge `recordToRecord`; over `Strong`: `focusRecord` (focus a whole **sub-Record**, background carried same-kind), `property` (the value-level field lens, type-changing), `recordToProperty`/`eliminateProperty` (grow/drop one field), `lensE` (existential `Lens` constructor), `withRecordDefault`/`withRecordOutputDefault`.
  - **`VariantToVariant.purs`** (+→+) — merge `variantToVariant`; over `Choice`: `focusVariant` (focus a whole **sub-Variant**), `case_` (the value-level case prism, type-changing), `caseToVariant` (absorb one input case; pinned to a `Void`-output sink it *eliminates* the case), `prismE`. Introducing a fresh *output* case is the one operation outside `Choice` (gated `left`/`right` can never emit it) — the ×→+ direction has it as `recordToCase`.
  - **`RecordToVariant.purs`** (×→+) — strength `Resolving`/`resolve :: p a b -> p (Tuple a c) (Either b c)` (a loop/iteration step: `Left` = `Done`, `Right` = `Loop`); merge `recordToVariant`; over `Resolving`: `resolveProperty` (hold field `l`, transform the background — input field ↔ output case), `propertyToCase` (single-field focus; background wrapped as output case `w`), `shutterWrap` (sub-Record focus, background wrapped at `w`), the `Shutter` optic with `shutter`/`shutterE`; `recordToCase` (introduce — plain `Profunctor`, `rmap (inj l)`).
  - **`VariantToRecord.purs`** (+→×) — strength `Retaining`/`retain :: p a b -> p (Either a c) (Tuple b c)` (a Mealy/coroutine step: output + next state); merge `variantToRecord`; over `Retaining`: `retainCase` (hold case `l`, transform the background — input case ↔ output field), `caseToProperty` (single-case focus; background wrapped as output field `w`), `caseToRecord` (Mealy reducer — case `l` updates the record, other cases replay it), `reelWrap` (sub-Variant focus, background wrapped at `w`), the `Reel` optic with `reel`/`reelE`. The mixed strengths have **no `(->)` instance** (a stateless function can't loop / retain state) — their instances live on `UI`.
  - **`Row.purs`** (module `Data.Profunctor.Row`) — the shared floor: the row-constraint vocabulary (`InclusiveRows`/`ExclusiveRows`/`DispatchableVariants`) + the `dimap`-only unary **reshapings** (`widenRecordInput`/`narrowVariantInput`/`narrowRecordOutput`/`widenVariantOutput` and single-label forms `widenInputProperty`/`widenOutputCase`/`narrowInputCase`/`narrowOutputProperty`); the merge instances in `UI.purs` build on the two widening ones.
  - **`Row/Example.purs`**, **`test/Main.purs`**, **doc/row-profunctors.md** — a phantom carrier exercising the API shape, `(->)` value-level tests for the diagonal combinators, and the design note.

### Composition Patterns

- `Semigroupoid.do` (qualified-do) - Data flow pipelines
- `RecordToRecord.do` / `RecordToVariant.do` / `VariantToVariant.do` / `VariantToRecord.do` (qualified-do) - the four row merges
- label-indexed components (`MDC.filledTextField @l`, …) - compound widgets are themselves `×→×` merges; they slot into form merges directly, `focusRecord`/`property` nest them into larger aggregates
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
