# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Bambik is a prototype PureScript library implementing **Profunctor User Interfaces** - a novel approach to declarative Web UI development using Material Design Components. The key insight is that profunctors unify optics (data structure navigation) and arrows (data flow), making them ideal for composable UI development.

## Build Commands

Do `export PATH=$PATH:/node_modules/.bin` and then `spago build`.

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

- **src/UI.purs** - Core UI profunctor type with all profunctor class instances (Strong, Choice, Endo, Sum, Zero)
- **src/Web.purs** - DOM monad (`Web = StateT DOM Effect`) and primitive elements (`text`, `input`, `button`, `div`, etc.)
- **src/MDC.purs** - Material Design Component wrappers as oculars
- **src/Data/Profunctor/** - Profunctor building blocks (Endo, Sum, Zero, Product, ProductToSum, etc.)
- **src/Data/Profunctor/HalfOptic/** - Single-field/single-case "half-optics", organized as the 2×2×pin (product/sum × introduce/eliminate, + edit/focus) from `doc/row-to-row-vs-half-lens.md`. The **product** row (`Property.purs`: `introduceProperty`, `eliminateProperty`, `edit`) is just `Strong` (`first`/`second`), so it works on `UI`. On the **sum** row (`Case.purs`), `eliminateCase` and `focusCase` fold onto `Choice` (`left`/`right`); only `introduceCase` keeps a dedicated class `IntroVarP` (`IntroVarP.purs`), which is incomparable to `Choice` because its source emits a case the input never carries. `edit`/`focusCase` reuse the lens/prism in `src/Data/Lens.Extra/Commons.purs`.
- **src/Data/Profunctor/RowToRow/** - The n-ary, labeled counterpart of the half-optics: `RecordToRecord`/`RecordToVariant`/`VariantToRecord`/`VariantToVariant` binary row merges.

### Composition Patterns

Uses PureScript's qualified-do for different composition styles:

- `Endo.do` / `Form.do` - Record-like structures (multiple fields)
- `Sum.do` / `A.do` - Alternatives/variants
- `Semigroupoid.do` / `Flow.do` - Data flow pipelines

### Separation of Concerns

- **Business Logic** - Optics (Lens, Prism) in model files (e.g., demo/1/Model.purs)
- **Design System** - Oculars in Web.purs and MDC.purs
- **Composition** - UI elements compose orthogonally to optics

## Demo Structure

- **demo/1/** - Full MDC-based order form (complex nested forms, conditional sections)
- **demo/2/** - Plain HTML demo
- **demo/helloworld/** - Simple intro example

## Key Dependencies

- `profunctor-lenses` - Profunctor-based optics
- `qualified-do` - Syntax sugar for profunctor composition
- `material-components-web` - MDC JavaScript library
