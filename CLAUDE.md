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
- **src/Data/Profunctor/Row/** + **src/Data/Profunctor/Row.purs** - Row profunctors over `Record`/`Variant`. Each of the four direction modules stacks three layers (merge → unary strength → single-field combinator(s)):
  - **`RecordToRecord.purs`** (×→×) — merge `recordToRecord`; row-typed `Strong` `StrongRecordToRecord`/`focusRecord` (focus a whole **sub-Record** carrying the complement; generic `instance Strong p => StrongRecordToRecord p`, so `UI` gets it); single-field combinators `introduceProperty`/`eliminateProperty`/`editProperty` (`editProperty` = the value-level field lens).
  - **`VariantToVariant.purs`** (+→+) — merge `variantToVariant`; row-typed `Choice` `ChoiceVariantToVariant`/`focusVariant` (focus a whole **sub-Variant**; generic `instance Choice p => ChoiceVariantToVariant p`); single-case combinators `eliminateCase`/`editCase` (`editCase` = the value-level case prism). (Introducing a *fresh* variant case is the one operation outside `Choice`; no dedicated combinator — built via the `Sum`/`variantToVariant` path.)
  - **`RecordToVariant.purs`** (×→+) — merge `recordToVariant`; unary product→sum strength `IteratingRecordToVariant`/`iterating` (a loop/iteration step, `Either b c` = `Done`/`Loop`); single-field combinator `iterateProperty` (threads one label as input field ↔ output case).
  - **`VariantToRecord.purs`** (+→×) — merge `variantToRecord`; unary sum→product strength `ResumingVariantToRecord`/`resuming` (a Mealy/coroutine step, `Tuple b c` = output + next state); single-field combinator `resumeCase` (input case ↔ output field). The mixed strengths have **no `(->)` instance** (a stateless function can't loop / retain state), and unlike the diagonals' `focus` they thread the residual across the product/sum boundary with a *mode change* (× ↔ +) rather than carrying a same-kind complement.
  - **`Row.purs`** (module `Data.Profunctor.Row`) — the umbrella `Row` aggregator class + unary row reshapings: `Union`-based `widenRecordInput`/`narrowVariantInput`/`narrowRecordOutput`/`widenVariantOutput` and their single-field/case forms `widenInputProperty`/`widenOutputCase`/`narrowInputCase`/`narrowOutputProperty`.

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
