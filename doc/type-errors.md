# Reading row-layer type errors

The row combinators lean on `Prim.Row` constraints (`Union`, `Nub`, `Cons`,
`Lacks`), which cannot carry custom error messages — a failed constraint
surfaces as the compiler's own row-unification report. The reports are
usable once you know the three archetypal mistakes and where each one
surfaces. All outputs below are real, reproduced on the 7GUIs demos.

## 1. Label typo in an adopter

`asField @l` / `forField @l` build a closed singleton row; a typo'd label
fails where that singleton meets the model row — at `completed`, `mvu`,
or the seed — not at the adopter itself.

```
MDC.headline4 (HTML.text # projection show # forField @"cuont") # completed
```

```
at demo/7guis/counter/Main.purs:15:73 - 15:82

  Could not match type
    ( cuont :: t4 ... | t3 )
  with type
    ( count :: Int ... )
```

**Read it as:** the label on the left is what you wrote, the row on the
right is the model — scan the left row for the typo. The source span points
at the adopter application.

## 2. Two operands owning the same output field

Record-merge output fields are **owned**: exactly one producer per field
(`OwnedRecordOutputs`); dually, variant-merge input cases have exactly one
handler (`OwnedVariantInputs`). Giving two operands the same `asField`
label (or two handlers the same case) is caught by the owned sides'
`DisjointLabels` detector, which fails with a custom error naming the
duplicated label **and both operands' complete label sets**, spanned at an
offending operand line:

```
at demo/7guis/crud/Crud.purs:44:19 - 44:99

  Custom error:

    Two merge operands own the label "surname".
    One operand owns { surname }, the other { name, surname }.
    On an owned merge side each label belongs to exactly one operand: every
    record-output field has ONE producer, every variant-input case has ONE
    handler.
    Look for the duplicated `asField`/`field`/`forCase` label in this `do`
    block. (doc/type-errors.md #2)
```

The same defect can also surface *within* one operand's inferred row (the
`do` block's tail unified against a pinned total row); the within-row
detector then renders the whole offending row:

```
  Custom error:

    A merge operand's row owns the label "surname" twice.
    The row is { surname, surname }.
    ...
```

Unlike #1 and #3, this error is self-explanatory — this section exists as
its citation target and for the two-operand vs within-row distinction.

## 3. Missing (or extra) case handler

An `updates` fold (or a `VariantToVariant.do` dispatch) must handle every
case its event stage can emit. A missing handler in the `Variant.match`
record shows up as the emitted variant row failing to embed in the handled
one:

```
Could not match type
  ( picked :: Int ... | t11 )
with type
  ( create :: {...}, delete :: {...}, update :: {...} )
```

**Read it as:** the left row lists a case that is emitted but not handled;
the right row is what your handler record covers. Add the missing case (or
remove the stale emitter).

## Runtime sibling: gate starvation

Not every wiring mistake is a type error. A knowledge gate that is never
primed (a `looped`/`feedback`/`folding` state channel with no seed, a merge
operand that never emits) typecheck fine and renders as a **blank screen**.
Every gate carries a starvation watchdog for exactly this: if it withholds
and is never fed within 3 seconds, one `console.warn` names the gate, the
missing fields (for record merges), and the fix (`with`/`announce`/
`seeded`). The full emission trace is `window.__bambikTrace = true`; the
warnings alone need no flag (opt out: `window.__bambikNoWarn = true`).

## Rules of thumb

- The *left* row in a `Could not match` is usually what the widget/adopter
  claims; the *right* row is what the model or handler actually offers.
- Errors surface at the **boundary** where rows are pinned (`mvu` seed,
  `completed`, `updates` handler), not at the leaf that caused them — start
  from the reported span, then check the labels the leaf declares.
- `Lacks` failures mean duplication; `Union`/`Nub` failures mean mismatch.
