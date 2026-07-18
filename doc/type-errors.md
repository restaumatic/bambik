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
duplicated label, spanned at an offending operand line:

```
at demo/7guis/counter/Counter.purs:16:9 - 16:68

  Custom error:

    Two merge operands own the label "name".
    On an owned merge side each label belongs to exactly one operand: every
    record-output field has ONE producer, every variant-input case has ONE
    handler.
    Look for the duplicated `asField`/`field`/`forCase` label in this `do`
    block. (doc/type-errors.md #2)
```

**Read it as:** exactly what it says — the named label has two producers
(or two handlers); remove or rename one of them.

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

## Rules of thumb

- The *left* row in a `Could not match` is usually what the widget/adopter
  claims; the *right* row is what the model or handler actually offers.
- Errors surface at the **boundary** where rows are pinned (`mvu` seed,
  `completed`, `updates` handler), not at the leaf that caused them — start
  from the reported span, then check the labels the leaf declares.
- `Lacks` failures mean duplication; `Union`/`Nub` failures mean mismatch.
