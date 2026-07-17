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
(`OwnedRecordOutputs`). Giving two operands the same `asField` label fails
the `Lacks` constraint of the merge's exactness evidence:

```
No type class instance was found for

  Prim.Row.Lacks "name" ( name :: String )

while solving type class constraint
  Data.Profunctor.Row.FieldNames t11 t8 t8
...
while applying a function discard
```

**Read it as:** `Lacks "x" ( x :: _ )` = the field `x` appears twice on the
merge's output side — two operands both claim to produce it. The span covers
the whole `RecordToRecord.do` block; look for the duplicated label.

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
