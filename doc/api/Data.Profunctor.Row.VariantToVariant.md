## Module Data.Profunctor.Row.VariantToVariant

`Variant → Variant` row profunctors: the direction class
`VariantToVariant` — the binary **merge**, the one genuine per-carrier
primitive — with its qualified-do sugar. One-at-a-time events dispatch to
the operand handling their case (`ExclusiveRows` on input: exactly one
handler per case); outputs may overlap (`InclusiveRows`). Over ecosystem
`Choice`: `case_` (the value-level case prism, via `prismE`) and
`splitVariant` (the focus/background dispatch `reelWrap` shares).

The **nullary** operator is the merge **unit** — the class's own
`pempty :: p (Variant ()) (Variant ())`: `variantToVariant pempty g = g`.
Here silence is not merely lawful but forced — both empty-variant ends
are uninhabited, so the unit can neither receive nor emit — and any
silent element implements it (`PUI`: `pempty = silence`).

#### `Coprism`

``` purescript
type Coprism s t a b = forall p. Cochoice p => p a b -> p s t
```

The optic `unleft` induces: the **Coprism** — the prism run backwards
(`Coprism s t a b ≅ Prism b a t s`). Eliminating the residual `c`
(instantiated to `a`) by co-Yoneda collapses `∃c. (s + c → a) × (b → t + c)`
to `(embed : s → a) × (step : b → t + a)`: every input becomes a focus,
and every focus result either exits with `t` or **re-enters as the next
focus input** — `tailRec` at the optic level. Where a prism's residual
passes by visibly in the type, a coprism's circulates hidden as control
flow. `iterate` is this optic at row granularity.

#### `bind`

``` purescript
bind :: forall p i1 i1l i2 i2l o1 o2 o12 o1x o2x i o. VariantToVariant p => OwnedVariantInputs i1 i2 i i1l i2l => SharedVariantOutputs o1 o2 o o12 o1x o2x => p (Variant i1) (Variant o1) -> (p (Variant i1) (Variant o1) -> p (Variant i2) (Variant o2)) -> p (Variant i) (Variant o)
```

#### `coprism`

``` purescript
coprism :: forall s t a b. (s -> a) -> (b -> Either t a) -> Coprism s t a b
```

#### `coprismE`

``` purescript
coprismE :: forall s t a b c. (Either s c -> a) -> (b -> Either t c) -> Coprism s t a b
```

Construct a `Coprism` straight from its **existential encoding**
`∃c. (s + c → a) × (b → t + c)`: pick the looped channel `c`, then supply
`decon` (read a fresh input or a looped value) and `recon` (exit or loop
each emission). `coprism` is this at the co-Yoneda witness `c := a`.

#### `case_`

``` purescript
case_ :: forall @l p f f' b s s' mix. IsSymbol l => Cons l f b s => Cons l f' b s' => Union b mix s' => Choice p => p f f' -> p (Variant s) (Variant s')
```

Focus an existing case in place — the standard `Choice` case prism, read
photographically as **refocusing**: the **focus** `f → f'` changes, the
**background** `b` stays, so the **shot** `s` becomes `s'` (`Union b mix s'`
lets the untouched background `expand` into the new row). `f' := f`
recovers the simple `p f f -> p [ | s ] [ | s ]` form. Built via `prismE`
at `c := [ | b ]`.

#### `VariantToVariant`

``` purescript
class (Profunctor p) <= VariantToVariant p  where
  variantToVariant :: forall i1 i1l i2 i2l o1 o2 o12 o1x o2x i o. OwnedVariantInputs i1 i2 i i1l i2l => SharedVariantOutputs o1 o2 o o12 o1x o2x => p (Variant i1) (Variant o1) -> p (Variant i2) (Variant o2) -> p (Variant i) (Variant o)
  pempty :: p (Variant ()) (Variant ())
```

#### `discard`

``` purescript
discard :: forall p i1 i1l i2 i2l o1 o2 o12 o1x o2x i o. VariantToVariant p => OwnedVariantInputs i1 i2 i i1l i2l => SharedVariantOutputs o1 o2 o o12 o1x o2x => p (Variant i1) (Variant o1) -> (Unit -> p (Variant i2) (Variant o2)) -> p (Variant i) (Variant o)
```

#### `iterate`

``` purescript
iterate :: forall p done again out. Cochoice p => ExclusiveRows done again out => Contractable out done => Contractable out again => p (Variant again) (Variant out) -> p (Variant again) (Variant done)
```

The `+`-diagonal **trace** at row granularity, over ecosystem `Cochoice`:
loop the `again` cases of the output back into the input, emit only the
`done` cases — **iteration** (retry/wizard flows). `splitVariant` is the
done/again dispatch. Unit law: at `again = ()` (no loop-back cases) the
widget is unchanged. On `PUI` the re-entry is a `toUser`, so the loop
advances on the widget's next emission — an event loop, not a busy loop.

#### `prismE`

``` purescript
prismE :: forall s t a b c. (s -> Either a c) -> (Either b c -> t) -> Prism s t a b
```

Construct a `Prism` straight from its **existential encoding**
`∃c. (s → a + c) × (b + c → t)`: pick the residual `c`, then supply `decon`
(match `s` as the focus `a` or the complement `c`) and `recon` (rebuild `t`
from the built `b` or that same complement `c`). The quantified `c` is the
eliminator of that existential; `left` (`Choice`) is the carrier. The standard
`Data.Lens.prism` is this at the co-Yoneda witness `c := t`.

#### `splitVariant`

``` purescript
splitVariant :: forall f b s. ExclusiveRows f b s => Contractable s f => Contractable s b => Variant s -> Either (Variant f) (Variant b)
```


