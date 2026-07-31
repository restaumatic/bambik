# Variant syntax sugar (a forked compiler)

bambik does **not** build with a stock PureScript compiler. It pins a **fork of
`purs`** — the `purescript` dependency in [`package.json`](../package.json),
version `0.15.16-variant.6` — which adds first-class syntactic sugar for
**Variants**, the exact dual of the record sugar (`{ … }`, `_.label`) that stock
PureScript already gives products.

Variants are how bambik models event/output channels (sum types — see
[row-profunctors.md](./row-profunctors.md)), so they appear everywhere; the fork
makes writing them as ergonomic as records. Everything below is purely
**syntactic** — it desugars to the same `Prim.Variant.Variant` type and the same
runtime representation the [`purescript-variant`](https://github.com/natefaubion/purescript-variant)
library uses, so library functions (`inj`, `on`, `match`, `expand`, `contract`)
interoperate freely with the sugar.

The fork adds a new builtin `Prim.Variant.Variant :: Row Type -> Type` (the dual
of `Prim.Record`), implicitly in scope, plus three sugar forms.

## 1. Type sugar — `[ … ]`

`[ … ]` is sugar for `Variant ( … )`, mirroring `{ … }` for `Record ( … )`. No
import is needed, and it is **two-way**: variant types are pretty-printed back as
`[ … ]` in errors, docs, and the REPL.

| stock | sugar |
|---|---|
| `Variant ( a :: X, b :: Y )` | `[ a :: X, b :: Y ]` |
| `Variant ( a :: X \| r )` | `[ a :: X \| r ]` |
| `Variant ()` | `[]` |
| `Variant r` (tail only) | `[ \| r ]` |

```purescript
closed :: [ a :: Int, b :: String ] -> Int
open   :: forall r. [ a :: Int | r ] -> Int
empty  :: [] -> Int
```

Nesting works as expected: `[ foo :: [ bar :: String ] ]`.

## 2. Constructor sugar — `.label`

`.label x` builds an (open) variant carrying `x` in case `label`; bare `.label`
is the injector **function** — the value-level dual of the record accessor
section `_.label`.

| stock | sugar |
|---|---|
| `inj (Proxy :: _ "ok") 42` | `.ok 42` |
| `\x -> inj (Proxy :: _ "ok") x` | `.ok` |

```purescript
closed :: Variant ( ok :: Int, err :: String )
closed = .ok 42

mkOk :: forall r. Int -> Variant ( ok :: Int | r )
mkOk = .ok

mapped :: Array (Variant ( ok :: Int | () ))
mapped = map (.ok) [1, 2, 3]          -- injector section, first-class

nested = .foo.bar "abc"               -- dot-chain: .foo (.bar "abc")
```

## 3. Pattern sugar — `case _ of .label binder ->`

`.label binder` matches one case and binds its payload — the elimination dual of
the `.label` injector, and the variant analogue of pattern-matching a data
constructor. Dot-chains nest (`.foo.bar n`).

```purescript
describe :: Variant ( ok :: Int, err :: String ) -> String
describe v = case v of
  .ok n  -> "ok:" <> show n
  .err e -> "err:" <> e
  _      -> "?"                        -- catch-all REQUIRED (see below)

unwrapNested v = case v of
  .foo.bar n -> n
  _          -> 0
```

> **A `_` catch-all is required.** Exhaustiveness checking is not yet
> type-directed for variants — the checker runs before type-checking and cannot
> see the scrutinee's row — so even a "complete" match needs a `_`/`Partial`
> fallback to avoid a partiality warning.

## What this codebase uses

| form | adopted | where |
|---|---|---|
| Type `[ … ]` | **project-wide** | every variant type in `src/` and `test/`; the `Variant` type *name* is no longer written anywhere — it comes from the `Prim.Variant` builtin |
| Constructor `.label` | **concrete code only** | `inj (Proxy @"lit") x` → `.lit x` in `test/Main.purs`, `test/RestaurantReel.purs` |
| Pattern `case _ of .label` | **not used** | concrete eliminators stay as total `case_ # on …` (see below); the library is label-polymorphic |

The constructor and pattern forms need a **literal** label, so they have no site
in the row-profunctor library itself — it constructs and eliminates variants
through `inj (Proxy @l)` / `on (Proxy @l)` with a type-variable `l` (writing
`.l` there would mean the *literal* field `"l"`). They apply only to concrete
code: the tests above, and downstream business models and demos.

Record sugar (`{ … }`, `{ | r }`, `{}`, `_.label`) used throughout the codebase
is **stock PureScript**, not part of the fork; the variant forms above are the
dual additions the fork contributes.

## Does the fork remove the need for `purescript-variant`?

**No.** The fork is purely *syntactic*: the type sugar `[ … ]` only abbreviates
the `Prim.Variant.Variant` type constructor, and the value sugar desugars to the
same runtime representation. None of the runtime machinery the library code
relies on — `inj`, `on`, `case_`, `expand`, `contract`, `class Contractable`,
`class VariantTags` — has any sugar equivalent, so `Data.Variant` stays a
dependency (imported across `Data.Profunctor.Row` and the
`Data.Profunctor.Row.*` direction modules).

The value sugar can replace `inj`/`on`/`case_` **only where the label is a
syntactic literal**, which rules out every site in the row-profunctor library
(all label-polymorphic — `inj (Proxy @l)` with a `forall l`; writing `.l` there
would mean the *literal* field `"l"`, not the type variable). Two further
caveats apply even in concrete code:

- **Construction is a clean swap:** `inj (Proxy @"foo") x` → `.foo x`. Done in
  the test suite (`test/Main.purs`, `test/RestaurantReel.purs`).
- **Total elimination is *not* worth swapping:** `case_ # on … # on …` over a
  closed row is checked exhaustive by the type system (it must consume the row
  down to `Variant ()`). The pattern sugar can't reproduce that — it *requires* a
  `_` catch-all (§3), which adds a dead branch and silently swallows a forgotten
  case. So the remaining `case_`/`on` eliminators in `demo/nguis/order-form-mdc2/OrderForm.purs`
  (`methodText`, `summarize`), `test/RestaurantReel.purs`, and
  `test/EntityEventExample.purs` are kept deliberately.
