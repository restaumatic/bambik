# extras/profunctor — complements of `purescript-profunctor`

Modules here claim names in **`Data.Profunctor.*`**, the namespace of
[`purescript-profunctor`](https://pursuit.purescript.org/packages/purescript-profunctor),
because they belong in that library's family — beside `Strong`, `Choice`,
`Costrong` and `Cochoice` — and follow its layout: **one class per module**,
with duals split into separate files exactly as `Strong` and `Costrong` are.

Nothing here mentions `PUI`, a row, or a carrier. Each class is stated
**positionally** (`Tuple`/`Either`) like the ecosystem's own, so the four
strength classes could be lifted upstream unchanged.

| Module | Class | Ecosystem sibling |
|---|---|---|
| `Data.Profunctor.Resolving` | `Resolving` — `p a b -> p (Tuple a c) (Either b c)` | the `× → +` analogue of `Strong` |
| `Data.Profunctor.Coresolving` | `Coresolving` — its retraction, a terminating fold | the `× → +` analogue of `Costrong` |
| `Data.Profunctor.Retaining` | `Retaining` — `p a b -> p (Either a c) (Tuple b c)` | the `+ → ×` analogue of `Choice` |
| `Data.Profunctor.Coretaining` | `Coretaining` — its retraction, a productive unfold | the `+ → ×` analogue of `Cochoice` |

The mixed shapes' background *crosses* carriers, which is not a Tambara
action — hence the coinage, and hence `PUI m` instances but no `(->)`:
`resolve` needs quiescence (time), `retain` needs memory (state).

`Data.Profunctor.Cont` is the root's one member that is **not** liftable: a
carrier rather than a class, the CPS profunctor `Cont r a b = (b -> r) -> (a -> r)`
whose header inventories which of the library's classes it validly inhabits
and which it provably cannot. Those impossibilities are why the trace forms
take seeds and why `looped` is a primitive.

The optics these classes generate live one root over, in
[`extras/lenses`](../lenses/README.md). The row-granularity combinators built
on them live in [`extras/row-profunctor`](../row-profunctor/README.md).

## Building

Covered by the single glob `extras/**/*.purs` in the repo's `spago.dhall`.
An application consuming bambik as a spago git package needs that root in its
own `sources` too — spago globs a git dependency as
`.spago/<pkg>/<ver>/src/**/*.purs`, hardcoded, so modules outside `src/` are
otherwise never compiled. See the skill's `bootstrap.md`.
