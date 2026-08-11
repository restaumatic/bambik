# extras/optics — complements of `profunctor-lenses`

Modules here claim names in **`Data.Lens.*`**, the namespace of
[`purescript-profunctor-lenses`](https://pursuit.purescript.org/packages/purescript-profunctor-lenses),
because they belong in that library's family — beside `Data.Lens.Lens` and
`Data.Lens.Prism` — and follow its layout: **one optic per module**, each
carrying its type, its collapsed constructor and its `*E` existential
encoding, at arbitrary `s t a b`.

Nothing here mentions `PUI`, a row, or a carrier. Every optic is
`p a b -> p s t` quantified over its generating class, so the row layer's
combinators are these optics at row granularity — `feedback` a `Colens`,
`iterate` a `Coprism`, `folding` a `Coshutter`, `unfolding` a `Coreel`,
`subResolving`/`subRetaining` a `Shutter`/`Reel`.

Optics follow from their generating classes by **Pastro–Street**, so what
had to be invented here is the actions, not the technique.

| Module | Optic | Generating class | Upstreamable alone? |
|---|---|---|---|
| `Data.Lens.Colens` | `Colens` — the lens run backwards | `Costrong` (ecosystem) | **yes** |
| `Data.Lens.Coprism` | `Coprism` — the prism run backwards | `Cochoice` (ecosystem) | **yes** |
| `Data.Lens.Shutter` | `Shutter` — a lens that can snap shut | `Resolving` (coined) | travels with its class |
| `Data.Lens.Coshutter` | `Coshutter` — the fold state as a reader | `Coresolving` (coined) | travels with its class |
| `Data.Lens.Reel` | `Reel` — a wound transport that never finishes | `Retaining` (coined) | travels with its class |
| `Data.Lens.Coreel` | `Coreel` — a generator, producing every step | `Coretaining` (coined) | travels with its class |

`Colens` and `Coprism` are the purest complements of the six: their classes
are **already** the ecosystem's (`Data.Profunctor.Costrong`/`.Cochoice`) —
`profunctor-lenses` simply never built the optics. The other four are coined
class and optic alike, so each would travel upstream with its class from
[`extras/profunctors`](../profunctors/README.md).

`Data.Lens.Prism.Existential` is the purest complement in the whole tree:
both the optic and its `Choice` are already `profunctor-lenses`', and only
the one constructor `prismE` is missing. It **extends** that family rather
than shadowing it, which is why it is not named `Data.Lens.Prism`.

## Building

Covered by the single glob `extras/**/*.purs` in the repo's `spago.dhall`.
An application consuming bambik as a spago git package needs that root in its
own `sources` too — spago globs a git dependency as
`.spago/<pkg>/<ver>/src/**/*.purs`, hardcoded, so modules outside `src/` are
otherwise never compiled. See the skill's `bootstrap.md`.
