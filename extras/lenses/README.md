# extras/lenses — complements of `profunctor-lenses`

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
[`extras/profunctor`](../profunctor/README.md).

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

## Upstreaming: PRs to `purescript-contrib/purescript-profunctor-lenses`

The table above already splits the root by eligibility; it splits into **two
PRs**. What follows is what the upstream repo actually requires — checked
against the live repo (2026-08; default branch `main`, maintainer garyb) and
the [purescript-contrib governance](https://github.com/purescript-contrib/governance)
docs — and what this code must change to comply.

- **PR 1 — eligible now**: `Colens` + `Coprism` (+ the `prismE` question).
  Their generating classes (`Costrong`/`Cochoice`) have been upstream in core
  `purescript-profunctor` since 2016 — the situation is exactly `Grate`'s,
  which was built on the pre-existing `Closed`
  ([PR #60](https://github.com/purescript-contrib/purescript-profunctor-lenses/pull/60)).
- **PR 2 — blocked**: `Shutter`/`Coshutter`/`Reel`/`Coreel`, blocked on
  their classes landing in a **released** `purescript-profunctor`
  (see [`extras/profunctor`](../profunctor/README.md)). One escape hatch is
  worth raising in the proposal issue: `Wander` is precedent that a
  profunctor **class can live in profunctor-lenses itself**
  (`Data.Lens.Internal.Wander`), so the four coined classes could ship with
  their optics in one contrib PR, sidestepping core review entirely — at the
  cost of the classes being invisible to non-lens users.

### Process

- **Open a proposal issue first.** The org contributing guide says large
  changes should be proposed before implementation, and both precedents
  agree: `Grate` (PR #60) closed a pre-existing proposal issue (#18);
  affine traversals were proposed in PR #80, discussed, closed, and revived
  as the merged [PR #112](https://github.com/purescript-contrib/purescript-profunctor-lenses/pull/112).
- **Two approvals** for non-trivial changes; all PRs must pass CI.
- **PR template checklist**: changelog `Unreleased` entry with PR link and
  username, linked proposal issue, README/`docs/` updates, tests
  *(if applicable)*.

### Structural changes required — house layout is not this layout

The upstream repo distributes an optic across three places; these modules
keep everything in one file, so each must be split:

- [ ] **Type synonyms move to `Data.Lens.Types`** — the optic module keeps
  only functions and re-exports `module Data.Lens.Types`. Concretely:
  `type Colens s t a b = forall p. Costrong p => Optic p s t a b` (stated via
  the house `Optic` synonym, not longhand `p a b -> p s t`), plus the primed
  `Colens' s a = Colens s s a a` — every upstream optic has both.
- [ ] **Each optic ships a concrete carrier in `Data.Lens.Internal/`** and an
  `A`-prefixed monomorphic synonym over it (`ALens = Optic (Shop a b)`,
  `APrism = Optic (Market a b)`; PR #60 added `Grating`, PR #112 added
  `Stall`). A `Colens` PR needs the co-Shop (the collapsed pair
  `(s -> b -> a) × (b -> t)` as a newtype with a `Costrong` instance),
  a `Coprism` PR the co-Market — these carriers **do not exist here yet**
  and are the main new code to write. They are what powers the house
  eliminator/clone pattern:
- [ ] **Function families follow the house naming**: `colens`, `colens'`,
  `withColens :: AColens s t a b -> ((s -> b -> a) -> (b -> t) -> r) -> r`,
  `cloneColens :: AColens s t a b -> Colens s t a b` — and the same four for
  `Coprism`. The `*E` free-residual constructors (`colensE`, `coprismE`) are
  bambik's convention, **not** upstream's: the house existential mechanism is
  the concrete carrier + `with*` eliminator, so keeping the `*E` functions is
  a departure to flag explicitly in the proposal issue (they are genuinely
  more general — the free `c` — so they may survive, but under review).
- [ ] **`prismE` is not a new module upstream.** `Data.Lens.Prism.Existential`
  exists here only because an external package cannot add exports to
  `Data.Lens.Prism`; in-repo it becomes one new export **of**
  `Data.Lens.Prism` (name subject to the same `*E`-convention discussion),
  and the module here dissolves. Also its `import Data.Lens (Prism)` would
  be an import cycle in-repo — the in-repo form imports `Data.Lens.Types`.
- [ ] **Decide the `Data.Lens` re-export.** Precedent is split: `Grate` is
  re-exported from `Data.Lens`, `AffineTraversal` is not. Ask in the issue.
- [ ] **Anticipate the `Re` question.** `Data.Lens.Internal.Re` already has
  `Costrong p => Strong (Re p s t)` (and the `Cochoice`/`Choice` pair), so a
  reviewer can point out that "the lens run backwards" is already reachable
  as `re` of a `Lens`. The answer the proposal should carry: `re` gives the
  *transformation*, not the *citizen* — a first-class `Colens s t a b` with
  constructors, eliminator and laws is to `re`-of-a-lens what `Review` is to
  `re`-of-a-prism, and `feedback`-style consumers need to *demand*
  `Costrong` in a signature, which only a named synonym states.

### Code-style and mechanics requirements

- [ ] **purs-tidy is mandatory** — CI runs `purs-tidy check src test examples`
  with the repo's `.tidyrc.json` (2-space indent, source-ordered imports,
  arrows-first type wrapping, no unicode). Reformat everything with that
  config.
- [ ] **Warning-clean on stock purs**: CI is
  `spago build --purs-args '--censor-lib --strict'` against the **unstable**
  compiler, plus a bower/pulp verification step (the repo still ships
  `bower.json`). Nothing here uses bambik's forked compiler, so this is
  formatting-level work only.
- [ ] **Rewrite module docs carrier-neutral**: the current headers name
  `PUI`, gating, `Data.Profunctor.Row.*` and the `extras/` roots. The house
  style to imitate is `Prism.purs`: a tutorial-shaped header with a worked
  example and the optic's **laws stated** (`Grate`'s one-liner-plus-citation
  is the floor, not the target). The co-Yoneda collapse narrative and
  `Colens s t a b ≅ Lens b a t s` belong; bambik's row forms don't.
- [ ] **Tests**: assert-based value-level tests appended to `test/Main.purs`.
  For `Colens`/`Coprism` this needs a lawful `Costrong`/`Cochoice` carrier to
  test against — `Re` over `(->)`-based optics, or the new internal carriers
  themselves.
- [ ] **Changelog**: `[Unreleased]` → **New features**, format
  `- Add \`Colens\` and \`Coprism\` (#NN by @user)`.
- [ ] **`docs/` update**: the org library guidelines require the docs
  directory to teach; `docs/Impredicativity.md` is directly relevant (it
  explains exactly the `Lens`/`ALens` split that `Colens`/`AColens`
  reproduces) and should gain the new optics.
- [ ] **Dependencies**: none to add — contrib libraries may depend only on
  core or contrib packages, and these modules import only `profunctor` and
  the lens package itself. `spago.dhall` and `bower.json` stay untouched.
- [x] **Licensing**: upstream is **MIT** (unlike core `profunctor`, which is
  BSD-3-Clause), no CLA. Bambik itself is BSD-3-Clause (see the repo's
  `LICENSE`, Copyright Restaumatic) — no obstacle: the copyright holder is
  the submitter, so granting MIT for the contributed code is theirs to do
  (GitHub's inbound-=-outbound default covers it on submission).

### Sequencing

1. Proposal issue on `profunctor-lenses`; PR 1 (`Colens`/`Coprism`, plus the
   `prismE` export) can follow immediately.
2. PR 2 waits on `extras/profunctor`'s classes reaching a released
   `purescript-profunctor` — or goes the `Wander` route with classes and
   optics in one contrib PR, if the maintainer prefers it in the proposal
   discussion.
