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

## Upstreaming: a PR to `purescript/purescript-profunctor`

The goal for the four class modules is a single PR to
[`purescript/purescript-profunctor`](https://github.com/purescript/purescript-profunctor),
batched exactly as [PR #13](https://github.com/purescript/purescript-profunctor/pull/13)
batched `Closed`, `Costrong` and `Cochoice` in 2016. What follows is what the
upstream repo and its governance actually require, checked against the live
repo (2026-08), and what this code must change to be eligible. `Cont.purs`
is **not part of the PR** — it is a carrier, it imports bambik's row algebra,
and it stays here.

### Process — how core libraries accept changes

`purescript-profunctor` is a **core library** (purescript org), governed by
[purescript/governance](https://github.com/purescript/governance):

- **Two approving reviews**: two core team members, or one core team member
  plus one core-libraries collaborator. Participating core members hold veto
  authority; silence for a month after a first core-owner approval counts as
  consent. The repo moves slowly (a handful of PRs since 2021) — plan for
  months, not days.
- **No CONTRIBUTING.md exists**; the process files are the PR template and
  CI. The [PR template](https://github.com/purescript/purescript-profunctor/blob/master/.github/PULL_REQUEST_TEMPLATE.md)
  checklist: changelog entry in the **Unreleased** section with a PR
  reference, linked issue/proposal, documentation updated, test *(if
  applicable — the repo has no test suite at all)*.
- **Open an issue first.** Not formally mandated anywhere, but it is the
  precedent that matters: [issue #2 "Add Loop"](https://github.com/purescript/purescript-profunctor/issues/2)
  (paf31 proposing a class, with candidate laws and Haskell precedent)
  preceded PR #13 — and #2 itself, with its laws left unresolved, sat five
  years and was closed *without* being added. A class proposal with unsettled
  laws does not land.

### The case to make — and the one filter that can reject it

Governance states the acceptance philosophy verbatim: *"If it is possible to
adequately solve a need downstream of the compiler and/or core libraries, we
are unlikely to add features for solving that need inside."* This root is
living proof the classes *can* live downstream — so that argument alone
loses. The winning argument is the one PR #13 made for `Costrong`/`Cochoice`:
these are the **missing complements of an existing family** (the four corners
of strength: `Strong` ×→×, `Choice` +→+, and the two mixed diagonals ×→+ and
+→×), and a concrete downstream consumer needs them to exist upstream —
PR #13 cited profunctor-lenses' `re`; this PR cites the `Shutter`/`Reel`
optic family in [`extras/lenses`](../lenses/README.md), which cannot go to
`profunctor-lenses` while its generating classes live in an application repo.

Two honest risks to state in the proposal issue rather than have discovered
in review:

1. **No Haskell precedent.** Haskell's `profunctors` has no counterpart of
   these classes (its `Traversing`/`Mealy` machinery is adjacent, not the
   same shape). `Closed`/`Costrong`/`Cochoice` all had Haskell siblings to
   point at; these don't. The mitigation is the symmetry argument — the
   2×2 square is visibly incomplete without them — plus the laws.
2. **No non-degenerate ecosystem instance** (see *Instances* below). This is
   the largest gap. `Costrong`/`Cochoice` also shipped instance-less, and
   [issue #46](https://github.com/purescript/purescript-profunctor/issues/46)
   shows the maintainers accept "impossible under strict evaluation" as an
   answer — but that precedent is 2016, and a 2026 reviewer may reasonably
   ask what, in the registry, will ever instantiate these. The honest answer:
   stateful/timeful carriers (bambik's `PUI` is one), which by nature live
   downstream — the same answer `Costrong` gives today.

### Code changes required before the PR

- [ ] **Decide the second positional method.** Every upstream strength class
  states both variants — `first`/`second`, `left`/`right`,
  `unfirst`/`unsecond`, `unleft`/`unright` — while these four have one method
  each. The sibling (`p a b -> p (Tuple c a) (Either c b)` for `resolve`,
  etc.) is `dimap`-derivable via swap, but PureScript classes have no default
  methods, so upstream symmetry means every instance implements both.
  Either add the four siblings (and pick their names) or argue the
  single-method shape in the proposal issue.
- [ ] **Rewrite every doc comment carrier-neutral.** The current headers name
  `PUI`, `Data.Profunctor.Row.*`, `Data.Lens.Shutter`, the `extras/` roots,
  quiescence and gating — none of which exist upstream. Keep the Loop/Done
  and Mealy-step intuitions; restate them in the house style of
  `Strong`'s docs (plain intuition, then the signature specialized to a
  familiar shape). The "no `(->)` instance because…" notes survive, recast
  as strictness/statelessness arguments like issue #46's.
- [ ] **State the laws equationally.** The retraction laws
  (`coresolve (resolve g) = g`, `coretain (retain g) = g`) plus naturality
  of the state channel `c`, written as class-doc bullets exactly as
  `Data.Profunctor`'s own Identity/Composition laws are. "≅ up to time,
  once primed" is carrier talk and cannot appear. Issue #2 is the cautionary
  tale: unsettled laws are what kept `Loop` out.
- [ ] **Inventory instances for the in-repo carriers.** The package ships
  `Star`, `Join`, `Split`. For each of the four classes, either supply a
  lawful instance or record the impossibility in the proposal (as issue #46
  did for `Costrong (->)`), so that "no instances" reads as a stated fact
  about strict/stateless carriers, not an omission. `Cont.purs`'s inventory
  here (degenerate `Resolving`/`Coretaining`, impossible
  `Coresolving`/`Retaining` for CPS) is the raw material.
- [ ] **Match house mechanics**: modules currently carry export lists —
  upstream modules are bare `module X where` (either passes CI; the style
  guide prefers explicit exports, so keeping them is defensible). No
  formatter config exists — match the existing style by hand. CI builds
  with `pulp build -- --censor-lib --strict` against the **unstable**
  compiler: warnings are errors, so the modules must be warning-clean on
  stock purs (they are today — no forked-compiler feature is used here).
- [ ] Optional but pattern-completing: the binary derived combinators
  (`Strong` ships `splitStrong`/`***` and `fanout`/`&&&` beside the class;
  the analogues here are the positional forms of the row merges,
  `p a b -> p c d -> p (Tuple a c) (Either b d)` and its `+→×` dual).

### PR mechanics

- [ ] `CHANGELOG.md` → `[Unreleased]` → **New features**, entry format
  ``- Add `Resolving`/`Coresolving`/`Retaining`/`Coretaining` (#NN by @user)``.
- [ ] No test suite exists; the template's test box is "(if applicable)".
- [ ] `bower.json` untouched — the four modules import only `Data.Either`,
  `Data.Tuple`, `Data.Profunctor`, all already in the dependency set.
- [x] **Licensing**: upstream is BSD-3-Clause, no CLA. Bambik itself is
  BSD-3-Clause (see the repo's `LICENSE`, Copyright Restaumatic), so the
  contribution's provenance is clean — same license both sides; the
  copyright holder submits.

### Sequencing

This PR gates the second half of [`extras/lenses`](../lenses/README.md): the
`Shutter`/`Coshutter`/`Reel`/`Coreel` optics can only go to
`profunctor-lenses` after these classes land in a **released**
`purescript-profunctor` (or via the `Wander` escape hatch — see that
README). `Colens`/`Coprism` are independent: their classes are upstream
already.
