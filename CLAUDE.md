# Bambik

## Overview

A prototype PureScript library implementing **Profunctor User Interfaces** - a novel approach to declarative UI development. The key insight is that profunctors unify optics (data structure navigation) and arrows (data flow), making them ideal for composable UI development.

**CHANGELOG.md holds the history**: what was added, tried, withdrawn and when. This file, the module headers and the guardrails state the rules as they stand and point at a CHANGELOG date only where a precedent matters; a dated obituary belongs in the CHANGELOG, never here or in a header. **doc/guardrails.md is normative**: the strict MUST/MUST-NOT rules for the library and for applications built on it, plus the admission test every proposed feature passes (derivation → laws → subsumption → honesty → reachability → green stack → sync). Its L15 makes the demo suites the compatibility contract: any library change, however internal, must leave every demo compiling, bundling, and behaving correctly — verified by running `spago build`, `spago test`, `npm run bundle-demos`, and `npm run smoke` in full, with demos edited only when the demo itself is the subject of the change. Consult it before adding, changing, or accepting any combinator, class, component, or demo idiom — a change that violates a guardrail is wrong even if it works. Its **L16 import tower** governs imports: only the algebra layer (`PUI`, `Data.Profunctor.*` in bambik) imports the ecosystem's `Data.Profunctor`; vocabulary modules (design systems, `PUI.Web.HTML`/`PUI.Web.SVG`, packaged controls) build from the carrier + the re-exported vocabulary; application code imports neither the ecosystem algebra nor carrier internals, and `grep "import Data.Profunctor (" demo/` stays empty. Its **Part II is a pointer, not a rulebook**: the code-style contract for application code — demos included — is stated once, in **`.claude/skills/developing-bambik-apps/writing.md`** (its *Code style* section: layout, types and values, business functions, wiring). Read that file before writing or reviewing any demo or app module; nothing here or in guardrails.md restates it, and a change to how applications are written edits writing.md and the demos. The deployed restatements are exactly two, both deliberate (deployed HTML cannot read the skill file): the demo pages' `#code-style` note in demo/index.html, and **demo/workflow.html** — writing.md's *Writing order* replayed as a real session (verbatim compiler responses, captured with the pinned compiler; linked beneath every demo's source box). Re-read both against writing.md whenever the contract changes.

## Building

Do `export PATH=$PWD/node_modules/.bin:$PATH` and then `spago build` (tests: `spago test`).

### PureScript forked compiler

Note the repo builds with the forked PureScript compiler pinned in `package.json` (variant row sugar `[ l :: T | r ]`, `.label` constructors — see doc/variant-sugar.md), so `npm install` first. Nothing in the toolchain depends on a local checkout, so a plain clone builds on any Linux x86_64 machine: the compiler installs from its **GitHub release** (`erykciepiela/purescript` tag `v0.15.16-variant.9`, built from branch `variant-type-sugar` — the release also carries the bare `purs` binary as an asset, and package-lock.json pins the tarball's integrity hash), and the `Prim.Variant`-patched variant library is an ordinary git package in packages.dhall (`with variant.repo`/`.version` → `erykciepiela/purescript-variant` tag `v8.0.0-prim-variant.1`, branch `prim-variant`: `Data.Variant` re-exports the compiler's built-in `Prim.Variant.Variant` instead of declaring its own, so the sugar and `inj`/`on`/`match` share one type). External applications consume bambik as an **ordinary spago git package** pinned to a release tag (`with bambik = { dependencies = [ … ], repo = "https://github.com/restaumatic/bambik.git", version = "v0.1.1" }`) — spago clones the repo and globs `src/**/*.purs`, so no checkout sits beside the app and the demos/docs ride along under `.spago/bambik/<tag>/`. The entry must spell out the library's dependency list (spago does not read a git package's own spago.dhall), but **nothing in the repo duplicates that list**: the bootstrap procedure fetches it from the pinned tag (`raw.githubusercontent.com/restaumatic/bambik/<tag>/spago.dhall`) when it writes the app's packages.dhall, so adding a dependency here needs no companion edit anywhere. A library release is a tag on this repo. The standalone bootstrap flow lives in `.claude/skills/developing-bambik-apps/` (bootstrap.md, which carries the scaffold as inline file contents rather than a stored template), self-contained and copyable out of the repo. Beside bootstrap.md/writing.md/building.md the skill carries **vocabulary.md** (the situation-indexed lookup index into writing.md and the module headers) and **walkthrough.md** (flight-booker read line by line) — pointers, never a second statement of a rule.

### Development loop - watch mode

When introducing code changes, keep `spago build -w` running in the background and read its output after each edit (~0.7s incremental) instead of one-shot `spago build`s per change — plain `spago build -w` covers library, tests, and the demos. Caveats: spago -w reads stdin and dies on EOF, so keep stdin open (never `</dev/null`); run only one watcher over the shared `output/` at a time. Parallel subagents sharing the repo must serialize compiles (`flock <lockfile> spago build`) — and when a serialized build fails, check the per-module `Compiling` lines for *whose* module failed before assuming it's yours. It's recommended to use git workspaces in such cases.

`npm run dev` serves **every** demo from one server at `http://127.0.0.1:1234/`, rooted at `demo/` in the same folder layout the deploy scps to the remote host — local `/nguis/inbox/?mdc3` is `/bambik/demo/nguis/inbox/?mdc3` there, so every relative link and asset path resolves identically in both places (the server hosts nothing but the demos, so it carries no `/bambik/demo` prefix of its own). The root landing page (demo/index.html) links the two suites. Narrow the bundling to what you're working on by name, app or set: `npm run dev counter-mdc2 cells-mdc2`, `npm run dev inbox` (every design system of the app), `npm run dev nguis`.

  Auto-rebuild and browser auto-reload throughout (~2s per edit; scripts/dev.mjs — one mtime-polling watcher over src/ and demo/ driving two paths by file type: a `.purs`/`.js` edit runs an incremental `spago build` and esbuild's own watch over `output/` rebundles the affected demos and reloads once the new bundle lands, while a page's `.html` has nothing to compile and reloads immediately — both are handled independently, so an edit to each in the same tick gets a reload *and* a rebuild; reloads reach the browser over an SSE endpoint at `/esbuild` using esbuild's own protocol, injected via the bundle banner so demo pages need no dev-only markup).

  Polling rather than `fs.watch`: inotify costs an instance per directory and Node hits its per-process ceiling well before the ~40 source dirs here, while `recursive: true` silently delivers no events at all on ext4.

  Runtime emission trace: set `window.__bambikTrace = true` (or `localStorage.setItem("bambik-trace", "true")`) in the browser console to log every propagation decision — stage-to-stage flow, `looped` re-feeds/swallowed echoes, and gate-withheld emissions with the sibling fields they wait for. Independent of the trace flag, every knowledge gate carries a **starvation watchdog**: a gate that withholds and is never fed within 3s prints one `console.warn` naming the gate and (for the record merges) the exact missing fields, with the fix (`seeded`/`announce`, or looping the state through the model) — and the browser sink logs the missing fields' stamped host elements beside the message (found by `name`/`aria-label`, clickable in DevTools), so an unprimed gate is a named failure pointing at its place on the page, not a blank screen (and an unprimed *entry* is a compile error: `body` demands input `{}`) (off until a carrier adopts the host switches — `PUI.Web.adoptHostDiagnostics`, called at `body`/`runComponentInNode`, so a headless `spago test` is silent; opt out with `window.__bambikNoWarn = true`).

Verification stack: `spago test` (value-level law tests over probes — merge units, gating, exactness, the trace quartet, plus the audit laws of doc/observational-semantics.md: the seeded retractions and the three raw `×`-side deadlocks, the knot's yanking/conjugation/idempotence with the `dimap f f` counterexample, the named `Strong` and interchange deviations, merge symmetry and mixed-merge associativity, the container action's wire law, the `(->)` diagonal merges as pure equalities, and the observation-level laws — boundary interchange on the nose with its exact common stream, one-feed-one-release (a two-field broadcast releasing once with no torn row; nested merges releasing once; and the disjoint-operand shape it is reachable from — two `focusField @l` editors merged in parallel is a **type error**, `OwnedRecordOutputs` wanting disjoint labels where a whole-row citizen claims the whole row, which is why this law has no demo-level test; plus the input-side prediction — the feed law belongs to **inclusive** record input, so `+→×`, whose input is dispatched one case to its one owner, carries every ingredient of the torn row except a broadcast and provably cannot tear), `⊑`-monotonicity of `>>>` and `⊗`, the container action's inner-surface laxity, the Ocular admission law and its failure for a capturing decorator, `announce`'s naturality — and **test/Exhaustive.purs**, the bounded exhaustive check: the 34 laws (symmetry, associativity, both units, `⊑`-monotonicity and projection's input half at all four shapes; exactness and conformance to `PUI.Gate`'s pure step at `×→×`; arming at `×→+`; feed-idempotence and one untorn release per feed at `×→×`; and the container action as the same gate over runtime labels — `acted`'s conformance to the step rekeyed per feed shape, its `⊑`-monotonicity, the wire law, feed-idempotence and one untorn release per feed with `[]` the empty array's) over **every** script to length 6 (two operands or elements) or 8 (three) with a fresh token per event — complete by the data-independence argument of doc/observational-semantics.md §9, about 50 s of the run); `npm run smoke` (scripts/smoke/ — the committed headless-Chrome CDP harness: serves the repo, launches an isolated throwaway Chrome, runs scripts/smoke/tests/*.mjs. **Only what the laws cannot reach lives here** (an app's wiring is implied by the laws and its own code, so there are no per-demo walks): the **leaf-law bench** (tests/leaf-laws.mjs over demo/laws/: every published component alone, Repetition and Answer per shape, the platform's own input on every editor — typing and arrow keys and mouse clicks through CDP, picks for selects — storing a changed row that keeps the rest, every status showing its event, every `…Optional` selector clearing to its none case with nothing left checked, emitters replaying on click), the **mount check** (tests/demos-mount.mjs: every registered demo mounts, throws nothing and logs no warning or error past the 3s starvation watchdog, and is dressed by its vocabulary's `body` — about 40 s for all pages, opened ten at a time), the **carrier-only laws** (tests/acting-laws.mjs: the container action's keyed reconciliation, identity following the key in the real DOM), the **axe-core stamp audit** (tests/a11y-laws.mjs: the name-and-reference rules over `#demo-column` on the counter twins, order-form and both espresso-bar twins — the stamp invariant as someone else's checker), the **tooltip walk** (tests/tooltip.mjs: hover shows, leave hides, and leaving after a click still hides, on both Material twins) the **textarea overflow** regression (tests/textarea-overflow.mjs), and the **holey sweep** (tests/holes.mjs, guardrails L18: every demo's holey twin — its `*Logic` modules stubbed to bare untyped `hole`s by scripts/holes.mjs, every other module unchanged over the real library, built under holey.dhall into the gitignored `.holey/` by `npm run bundle-demos` — mounts clean from its view alone and reaches no hole when every control is clicked, pointed at, typed into and picked; `npm run check-holes` regenerates and runs just this); the value-level `Acting` laws (empty announces `[]`, singleton retraction, gather gate, keyed retention) run in `spago test` on `PUI Effect` probes; bundle the demos first, filter with `npm run smoke -- <name>`); `npm run check-view-model` (scripts/check-view-model.mjs — the view-model rule of writing.md's *Types and values* made mechanical: no `Maybe` field and no `Boolean` field off the Boolean-editor allow-list in any demo view model module, and no accessor section `_.l` in any demo view, law benches included — a stored field is named by its label, guardrails L3); `npm run check-determined` (scripts/check-determined.mjs — L18's determination half made mechanical: every demo's view compiled with a typed hole for every imported value, the compiler's hole list read back one declaration per round, its unknowns the model's tails, failing on any error that is not a hole, on any exported view model signature that is not its hint with the tails filled (verbatim where the hint has no unknown, otherwise decided by the compiler: a probe module applies a function typed at a declaration's hints, their unknowns quantified, to the module's values) or that quantifies, and on any export no view imports (a name two lines report at different rows is the one `forall` case and is not compared); `node scripts/check-determined.mjs <demo> …` names demos, about a minute for all; `--write` writes the signatures whose hints have no unknown instead of comparing them — each the hint in the layout of scripts/signature-layout.mjs, a missing one inserted, an unwritten name appended over a runtime `hole`); `npm run api-docs` generates the browsable API reference into `generated-docs/md/` (gitignored) from the module headers — the single source of truth for combinator contracts.

## Building & Deploying Demos

1. Verify the forked compiler: `node_modules/.bin/purs --version` must report `0.15.16 [development build ...]`; if it shows stock `0.15.15`, run `npm install` (stock purs fails with "Module Prim.Variant was not found").
2. Bundle for deploy: `npm run bundle-demos` (minified, all demos; `node scripts/bundle.mjs <name|set>` for a subset) — use `npm run dev` for watch mode, not for deploys. **Every** demo is a named module (`Counter.MDC2`, `Crud.HTML`, `OrderForm.MDC2`, ...) entered at its own function (`counter`, `crud`, `orderForm`, ...) — no module is `Main`, so they all compile together under one plain `spago build` — and each bundles from the shared registry in **scripts/demos.mjs** (the single source of truth for directory + module + entry, shared with the dev server), which synthesizes the esbuild entry per demo because `spago bundle-app` can only call `Main.main`.
3. Deploy: `npm run deploy-demos` — scps demo/index.html, demo/workflow.html and both suite directories to host `xyz` (root@erykciepiela.xyz, see `~/.ssh/config`) at `/var/www/html/bambik/demo/`.
4. Verify: `http://erykciepiela.xyz/bambik/demo/<d>/` returns 200 (plain HTTP only). The deploy copies over the remote tree and deletes nothing, so a directory removed or renamed here lingers there until removed by hand.

Every demo page is pure structure over **shared chrome**: `demo/page.js` (loaded as `../../page.js`) runs the page's design system and fetches the listing it names. Every page carries one `<template data-variant="mdc3" data-source="MDC3.purs Inbox.purs">` per design system (a plain-HTML app one `html` template), holding that system's stylesheets (and any style of its own), and page.js installs the one the query names (`inbox/?mdc3`, the first template when it names none), drops the page's elements marked `data-variant` for the others (reorder's per-twin notes, espresso-bar's subtitle), and loads `bundle-<variant>.js` once the stylesheets have loaded, so a catalogue never initializes on an unstyled page. Space-separated filenames render several listings — the first fills the existing box, each further file gets a filename heading + listing, the header readout sums the sizes; order-dashboard shows its app and its packaged controls module this way. page.js fills the header's source/bundle size readouts, shows every listing verbatim, notes the vocabulary words the view imports beyond the earlier demos of README's reading order (each linked into vocabulary.md), and groups the running demo with its tracing note into one `#demo-column` — on a page marked `<body data-surface>`, inside the vocabulary's card surface built by page.js (the app's surface is the page's, so no app wraps itself in a card) — the demo mounts into `<body>` at runtime with no marker class of its own, so it cannot be wrapped in static markup and is collected once, then the observer disconnects. The two notes shared by every page live once in **demo/index.html** as `#code-style` and `#tracing` sections, linked from beneath the source box and beneath the running demo respectively; the source-box note also links **demo/workflow.html**, the writing-session showcase.

## Cutting a release

A bambik release is a tag on this repo plus a release page. Four steps:

1. Bump the tag in the dependency table of `.claude/skills/developing-bambik-apps/bootstrap.md` **before** tagging, so the tagged tree documents its own tag. Nothing else in the scaffold carries a version — the packages.dhall step writes `<tag>` and fetches the library's dependency list from it, so a new library dependency needs no companion edit.
2. Verify green in full — `spago build`, `spago test`, `npm run bundle-demos`, `npm run smoke` (L15) — then tag the verified commit (`git tag -a v0.2.0`) and push tag and branch.
3. Create the release page from the tag. The body is **minimal by decision**: the skill-usage prompt naming this tag's asset URL, and nothing else — the toolchain pins, the packages.dhall entry and the prototype status live in the skill's bootstrap.md (which ships attached) and are not restated per release, so there is one place to keep them right. Use the **GitHub REST API**, not the `gh` CLI — `gh` is not installed here, and `GITHUB_TOKEN` is in the environment. Write the JSON with a script rather than inlining it in the shell, so the body's newlines and markdown survive quoting:

```sh
node -e 'require("fs").writeFileSync("body.json", JSON.stringify({
  tag_name: "v0.2.0", name: "bambik v0.2.0", draft: false, prerelease: false,
  body: "…the skill-usage prompt, naming the asset URL for this tag…" }))'
curl -s -X POST -H "Authorization: Bearer $GITHUB_TOKEN" \
  -H "Accept: application/vnd.github+json" \
  https://api.github.com/repos/restaumatic/bambik/releases -d @body.json
```

   The response's `upload_url` (strip its `{?name,label}` suffix) is where step 4 posts. A `message` field in the response means it failed — check it; curl exits 0 on API errors.
4. Attach the authoring skill as an asset, built **from the tagged tree** so it cannot drift from the library it documents:

```sh
mkdir -p /tmp/skillpack && cd /tmp/skillpack
git -C <repo> archive v0.2.0 .claude/skills/developing-bambik-apps | tar x --strip-components=2
tar czf developing-bambik-apps-v0.2.0.tar.gz developing-bambik-apps
curl -s -X POST -H "Authorization: Bearer $GITHUB_TOKEN" \
  -H "Content-Type: application/gzip" \
  --data-binary @developing-bambik-apps-v0.2.0.tar.gz \
  "<upload_url>?name=developing-bambik-apps-v0.2.0.tar.gz"
```

   Then verify the published result rather than assuming it: re-fetch `releases/tags/v0.2.0` (asset `state` must be `uploaded`, `draft` false) and run README's install one-liner into a throwaway directory.

The asset is a snapshot, so it needs re-attaching per tag — that is its one cost, bought for a portable one-command install (`tar xz -C .claude/skills`, no GNU-only flags) and a visible place to find it. README's install command and release link need the new tag too. This procedure lives here, not in the skill: cutting a release is a library-maintainer task, and the skill ships to application developers who never do it.

## Architecture

### Core Type

```purescript
newtype PUI m i o = PUI (m { toUser :: i -> Effect Unit, fromUser :: (o -> Effect Unit) -> Effect Unit })
```

- `i` - input type (data model to display)
- `o` - output type (data model to capture)
- `toUser` - pushes model updates to UI
- `fromUser` - captures user interactions

The rows a pipeline operates over hold **state, not copy** (guardrails L17): **copy is a function, not a field** — a display whose content *is* copy takes its read function at the leaf and no label (`text balanceLine`), the function living in the view model module, so the view line names its own writer and the screen's copy is unit-testable in `spago test`. A display that renders a *number* takes a read function too (`progressBar elapsedFraction`) — a fraction is *derived*, and derivation is the same act as formatting — keeping its label as the **accessible name** only; quantity *editors* are untouched (`sliderLive @"Duration"` still names its field). `settled` is left with invariants among *edited* fields, and in the demos every surviving `# settled` sits on an editor. doc/research-copy-is-a-function.md is the rationale (it partially reverses doc/research-presentation-model.md, keeping its testability motivation and its `settled` half). Fixed copy is **static or constant** (writing.md): a static is on screen before and regardless of any data and is a value, never a label (it names no field and no case): `staticText "Hours" :: PUI Web {} {}`, never a model field; a static inside a pane is still a static, the pane deciding when it shows (`staticText "Error" # inCaseAt @"faulty" @"condition"`); a constant is fixed copy that reads data (a sentence's glue, a line carrying a business value) and lives in a copy function (`text refundLine`). A **status tells its case**: a `+→×` status takes no copy function — its case label is its whole copy (`snackbar @"Flight booked" identity`, `toast @"Review submitted" identity`), since a status is ephemeral and what the user must keep (a reference, an order number, a reason) is a model field shown by a display — so a status stands after an action whose outcome row names its outcomes as the words the user reads, and an outcome's reasons are sibling cases, each told by its own status (order-form's `"Order submitted"`/`"Order rejected: no total"`); a command straight from an emitter folds silently, its case being the button's caption. The non-ephemeral message is no status: MDC2's `banner` is an ocular over a display, shown by a pane while its state holds (inbox's `( banner $ staticText "Inbox zero!" ) # inCase @"empty" inboxState`), and HTML's `output`, the element for a user action's result, is the plain floor's status, staying in place because nothing there dismisses itself.

### Key Source Files

- **src/PUI.purs** — the core profunctor, the carrier. Instances: `Profunctor`, `Strong`, `Choice`, `Semigroupoid`, `Category`
  (`identity` is the echo wire), the four row merges, the two mixed strengths
  (`Resolving`, `Retaining`), and the **trace quartet** — `Costrong`/`Cochoice`
  (ecosystem duals of `Strong`/`Choice`: state feedback and iteration;
  knowledge-gated) plus the coined `Coresolving`/`Coretaining` (terminating
  fold, productive unfold). Each co-strength is its strength's retraction —
  and the retraction laws are **seeded**, because on this carrier the three
  `×`-side raw composites are provably dead (each gate waits on the other):
  `unfirst (seeded (Tuple a0 c0) >>> first g) ≈ seeded a0 >>> g`,
  `coresolve (resolve g >>> seeded (Right c0)) ≈ debounced g`,
  `coretain (seeded (Right c0) >>> retain g) ≈ g`, while `unleft (left g) = g`
  holds raw. That asymmetry is the theorem — **`PUI` is genuinely traced over
  `+` and only pointed-traced over `×`** (Elgot iteration is free, Conway
  feedback needs a starting point; the seeds are the operational ⊥), and it
  is why the knot is a carrier primitive while the `+` side needs none.
  **The point and the knot are carrier words, not classes**: `announce`
  (one registration emission out of the terminal record; laws **answer**,
  `announce a ≈ lcmap (const a) identity`, row-forced, and **early**,
  answered before any feed — the one thing a timeless carrier lacks) and
  `looped` (the `×`-diagonal self-trace, re-entrancy-guarded; laws the
  trace triple at the diagonal plus the behavioral re-entry law). Each was
  a one-instance class, `Seeding` and `Looping`, until 2026-10-14
  (CHANGELOG), dropped as carrier facts dressed as algebra, their laws kept
  in the words' headers. **An app's knot is `looped`**, and the only knot: a
  loop through the four shapes is cut at its record junction — the model
  re-enters whole, closed by `with` and the model its first stage takes
  (`# looped # with seed`); an event re-enters through its fold,
  since only there does the model row hold it. The per-shape knots the
  algebra could state (a hidden seeded field, a variant knot, a terminating
  fold, a productive unfold) have no words: each kept a state the rows did
  not show, and a demo's looped state is a model field (CHANGELOG,
  2026-10-14, the precedent for the next one proposed). The co-strength
  classes stay, with their `PUI` instances law-tested in test/Main.purs.
  The deadlocks, the seeded laws and every other statement below are stated
  against **doc/observational-semantics.md** — the two-phase protocol, the
  equivalence `≈` / refinement `⊑`, the three component laws
  (feed-idempotence, record-echo totality, no synchronous event echo —
  stated in the shape headers as the **two laws of a record input**,
  Repetition and Answer, beside the **four merge laws** monoid, projection,
  preservation and monotonicity, all six once in `Data.Profunctor.Row`
  "The laws" and read per shape; what they guarantee an application is
  writing.md *What the laws guarantee*, doc §3.3 for the semantics) with
  their per-shape modalities (§3.1: `×→×` **must** echo, `×→+` **must not**,
  `+→+` and `+→×` **may** — output shape decides whether an echo is owed,
  input shape how it is discharged; the two musts differ in reach: "must
  not" is `>>>`-closed and read off the type, "must" is a leaf law kept
  only along a route that stays at `×` — `emitter >>> fold`, every loop's
  body, is `×→×` by type and answers no feed, owing only wholeness, so a
  stage answers iff its route has no variant), §3.2's account of what a `×→×` gate
  waits for (**owned fields only** — a display never enters it, its `{}`
  per feed being the feed's answer, inert to gates and real only to
  sequencing; starvation is always a priming failure of an owned field,
  never cured by an echo on a display), and
  the named deviations (the gated `Strong` law holds only as primed
  equivalence; interchange is observation-level-relative — at the inner
  surfaces it fails on the nose and holds as `⊑`, at the boundary it holds
  on the nose for protocol-respecting operands, because **a feed is one
  step**: a gated merge's broadcast is batched and released once
  (`steppedFeed`), so a multi-field feed never emits a torn row — so a
  gated merge is premonoidal counting renderings and monoidal counting
  channels; `>>>` and the merges are monotone in `⊑`). The three component
  laws are about **shape**, and every published component
  has the shape of its behaviour (a variant at channel position is an
  event, inside a field a value), and every occurrence source — MDC3's
  `tile @l @k`, `listOf @l @k`, `onClickedXY @l`, HTML's `button @l` — emits as
  case `l`, so no record-shaped source exists; what the type still cannot
  enforce is that a `×→+` leaf never emits inside its own feed, which is
  the vocabulary provider's conformance.
  **State lives in registration, not in `m`**: registration hands the
  continuation over and returns the input leg
  (`PUI m i o = PUI (m ((o -> Effect Unit) -> Effect (i -> Effect Unit)))`),
  so a feed cannot precede it, and every instance — the strengths, the
  trace quartet, the gated `×→×` merge, the knot — allocates its cells in
  the registration `Effect`, once per instantiation. The algebra asks of
  `m` only what it composes: `Functor` for one operand (`Profunctor`,
  `Strong`, `Choice`, the trace quartet, `Resolving`, `Retaining`,
  `looped`), `Apply` for two (`Semigroupoid`, the `×→×` merge), and
  `Applicative` for none (`Category` — the wire is the continuation
  itself — the point, the ungated merges, `fold`). `MonadEffect m` is
  left to what builds in `m`: `Scoped` (the scope a word reads — `ticks`,
  `action`), `Hosting` (collection placement) and `handles`; two-leg
  leaves are written as `{ toUser, fromUser }` and adapted by
  `fromHandles`/`handles`, and the only `unsafePerformEffect`s left in the
  algebra are the three module-level diagnostics switches. Also `Hosting` and
  the generic `Acting (PUI m)` instance (see the collection bullet), and the
  **development diagnostics** — the private `tr` (emission trace) and
  `gateGuard` (starvation watchdog) and the pure `isHole`, instruments
  pointed at the algebra rather than part of it, living here because `PUI`
  is their heaviest caller and they cannot sit under `PUI.Web`, which
  imports `PUI`. Both switches and the log
  sink are parameters (`setTracing`/`setDiagnostics`/`setSink`, no-ops until
  installed), so the algebra carries **no JavaScript**; a carrier installs them
  (`PUI.Web.adoptHostDiagnostics`). Its export list groups the **plumbing**
  the vocabularies and the law tests build on (`announce`, `seeded`,
  `resolveFor`, the scope and diagnostics API) apart from the vocabulary a
  view writes; no demo writes a plumbing word. The module header carries the
  pipeline-semantics doc; `npm run api-docs` generates the per-combinator
  contracts from the headers.
- **Holes against the real library** (guardrails L18) — every view runs with
  its logic stubbed to bare untyped `hole`s (`PUI.Web.hole`, which throws on
  every touch but the field `__bambikHole`, read by `PUI.isHole`), importing
  the real modules unchanged. What makes
  that possible: a stage is typed at **one row** (no `Union` subsumption —
  a business function is typed at the stage's row, its signature the
  view's hole hint with its tails filled — `Record t0 -> String` reported,
  `countLine :: { counted :: Int } -> String` written — checked by
  unification), a shared record input is one row variable
  (no class states an equality), keyed collections compare
  structurally (`Data.Profunctor.Row.Structural`, a key read through the
  ecosystem `Foreign`) and a selector compares its options' tags, and the words that consume a logic value when built
  (`announce`, `ticks`, `clock`, `debouncedTextField`, `action`, `load`, `foreach`, `anyOf`, `anyEdit`,
  `dispatched`, `accumulated`, `each` and `text`, which renders nothing of a hole
  read) treat a hole as absent.
  The record of how it was chosen (twin modules, free rows, and this) is
  doc/research-holey-weak-types.md. Its converse is the **determination**
  half of L18: with a typed hole for every value the view imports, the
  compiler's hole list is the view model module's signatures **up to
  their tails**. The view fixes the skeleton — which holes share a row
  (one row per stage, the knot re-feeding it whole), what each returns,
  every field and case a line names by label at its leaf's type — and
  leaves the rest of each row unknown: the tail, the model's to fill
  (counter's view reports `Record t0 -> String`, `Record t0 -> Record
  t0`, `Record t0`, and the module writes `t0` once, as `counted ::
  Int`, in each). So a view does not determine *the* model — many fit
  one view, representation being the model's choice — and the view
  **writes no row**: the model's, a classifier's cases, a projection's
  element row and a payload are the signatures', and no word takes them
  as a visible type argument (`looped`, `with`, `load`, the panes and
  the collections take labels only; until 2026-10-15 they took the rows
  so that nothing was unknown — CHANGELOG), and an action takes none
  either: it is typed at exactly its outcomes, closed by what tells them
  — its statuses or capture, or in an emitter block the `joined @l` /
  `joined2 @l1 @l2` that joins it (until 2026-10-16 it declared them,
  `action @l @( "…" :: _ )`, since its own widening was stuck on a
  hole's row — CHANGELOG). Option lists stay closed by `<+>`. A stored field is
  **named, never picked** (guardrails L3): the reading word's label
  sibling takes it in its own slot (`textAt @"title"`, `listOfAt …
  @"messages"`, `# providedAt @"halted" @"phase"`), typed by the model
  row — no view contains an accessor section or a restated row (`npm
  run check-view-model`). `npm run check-determined`
  (scripts/check-determined.mjs)
  replays it for every demo.
- **src/PUI/Gate.purs** — the knowledge gate as **one pure Mealy machine**
  over runtime **participants**: `gateStep :: Ord k => GateState k v ->
  GateInput k v -> Tuple (GateState k v) (GateOutput k v)`, in a module with
  no `Effect` in it. Participants are keys each holding one slot; the gate
  releases the slots in participant order once every participant is known,
  retain-last thereafter. The `×→×` merge enrols its **owned field
  labels** (participants are fields, not sides, so the
  zero-field clause of L6 — an operand owning no field is born satisfied and
  inert — is a consequence of enrolling nothing, not a `GateConfig`), and
  the container action's gather (`acted`) enrols the **fed keys**, rekeyed
  per feed (`Rekeyed`: survivors keep their slot, entrants unknown, leavers
  forgotten; `[]` is the zero-participant release) — the gather gate *is*
  the record gate with the row's labels supplied at runtime, and a feed is
  one step at both (a reconcile whose elements echo gathers once, whole).
  The effectful part is one function in `PUI`, `driveGate` (read, step,
  write, act, hand the output back for diagnostics); `steppedFeed` brackets a
  feed with `StepBegun`/`StepEnded`; `gatedRecordOutputs` contributes each
  operand emission as its labelled fields and assembles a release back into
  the row (`Record.Unsafe`), `actedWith`
  contributes each element emission as its one slot. The machine is
  data-independent with finite control (`v` polymorphic, `known ⊆ order`),
  which is what makes the bounded exhaustive check in test/Exhaustive.purs
  complete rather than a sample (doc/observational-semantics.md §9), and it
  is the port target for a mechanised proof.

  The **vocabulary** `PUI` defines or re-exports (contracts in the headers):

  | Word | What it is |
  | --- | --- |
  | `static` | an element with nothing in it — an ocular applied to the wire, pinned `{} → {}` (`static (span >>> cl "ripple")`); with `staticText`/`staticHTML` the three statics |
  | `separating` | (`PUI.Web`) a static rule as an **ocular shown only between things**: `separating rule w` builds the rule, then `w`, and a `MutationObserver` on their parent keeps the rule visible exactly while something rendered precedes it and `w` renders something — decided on the DOM (non-blank text, replaced or custom elements, recursively; comments, hidden nodes and other rules count for nothing), never on data, so it stays `forall a b` and passes the admission law. **An ocular may be UI-reactive, never data-reactive** (`clWhen` reads data, so it is typed at `{ \| r }` and is no ocular). Every vocabulary's `divider` and HTML's `hr` are `separating` over their rule, written leading like every static (`divider $ slider @"Attendees" # inCaseAt @"chosen" @"Room"`, meeting-booker — no rule above an absent slider); scripts/smoke/tests/separator.mjs checks it on the real DOM |
  | displays | a display — `text`/`textAt`, the statics, `each`/`dynamic`, the progress bars, `imagePane`, `columnHeader`, a vocabulary's typography over any of them — is `{ \| r } → { \| r }`: it renders the row and **releases the row it was fed**, so a display is a stage like an editor and a view line sits where it sits on the screen. `displayWiring` (render, echo the value) is the protocol; `confirmed`'s content, the drawer's content and `group`'s heading are plain stages inside the library too |
  | `with` | discharge a chain's initial obligation at any seed type: `announce a >>> w` — a model into the knot (`looped # with seed`) or into a knotless flow (`# with invitation`, potluck), the seed's signature the row; its own input is ignored, so it sits at any row, and a hole seed is never announced. Never `with {}`: `body` feeds `{}` once itself, so a load action standing before the knot runs on that feed. `announce` (the point), the `seeded` echo wire derived from it through `Choice` (what `debounced` primes its loop with) and `resolveFor` are plumbing, not vocabulary |
  | `looped` | the **knot**: the `×`-diagonal self-trace over the loop's row, which the seed's signature states (`looped # with freshCount`), re-entrancy-guarded; the model re-enters whole, primed by its first feed, which `with` supplies — a looped state is a model field, never a hidden channel. (`mvu`, the two words fused, was DELETED: the acronym names an architecture the loop does not need) |
  | `settled` | `rmap`-only normalization of the row, the normalizer an update of the stage's row in the logic |
  | lines that show data | every line of a `Semigroupoid.do` block is a stage, a display included: it is written in its screen position and releases what it shows. The pane for displays and editors alike is `inCase @l f content`/`inCaseAt @c @l` (`PUI.Web`: attached and fed the whole row on case `l`, the wire while detached); a list of displays is `item # foreach @l proj` (the fed row passes on), over a stored array `# actedAt @l @t`; `confirmed title $ content` (MDC2/MDC3) the witness rung — modal, flow withheld until the user confirms |
  | scope | `Scope`/`Scoped` in `PUI`: the carrier's record of whether the subtree a word was built in is on the screen — a pane suspends its content's scope while detached (`attachable`), a collection's leaver and a `dynamic` rebuild cancel theirs (`Hooks.instantiate` returns `cancel`); `ticks` skips beats while suspended and ends on cancel, `action` drops an outcome that settles in a suspended or cancelled scope, `separating` disconnects its observer on cancel — the pane law (a detached emitter is silent) made true for the emitters that originate occurrences; retention untouched. `Web` keeps the scope in its builder state, `Effect` reads an ambient one under `inScope` (the law tests) |
  | `ticks @l` | the tick source, the `×→+` leaf owning case `l` (`ticks @"Clock ticked" tickPeriod`): every period the row it was last fed leaves as case `l` — replay built in, as under every emitter; feeds retained, nothing emitted inside a feed; a tick that needs telling is told by a status on its way, `>>> (status # observed)`, so the source opens with none |
  | `fold @l f` | **one case folded into the record** (`+→×`): the faceless leaf owning case `l`, `f` of the payload the whole next model (`fold @"Count" increment`) — `unshown @l # folding f`, named for the common case, a command straight from an emitter folding silently because its result is already on screen. A loop's folds are one per event case, merged in `VariantToRecord.do`, each releasing the whole next model (the merge's output row is **shared**: the copairing of the coproduct, each release forwarded whole as it comes, where the `×→×` merge owns its fields exclusively; a status beside the folds releases nothing and is typed at every row). Memoryless: a replaying emitter's payload is the row it was fed, a payload event arrives `joined` with it (`kept` the fold that keeps the model), an effect returns the model. **Every `+→×` leaf folds**: a status takes its fold (`snackbar @"Person created" identity`) and tells its case, the label its whole copy, so nothing stands in a fold block releasing nothing. The forms it passed through on the way (CHANGELOG, 2026-10-02 to 2026-10-07) are the precedent against a fold that keeps state of its own |
  | `folding @l f` | plumbing, not vocabulary: the fold behind every `+→×` leaf — a status face owning case `l` and the fold of its payload in one leaf; every vocabulary status is it over its face (`snackbar @l f = snackbarFace (reflectSymbol (Proxy @l)) # atCase @l # folding @l f`) and `fold @l f` is it over `unshown @l`; derived as the status and the wire under the `×→×` merge; the status is fed the **event**, never the fold's result, so a status with copy stands only after an action that returned what it tells |
  | `reset @l x` | `fold @l (const x)`: one case folded by replacing the model whole with `x` (`reset @"New game" openingPosition`, tic-tac-toe), the payload unread; a status resets with `(const x)` (espresso-bar's placed order resets the form) |
  | `capture @l` | `fold @l identity`: one case folded by taking its payload as the model whole (`capture @"People loaded"`, crud) — the fold of an effect that returns the model; a status captures with `identity` (`snackbar @"Order loaded" identity`; crud, order-form, payment, weather, password-generator, reorder). `reset` and `capture` are kept over `fold` for the view line's sake, a readability decision, not a subsumption one |
  | `joined @l` | **case `l` joined with the row its emitter was fed** (`×→+`), the label the source's own restated — every emitter is open on its output, so nothing but the label says which case is the source's; the label **closes the source** to case `l` and widens the joined case into the block's row, which is what places an action (typed at exactly its outcomes) among emitters; a two-outcome source is `joined2 @l1 @l2`, since one label closes one case: `first` around the source, the row riding the state channel and leaving beside each payload as `{ event, model }` under the same case — a list pick, a canvas click, an action's outcome (the action joined with its button, order-form's estimate and submit, ticker); a pane's button replays the model itself, so it needs no `joined`. `kept = _.model` is the fold of an outcome that is told and changes nothing. A click is the payload-less case: every vocabulary emitter is `joined` of a private click source (`PUI.Web.occurrences`) with the `{}` event dropped and the case renamed (`replayed`), written there (a word for it, `replaying`, was a derivation and went — CHANGELOG, 2026-10-14) |
  | `action @l f` | the **`+→+` leaf of case `l`**: fed the row its button replayed, running `f` on it, its outcome leaving as a case of the row the `Aff` names — typed at **exactly** those outcomes, like `load`, so the view writes no row for it: the statuses or capture after it own their cases and close it (`action @"Create" createPerson`, crud; `action @"Book" submit`, flight-booker), and among emitters the `joined @l`/`joined2 @l1 @l2` that joins it closes it and widens it into the block's row (order-form, ticker) — with its logic a hole, those followers type it; opened by its progress indicator (`indeterminateLinearProgress # action …`), which owns the run's `started`/`ended` and nothing else |
  | `load f` | the **`×→+` action a feed runs**, typed at exactly the outcomes its `Aff` names, the capture or status after it naming the case it takes: the load before the knot, run by `body`'s one `{}` (`indeterminateLinearProgress # load loadPeopleCatalogue; capture @"People loaded"`, crud; order-form); like `action` it widens nothing, so its outcomes need no line of their own |
  | no adopter on a view | **every leaf narrows its input and widens its output itself** — a status or an action owns the one case its label names, an emitter is open on its output, an action is exactly its outcomes and `joined` widens it into an emitter block, a dialog (`confirmed @l`) owns the case that opens it — so the floor's single-label adopters `atField`/`atCase`/`toField` stay inside the vocabularies (a face is adopted at its label, `snackbarFace … # atCase @l`) and `PUI` re-exports none of them; no view writes `toCase` either — an editor's release leaving as an event is the editing list's to say (`anyEdit @l @k`, movie-browser), so `PUI` re-exports `settled` and nothing else of the floor; `expanded` (one day old) is deleted, and `clicked` (make any element clickable: a shape change dressed as a word, stamping nothing) is deleted with it — a click is a vocabulary leaf's, `button`, `listOf @l @k`, MDC3's `tile @l @k`. Plus `acted`/`actedAt` — `focusField`/`focusCase` are deliberately **not** re-exported: the leaf lift is design-system plumbing (every vocabulary editor is `focusField @l`-lifted inside, `group @l` carries sub-model nesting), and `grep "focusField @" demo/` is empty. A rename of a closed singleton on both sides has no word: its one use is re-labelling a component built at a fixed label, which L3 forbids (`asField`/`asCase`, CHANGELOG 2026-10-14). **Business functions are arguments of leaves, never adopters**: a status is label-indexed at its business case and tells it by its label (`snackbar @"Flight booked" identity`, read "the flight-booked snackbar"; mutually exclusive outcomes are sibling statuses, each owning its case) and an emitter emits its own case — inside, a status is its face over its label, `snackbarFace (reflectSymbol (Proxy @l)) # atCase @l` |

  Durations are structural `{ ms :: Number }` throughout (`debounced`,
  `resolveFor`, `debouncedTextField`) — never `Milliseconds`.
- **src/PUI/Web.purs** - the carrier **and the root of the web layer**: DOM monad (`Web = StateT DOM Effect`), `Node`, DOM building blocks (`element`, `attachable`, `runDomInNode`) and FFI, plus the **element-neutral vocabulary** — every word that names no element: the decorators (`attr`/`:=`, `attrDyn`/`:=>`, `cl`, `attrWith`/`attrAt`, `clWhen`, `init`), the text leaves (`text`, `textAt`, `statusText`, `staticText`, `staticHTML`), the occurrence source `onClickedXY`, `provided`/`providedAt` and the pane (`inCase`/`inCaseAt`) — each `…At` the label sibling of a function-taking word, for a field the model stores, and the structure builders (`dynamic`, `each`, `el`), and `hole` (guardrails L18) — so SVG and every design system use them without importing the HTML vocabulary (the interaction table under the HTML bullet lists them). It also carries **the leaf protocol as functions**: `editorWiring` (write unless held, echo every feed, report the platform's change through a busy guard, a report before any feed dropped), `selectorWiring` (the same over an option list in index terms, the echo left to `selectedAt`), `displayWiring` (render, answer `{}`), `statusWiring` (render, release nothing), plus `eventLeaf` (chrome placed at the emitter's row with a bare `lcmap (const {})` — the library names no coercion), `eventText`, the click plumbing every emitter is built from (`occurrences`, the private click source at `[ occurred :: {} ]`; `replayed @l f`, `joined` with the fed row, the event dropped and `f` of the row leaving as case `l`; `clickedRow @l` and `eventLeaf @l` over them), `escaped` (every runtime string a vocabulary interpolates into `staticHTML` markup passes through it), `listenNode`/`listenNodeDeferred`, `ensureStyle`, `autoDismiss` and the option tags — every leaf of every vocabulary is one of the four wirings over its own element, so the echo, the busy guard and the one-registration listener are written once, and a leaf that hand-writes its `toUser`/`fromUser` pair is a review finding. No *element* lives here. Everything browser-specific is a submodule of it: the element vocabularies `PUI.Web.HTML`/`PUI.Web.SVG` and one module per design system (`PUI.Web.MDC2`, `PUI.Web.MDC3`, `PUI.Web.Shoelace`, `PUI.Web.Fluent`, `PUI.Web.Bootstrap`), all under **src/PUI/Web/** — so the carrier-independent algebra (`Data.Profunctor.*`) stays visibly separate from the web specialization
- **src/PUI/Web/HTML.purs** — the 1-1 HTML vocabulary over the carrier: the element oculars and HTML's native controls, and nothing that names no element (that is `PUI.Web`'s).

  **Entry**: `body :: PUI Web {} o -> Effect Unit` registers the wiring and
  then feeds `{}` **once** — the terminal record's one value,
  which a point has already answered at registration and, by Repetition at
  `{}`, answers no further — so it demands a **closed** app — a pipeline's
  residual input row *is* its initial-state obligation, discharged by `with
  initial` down to `{}`, the one self-pointed record. A forgotten seed is therefore a
  compile error at the mount point, naming the unsupplied fields. This is the
  plain-HTML floor's entry: every design-system module exports a `body` of
  the same signature that first dresses the page for its catalogue — MDC2
  puts the `mdc-typography` baseline on the body, MDC3 adopts its typescale
  stylesheet, Fluent applies its theme, Shoelace sets its icon base path,
  Bootstrap's is this one under its own name — and then mounts here, so an app imports
  its entry from its vocabulary like every other word and no vocabulary acts
  on the page at import time.

  **Interaction vocabulary** — element-neutral, so it lives in **`PUI.Web`**
  (the collection combinators `foreach`/`acted` live one level
  lower still, at the **PUI level** — see the container-action bullet):

  | Word | What it does |
  | --- | --- |
  | `attrWith` | value-computed attribute — the channel-fed counterpart of static `attr`/`:=`, so a cell's style/coord/colour updates in place through the channel rather than by rebuilding a closure |
  | the replay protocol | every emitter replays the row it was last fed on click — `button`'s replay-last-value protocol: replay is lawful over records only, the channel is a variant, so an emitter is `×→+` by shape — and is **derived** as `joined` of a private click source (`occurrences`, `p i [ occurred :: {} ]`, at `{}` the point's dual) with the `{}` event dropped and the case renamed (`replayed @l`), so the replay `Ref` is `Strong`'s retention and a click before any feed is the primed `Strong` law's withheld emission. The vocabularies build on it: `clickedRow @l`/`eventLeaf @l` behind the button family, `listOf @l @k` and MDC3's `tile @l @k` (one clickable cell of a grid, a real `<button>`, reporting field `k` of the row it showed). `clicked @l @k`, which made any element clickable, is deleted (2026-10-15): neither leaf nor ocular, it stamped no role or name |
  | `provided` | the **emitter pane**: case-gated existence of a `×→+` content (`PUI Web { \| r } [ \| o ] -> PUI Web { \| r } [ \| o ]`) — its argument is a variant-returning business function (a stored variant field is named by label instead: `providedAt @c @l`, with `inCaseAt` beside it), content attached and fed the **whole row** on case `l` (so a button inside replays the model and needs no `joined`), detached on every other case, where its silence is `×→+`'s answer. Its sibling for displays and editors is `inCase`, answering for a detached content itself — over the same attach/detach mechanism, `attachable`. There is no `Maybe` form: a state a pane depends on is a variant with named cases, so mutually exclusive states are exclusive by construction and the view line names the state it shows. It **detaches**, so it is a pipeline stage, not a gated-merge operand; it names only its case (`provided @"reading" messageView`), as `inCase` does — the states a pane chooses among are the classifier's signature (L18); a case carries a payload only where the business needs one |
  | `clWhen` | value-dependent class — styling, deliberately last-element-only |
  | panes | `inCase @l f content` (displays and editors, a stage `{ \| r } → { \| r }` fed the whole row), `provided @l f emitter`; both name only their case, the classifier's signature stating the rest. `confirmed @l caption f content` (the witness rung, MDC2/MDC3) is no pane but a `+→×` leaf like a status: opened by case `l`, which it tells as its title, its content shown the payload, the confirm folding `f` of it |
  | `onClickedXY @l` | container-level pointer-down coordinates (local/viewBox `{ x, y }`) for canvases, emitted as case `l` |
  | sources | `source @l now watch` and the facts over it — `geolocated @l` (`[ located :: { lat, lng }, unknown, denied ]`), `online @l`, `clock @l period` (local `{ hours, minutes, seconds }`, the `×→×` sibling of `ticks`), `colorScheme @l`; HTML's `filePicked @l` is the capture over the same word: a fact the platform knows and the app cannot seed, owning one field, reading nothing, **pointed** (announces its first value at registration, so a merge of sources releases at once and no dummy seed exists), every feed answered with the last value known, each platform change an emission outside any feed; a fact the platform may withhold is a variant with named cases, never a `Maybe`, so no gate starves on it; the watch is quiet in a suspended scope and ends with a cancelled one. Merged `RecordToRecord.do`, each owning its field — the one application use of the `×→×` merge (here-and-now, incident-report) |

  Announcing statics are `staticText` (`PUI.Web`) and the void `hr` (`{} → {}`
  chrome); the raw-HTML `staticHTML` sits beside `staticText` in `PUI.Web`,
  since L10 keeps an HTML-string surface out of the public vocabularies. `input`/`textArea`
  are focus-guarded. The **element oculars** cover the usual set
  (`div`/`span`/`table`/`tr`/`td`/`ul`/`li`/`p`/`h1`–`h6`/`img`/`a`/`label`/
  `strong`/`em`/`code`/`blockquote`/`header`/`footer`/`section`/…, plus
  `PUI.Web`'s generic `el` for computed tags) with `PUI.Web`'s `attr`/`:=` and `cl` decorators; the
  **SVG** oculars (`svg`/`circle`/`path`/`text`) live in **`PUI.Web.SVG`**,
  imported qualified when a component needs both the HTML `text` leaf and the
  SVG `<text>` element. SVG works because `element` is namespace-aware
  (`svg` opens the SVG namespace, children inherit, only SVG-namespaced
  elements use `createElementNS`; HTML stays on `createElement` so MDC init is
  unaffected).

  The native elements with a model interface are **label-indexed components**
  (L3), each stamping its label as the host `name`: the selector
  `select @l opts` / `selectUnpicked @l @c opts` / `selectOptional @l @c @n opts` (editors of field `l` — bare
  `<select>`/`<option>`s, no caption chrome of its own), `rangeInput @l`
  (`<input type="range">`, the live bounded-quantity slider over
  `Cons l { current, min, max, step }`), the `progress @l` display
  (`Cons l Number`, fraction 0–1), the `output` status (the one fixed
  canonical row here, `[ event :: String ] → { | r }` — HTML's element for the
  result of a user action, shown in place since plain HTML has nothing
  self-dismissing), `input @l` and `textArea @l` (the
  focus-guarded text editors, `Cons l String` — no caption chrome of their
  own, a caption staying a sibling `label`+`staticText` merge), and the capture `filePicked @l`, a source over `<input type="file">`: `[ picked :: { name, size, mime }, none ]`, the user supplying the value and the platform unable to be shown one.
  **`focusField` is not on the application surface** (not re-exported
  from `PUI`): the leaf lift is design-system plumbing in every vocabulary,
  the plain-HTML floor included. **Shape is the type** (guardrails L3) — every
  exported component ends in one of the four row forms, each side a record
  or a variant — app-packaged controls included. A word that would need
  two shapes is two words: the pane is `provided` (emitters) or
  `inCase` (displays and editors), the list `foreach` (displays) or
  `anyOf` (emitters). Only the decorators
  `clWhen`/`attrWith`, like the oculars, keep the shape they decorate.

  **Structure computed from data is `PUI Web` all the way down** — no markup
  DSL — in two regimes:

  1. **Fixed structure, changing values** (grids, an SVG canvas): feed the
     structure as data through the retaining `foreach` and compute each
     element's content/attributes from its fed value — `text f`/`textAt @l` reading the row
     for content, `attrWith`/`attrAt @l` for style/coords, MDC3's `tile @l @"key"` to emit
     identity. Built once, updated in place: no wholesale rebuild, no `data-*`.
  2. **Structure that genuinely varies with the data** (markdown blocks): the
     closure builders `dynamic` (a whole component per value,
     `el ("h" <> show level)`) rebuilds per feed, and `each xs build` (a closure-known list pinned to `{}`) is built once, at registration, like every static.
     Each owns its container like `foreach`.
#### The design-system vocabularies

Five modules under **src/PUI/Web/** — `PUI.Web.MDC2`, `PUI.Web.MDC3`,
`PUI.Web.Shoelace`, `PUI.Web.Fluent`, `PUI.Web.Bootstrap` — proving bambik a
design-system **umbrella**. What they share, stated once:

- **Two sorts.** *Components* carry a model interface and are citizens of
  exactly one shape (the boundary reading — displays as assurance
  policies, sources as the seed generalized, user input as occurrences —
  is doc/displays-and-sources.md): `×→×` editors (text fields, `checkbox`,
  `toggleSwitch`, `slider`/`sliderLive`) and displays (progress/gauge), `×→+`
  events (`button` and its emphasis siblings, `fab`, `iconButton`, `menuItem`),
  `+→×` statuses (`snackbar`/`toast`/`messageBar`/`output`, each telling its case by its label and taking its fold), plus the
  **selectors** (`select`, `radioButton`/`radioGroup`, `segmentedButton`,
  `dropdown`) — `×→×` editors of field `l`, each in three words:
  the plain word's field holds the option itself (lifted
  by `PUI.Web.selectedAt`); the `…Unpicked` sibling's is a variant whose
  case `c` is the made choice, every other case showing nothing checked
  and a pick never taken back — a choice **owed** but not yet made
  (`selectUnpicked @l @c`, lifted by `PUI.Web.selectedUnpickedAt`); the
  `…Optional` sibling's is the same variant shape with a named none case
  `n` the face clears back to — a choice the user **may leave** unmade
  (`selectOptional @l @c @n`, lifted by `PUI.Web.selectedOptionalAt`;
  selects get an empty first option, Shoelace its clear button, radios
  and segments clear on a second press of the checked one via
  `PUI.Web.clearedOnRepress`). Unpicked and optional share a view model
  and differ in behaviour, so they are two words; consumers adopt the
  made case either way (the forms this passed through — a leaf silent on
  `Nothing`, a `×→+` picker, a selection-prism argument — are CHANGELOG,
  2026-09-23 to 2026-09-26).
  *Oculars* are shape-preserving decorators with no model of their own
  (`card`/`cardActions`, dialogs, lists, typography, elevations) — and a
  **surface ocular carries no copy config**: MD2 gives a card twelve optional
  structure classes and no heading, MD3's card element is a bare `<slot>`, so
  `card` is a plain `Ocular` in all five vocabularies. A card whose content
  is one model sub-record is not chrome but the **labelled group** `group @l`
  (MDC2/MDC3 — guardrails L3): `focusField @l` fused with
  the card surface, the label the field, the heading verbatim and the
  accessible group name (`role="group"`); the fusion criterion (a label fuses
  exactly where it does work `# focusField @l` alone cannot) is recorded at
  L3. The blind `card` is for content that **edits nothing**:
  order-form's summary, product-review's preview, meeting-booker's plan,
  loan-calculator's repayment figures. Editors never share a blind card
  (they share a sub-record), and an app never wraps itself whole in one: the
  surface an app is shown on is its **page's** — demo pages opt in with
  `<body data-surface>` and demo/page.js builds the vocabulary's card around
  the mounted demo, so an entry reads `body $ …`, never `body $ card $ …`.
- **Every leaf is label-indexed** (L3) and captions itself from that label
  verbatim; editors also stamp it as the host `name`. A leaf takes no options
  record: no call site writes `{}`; the few words with optional
  presentation have a primed sibling (MDC2/MDC3 `button' { icon }`,
  `listOf'`/`listOfAt' { selected }`), and the textareas, `iconToggle`
  and `drawer`, whose every call needs its record, keep it.
- **Leaf-echo protocols** are identical across all five: focus-guarded text
  fields (model updates never clobber the field being typed in, and the channel
  stays live), per-feed display echo (the `{}` answer to a feed and nothing
  at registration — inert to every gate, real to sequencing), selectors
  answering every feed with the row, emitters firing nothing inside a feed, and the replay-last-value
  protocol on emitters. The leaf-law bench (demo/laws/, one page per
  vocabulary, `scripts/smoke/tests/leaf-laws.mjs`) mounts every published
  component alone and checks Repetition and Answer per shape against the
  real DOM.
- **The `dimap` round-trip contract for editors** (stated in each module
  header): an editor bracketed by `dimap f g` behaves as an iso lens; lossy or
  failing conversions belong in the model (`settled` on the whole-row stage), never
  in a leaf bracket.
- **Same names and signatures wherever both catalogues have the concept**, so a
  screen changes design system by changing one import. A catalogue's honest
  exclusives and honest gaps appear under their own names — the per-module
  deltas below. Bounded quantities ride one row everywhere:
  `{ current, min, max, step }` as **model data from the seed** (`step` is
  `[ discrete :: Number, continuous :: {} ]` — a named two-state field, like every
  other), re-scopable at runtime, never UI config.
- **Every vocabulary is entered through its own `body`**,
  `PUI Web {} o -> Effect Unit` exactly like `PUI.Web.HTML.body`: it does the
  catalogue's page-level setup at mount — MD2's `mdc-typography` baseline,
  MD3's typescale stylesheet, Fluent's theme, Shoelace's icon base path — and
  then mounts; Bootstrap, needing none, names the plain one as its own. No
  vocabulary acts on the page at import time, and a twin's entry
  line switches design system with the rest of its import. The app bar and
  drawer stay oculars inside it: a shell is chrome, the root is the mount.

Per-catalogue deltas:

| Module | Basis | Deltas worth knowing |
| --- | --- | --- |
| `PUI.Web.MDC2` | `material-components-web`: documented markup + a foundation instance (`newComponent material.x."MDCX"`) wired through its documented properties/events; text fields write through the foundation's `value` so label float stays foundation-managed | the fullest catalogue: `indeterminateLinearProgress`/`indeterminateCircularProgress` are **statuses** (`[ started :: {}, ended :: {} ] → {}`, mirrored in MDC3: `action`'s progress slot dispatches the run's two occurrences, no model owns a `busy`, and a status owes the channel nothing, so the slot is outside the gated broadcast), `listOf` (a **dynamic collection component**, `listOf @l @k provided rowsOf item :: PUI Web { \| i } [ \| s ]` (the element row `r` the projection's signature, `Cons k key rest r`, `Cons l key () s`: the rows keyed by their field `k` — a named field of the row the projection builds, never a pick function, since the UI decides its view model — and a clicked row leaving as case `l` carrying that key) — keyed `foreach @k` retention, MD2 selected styling an optional `selected` predicate; `listOfAt @l @k @t` names a stored array field `t` instead of projecting), `dataTable`/`dataRow`/`dataCell`, `imageList`/`imagePane` (the channel-fed sibling of the static `imageListItem`), `layoutGrid`, `topAppBar`, `drawer` (permanent, with a **live nav slot**: nav and content are both fed the drawer's row, the nav's output is the drawer's, so a nav whose pick folds into the model hands it to the app's loop while the content shows the model beside it, photo-gallery), `tooltip`, `banner`, `tabBar` (the same-type selector with unconditional echo — the `looped`-ensemble citizen), `menu`/`menuItem`, `chipSet`/`filterChip`, `iconToggle`, `dialog`/`confirmed` (modal protocol: **open on feed, close on emission**; `confirmed @l` the `+→×` leaf telling its case as its title), `group @l` (the labelled model group — card surface + heading + `focusField @l` in one word, label stamped as the accessible group name; mirrored in MDC3) |
| `PUI.Web.MDC3` | Google's `@material/web` custom elements — a leaf is `element "md-…"` plus property/event wiring: no foundation classes, no hand-fused ripple/label chrome | structured to **mirror MDC2** (same helper shapes, same definition order). MD3 renames arrive as the catalogue does: the MD3 typescale (`displayLarge`…`labelSmall`), four emphasis siblings (`elevatedButton`/`tonalButton`/`outlinedButton`/`textButton`), `elevation1/3/5`, and **no `banner`** (MD3 dropped it). Catalogue entries `@material/web` lacks (segmented button, snackbar, card, top app bar, drawer, data table, image list, tooltip) are hand-rolled over the `--md-sys-*` tokens, each injecting its stylesheet once via `ensureStyle`, the `md-typescale-*` stylesheet adopted by its `body` at mount; pages need only the Roboto + Material Symbols fonts |
| `PUI.Web.Shoelace` | `@shoelace-style/shoelace` custom elements, Lit-based so no bind deferral | the MDC3 recipe verbatim. Exclusive: the star `rating` editor. Shoelace's own names where the concept differs — `textField`/`textArea` (no fill/outline split, plain `label`), `toast` (`<sl-alert>`), `progressBar`, `sliderLive` (`<sl-range>`). Page links the light-theme CSS from the CDN; icons from the CDN base path its `body` sets at mount. Typography is deliberately absent — Shoelace styles plain HTML, so the HTML oculars *are* the type scale |
| `PUI.Web.Fluent` | Microsoft's `@fluentui/web-components` v3; tokens set from `webLightTheme` by its `body` at mount, so pages need no CSS link; labels associate via `<fluent-field>` wrappers | exclusives `ratingDisplay` (read-only — the catalogue has no star *editor*, and this vocabulary does not invent one) and `messageBar`; type ramp `title3`/`body1`/`caption1` over `<fluent-text>`. **Caveat**: FAST binds a beat after DOM insertion and replays pre-bind property writes at bind, and its update queue is rAF-driven (starving in frameless headless sessions) — so the dropdown/radio-group leaves defer writes on a **timer** poll (`whenBoundDo` in Fluent.js) and finish the two starvable registrations themselves; the dropdown's options must be wrapped in `<fluent-listbox>` (v3's markup contract) |
| `PUI.Web.Bootstrap` | **CSS-only**: native elements dressed in documented classes (`form-control`, `form-select`, `form-range`, `btn btn-primary`, `progress`, `toast`, `card`, `list-group`, `badge`) — no component JS, not an npm dep; the page links the Bootstrap 5 stylesheet from the CDN | the only FFI is the toast's `autoDismiss` timer (what Bootstrap's own JS plugin would do). No commit/live slider split (`sliderLive` only; the label line carries a live numeric readout). `listGroup`/`listGroupItem`, `badge`; typography is plain HTML |

Internals (MDC2/MDC3): the live leaf is `focusField @l`-lifted — `focusField` is the
`Strong` field lens, so every editor is a **whole-row citizen**
`p { l | rest } { l | rest }` whose emissions re-attach the background the
lens retains (runtime completeness by construction; freshness rests on the
enclosing loop's re-broadcast) — with hand-fused chrome where abstract
labels can't flow through the merges' `Nub`, while all-chrome groups have
concrete rows and stay literal `RecordToRecord.do` merges of announcing chrome
(`staticText`/`staticHTML`/`static` at `{} → {}`). Code order = DOM order.
- **No canonical labels: leaves state business labels, adopters derive them** (L3).

  every canonical-row leaf is **label-indexed**: the business label is a visible type argument on the leaf itself — `filledTextField @"First name"`, `select @"Milk" cfg opts`, `button @"Submit order"` — so a merge operand or emitter states its row once, at the leaf, and nothing in application code ever says `value`/`clicked`/`event`.

  Business functions are **arguments of leaves**, never adopters: a display takes its read function, a status its business case and fold (`snackbar @"Flight booked" identity` — its case label is its copy; inside, the face over the label adopted `# atCase @l # folding @l f`), and an emitter emits its own case, the business outcome decided where the case is consumed. **Copy is a function, not a field** — a display whose content is copy takes its read function at the leaf and carries no label at all (`text progressLine`; a field shown as stored is named, `textAt @"title"`), so the screen's copy is one pure logic function under unit test and the view line names its writer (doc/research-copy-is-a-function.md). The adopters that once carried a function or a canonical row (`forCase`, `forCases`, `toCases`, `forField`, `forProperty`, `projection`, `projected`) are the CHANGELOG's precedents against any new one.

  The label also names **and captions** the component: every label-indexed leaf **stamps its label on its host element** — `name` on form citizens, `aria-label` on quantity displays — whose label is that accessible name and nothing else, the value arriving as a read function (the stamp invariant, stated at `OptCaption` in `PUI.Web` — whose second half is the accessible name, the caption **verbatim**: browsers compute names from rendered text, so faces whose catalogue styling transforms the caption stamp `aria-label` with it verbatim — MDC2's button family, tabs, segments and menu anchor, whose MD2 uppercase once computed `@"Count"` as "COUNT" — and so do faces the platform cannot name by itself: MD3's checkbox and switch sit inside a wrapping `<label>` whose association never reaches the input in their shadow root, so the switch stamps its label and the checkbox the rendered text of its content; `text` is outside the family entirely — no label at all, and under host diagnostics a bare `text` comment marker) — and every captioned leaf — editors' `floatingLabel`/`label` and the whole `×→+` emitter family (`button`/`outlinedButton`/`textButton`/`elevatedButton`/`tonalButton`/`fab`/`iconButton`/`menuItem`, in all six vocabularies) — defaults its caption to the label **verbatim** (`OptCaption` in `PUI.Web`, shared by all six vocabularies; the MDC modules add their own `OptLabelIcon`/`OptLabel`/`OptSelected` for their richer faces). **Nothing derives a caption from an identifier** (`humanizeLabel` is the CHANGELOG's precedent): a label *is* the copy it draws (the reason: the code stays as close to the running UI as possible — the words on the screen are the words in the code, so a line is found from the screen and the screen from a line), so it is written as such and is usually a quoted string, since human copy is no identifier (`filledTextField @"First name"`, `button @"Submit order"` drawing those words and emitting `[ "Submit order" :: _ ]`, quoted at every mention — `atCase @"Submit order"`, `match { "Submit order": … }`). Because a leaf's label is the model field it edits, **the business rows carry the same quoted labels** (`{ "First name" :: String }`), whose one syntactic cost is that a quoted label cannot appear in a **record pun** — the view model modules bind explicitly instead (`createPerson { "Name": name, "Surname": surname, people }`), while field access, accessor sections and update syntax are unaffected. **No demo passes an emitter `label:`**: where two buttons would share one handler under different words, the buttons are two business actions — each emits its own self-describing case and the fold block applies the one handler to both (checkout's `fold @"Next" stepTo`, `fold @"Back" stepTo`), so each button reads as what it does. For emitters `label:` is left only for a glyph-only face (`fab { label: Nothing }`).

  For **editors** the same rule holds, and **no demo passes a caption config at all**: the label carries the full copy, punctuation and units included — `filledTextField @"Start date (DD.MM.YYYY)"`, `sliderLive @"Amount (€)"`, `filledTextField @"Formula (e.g. =SUM(A0:A5)*2)"`, `filledTextField @"What needs to be done?"`. The type argument *is* the caption, so a leaf never states its copy twice. Selector **options** follow the same rule via `choice` (in `PUI.Web`): a choice **is its tag** — the case it stores and the copy it draws — so it states its copy once and nothing rides beside it — `dropdown @"Room" (choice @"Focus pod (4 seats)" <+> choice @"Boardroom (12 seats)")`. The options are `Choices w`, the tags in the order written over the closed row `w` of `{}` cases, built only by `choice @l` (one tag over its closed singleton row) and `<+>` (joining lists, uniting their rows), so the row a selector's field holds is closed by the list itself and its order is the order written — deliberately **not** the variant row's, which the compiler sorts alphabetically, while option order is a design decision (rooms by size, durations by length). A selector face runs over the tags and compares picks by tag (`tagged` lifts it to the field's variant), so no selector needs `Eq`; presentation a face draws beside a tag comes as a record labelled by the cases, exactly those (`PerChoice`) — `tabBar' @"Drink" { icons: { … } }` (MDC2/MDC3).

  An editor whose text is *derived* from sibling fields keeps the derived texts as model fields and normalizes them into each other with `settled` (temperature-converter holds both `@celsius` and `@fahrenheit` texts — a label is any string, so where a symbol *is* the conventional caption it is written as one — each field's stage running `# settled fromCelsius` / `# settled fromFahrenheit`, a failed parse leaving the sibling untouched)

  **A visible `@"…"` names a row label — a field or a case — and nothing else**: a title, a caption, a column heading, a tip or a static's text is a String value (`topAppBar "Espresso Bar"`, `confirmed @"Refund a customer" "Refund" refundStandard` — the caption, not the case or the fold), `columnHeader "Qty"`, `staticText "Hours"`, `drawer { title, subtitle }`), and a quantity display takes only its read function (`linearProgress elapsedFraction`); the one visible argument that is neither is a **declared row**, below.

  The closure of the label discipline is the **anchor invariant** (writing.md *The anchor invariant*; library-side face and rejected-design record in guardrails L3): every view line names exactly one semantic anchor — a model **field** (`@l` on an editor/selector), a business **case** (`@l` on an emitter, pane or status), a **named read function** (a display's content), or **nothing** (chrome) — line ↔ named symbol in the model or view model module, deliberately never line ↔ field (a symbol on an ocular was rejected: no model, nothing to anchor); and each anchor sits **in its own position**: a field or case as the leaf's type argument, a read function as a display's positional argument, so every leaf reads as a noun phrase — word, anchor, then its positional arguments (`snackbar @"Flight booked" identity`, `topAppBar "Espresso Bar"`) — and records hold only optional presentation or same-typed values whose names prevent a swap, never an anchor or a required value; a leaf never makes the application name its rows with vocabulary field names (L3's canonical-labels rule: it takes a read function, `imagePane developedShot`)
- **extras/** - the layer the shape modules stand on, extracted so `Data.Profunctor.Row.*` holds only what mentions a row, laid out **exactly as the ecosystem lays out its own**, and kept outside `src/` under separate source roots because claiming an ecosystem module name is a claim about what the module *is*. Everything here but `extras/variant` and `extras/row-profunctor` is **unreached by the library, the demos and the tests** (L14 would otherwise prune it) but stays in the build glob so it cannot rot; all of it but `Cont` is also non-row and non-carrier, mentioning neither `PUI`, a row nor a carrier.
  - **extras/profunctor/Data/Profunctor/{Resolving,Coresolving,Retaining,Coretaining,PointedCostrong}.purs** — one *class* per module beside `Data.Profunctor.Strong`/`.Costrong`, so the coined strengths and their co-strengths are separate files, stated positionally with `Tuple`/`Either` or at bare `a b`, no row in sight. Pure **complements of the ecosystem's own**: liftable into `purescript-profunctor` unchanged. `PointedCostrong` (`unfirstFrom :: c -> p (Tuple a c) (Tuple b c) -> p a b`, `unfirst` with its state channel started at `c`; laws yanking and sliding; instances `(->)`, `Cont` and `PUI m`) is the seeded `unfirst` the knot is stated against. None of the five has a row form (`Data.Profunctor.Row`, *What is deliberately not here*): `Resolving`/`Coresolving` are reached by `debounced`, the other three by their law tests alone.
  - **extras/profunctor/Data/Profunctor/Cont.purs** — the root's one **carrier**, and so its one member that is *not* liftable: the CPS profunctor `Cont r a b = (b -> r) -> (a -> r)`, the repo's only *pure* carrier of the row algebra — a timeless model where the merge gate is continuation nesting rather than a pair of `Ref`s. Its header inventories, **exhaustively** over every profunctor subclass in the repo and the ecosystem, which classes it validly inhabits (`Strong`, `Choice`, `Category`, `Wander`, `Acting`, `Cochoice`, `RecordToRecord`, `VariantToVariant`, `Monoid r => RecordToVariant`, plus the degenerate `Resolving`/`Coretaining`/`PointedCostrong`) and which it provably cannot (`Costrong`, `Coresolving`, `Retaining`, `VariantToRecord`, `Closed`, and the knot), each with its reason — so an absent instance is a stated impossibility, never an unwritten one, and those impossibilities are exactly why `looped` is a primitive. It builds (CHANGELOG, 2026-08-11), so a move in the `Data.Profunctor.*` layout that invalidates it is a compile error rather than silent rot.
  - **extras/lenses/Data/Lens/{Colens,Coprism,Shutter,Coshutter,Reel,Coreel}.purs** — one *optic* per module beside `Data.Lens.Lens`/`.Prism`, each carrying its type, its collapsed constructor and its `*E` existential encoding at arbitrary `s t a b`. `Colens`/`Coprism` are the optics of the *ecosystem* classes `Costrong`/`Cochoice`, which `profunctor-lenses` never built, so they too are liftable as they stand; `Shutter`/`Coshutter`/`Reel`/`Coreel` are coined class and optic alike, so each would travel with its class. Plus **extras/lenses/Data/Lens/Prism/Existential.purs** (`prismE`, the existential constructor of the ecosystem's own `Prism`, which `Data.Lens.Prism` does not export — it *extends* that family rather than shadowing it, which is why it is not named `Data.Lens.Prism`): the purest complement in the tree, since both the optic and its `Choice` are already the ecosystem's. The row layer reaches only the pure shapes' optics (`focusField` a `Lens`, `focusCase` a `Prism`); the coined and co- optics have no row form and are exercised as business optics in test/BusinessOptics.purs.
  - **extras/variant/Data/Variant/Case.purs** — the **value-level label read** `caseText` (the case label of a variant value, verbatim — `unvariant` + `reflectSymbol`, a composition `purescript-variant` never exported, so liftable unchanged; law `caseText (inj @l a) = reflectSymbol (Proxy @l)` in the header). The one extras module that is demo-reached and law-tested rather than parked: under L3 a case label *is* the copy it draws, so application code reads labels back with `caseText` instead of `match`-restating them (espresso-bar's summary, order-form's "Paying by cash", potluck's menu, meeting-booker's booked line, product-review's preview) — options are therefore labeled as the exact copy the line needs (`choice @"with oat milk"`, `choice @"cash"`), while a map that does real work (meeting-booker's `roomText` shortening, tic-tac-toe's glyphs, signup-form's sentences) stays a named copy function. The rule as applications read it is writing.md's *A label is read back, never restated*; `Data.Variant.Case` counts as a domain module, importable from logic and view alike.
  - **extras/row-profunctor/Data/Profunctor/{Row,Row/*,Acting}.purs** — the **row-profunctor algebra itself**, and a different claim from the two roots above: not anyone's complement but bambik's own invention, yet still carrier-agnostic (the algebra of merging labelled rows; `PUI` is one carrier that satisfies it, `(->)` another — it carries both **diagonal** merges, `RecordToRecord` and `VariantToVariant`, so their unit/associativity/symmetry laws run as pure equalities, while the two mixed merges stay `PUI`-only by necessity: a function cannot fabricate a case and `variantToRecord` needs retention). Every class in this root has a `(->)` instance, and every law here is value-testable; what a timeless carrier lacks — a beginning and feedback — is stated in `PUI`, not here. With this root out of `src/`, **`src/` holds exactly the carrier and its vocabularies** — `PUI`, `PUI.Web` and `PUI.Web.*` — so the split reads: `src/` is the UI library, `extras/` is the algebra it stands on. **Structural** (`Data.Profunctor.Row.Structural`, `withStructuralEq`/`withStructuralOrd`) lets a keyed collection index its keys by the values' structure, read through the ecosystem's `Foreign`, so it does not demand an `Eq`/`Ord` instance a hole cannot supply (L18).
  - All five roots are covered by the single glob `extras/**/*.purs` in spago.dhall's `sources` beside `src/**/*.purs`, and watched by scripts/dev.mjs. **Downstream caveat**: spago globs a git dependency as `.spago/<pkg>/<ver>/src/**/*.purs` — hardcoded, ignoring the package's own `sources` (the same reason the bootstrap must spell out the dependency list) — so modules outside `src/` are invisible to a consuming app. A tag carrying this layout therefore needs the app's own `sources` to add `.spago/bambik/<tag>/extras/**/*.purs`, which is why bootstrap.md's spago.dhall step carries that second glob; one glob covers all the roots, and it is tag-pinned, so it moves with `bambik.version`.
- **extras/row-profunctor/Data/Profunctor/Row/** - Row profunctors over `Record`/`Variant`: four shape modules, each carrying its **shape class** — the binary merge, the one genuine per-carrier primitive; no class carries a unit of its own, and the algebra names no unit but `Category`'s `identity`: three shapes reach theirs from it (the `{}` wire at the merge's row for `×→×`, operands sharing one input row; `identity @(Variant ())` for `+→+`; that wire entered from the empty variant for `+→×`, `lcmap case_ identity`), and `×→+`, the one no wire reaches, has none over the algebra — the carrier's silent component is its unit, law-tested on `PUI` — with qualified-do sugar (`bind`/`discard`). Everything kept is reached by a demo or a law test (L14); laws are stated in the module headers. **Type variables name their kind:** `r` is a record row (`{ | r }`), `v` a variant row (`[ | v ]`), `b` the background row of a `Cons` (`Cons l a b r`, `Cons l a b v`), `rl` a `RowList`; when a signature has several rows of one sort they are numbered in order of appearance and the result's row is bare (`Cons l a b1 r1 => Cons l a b2 r2`), a visible row argument keeps the same letter (`@r`, `@v`); plain types stay `a b c`, a focus type `f`, a Symbol `t`/`l`/`c`; the design-system modules follow the same rule for their leaves (`Cons l a b r`). **The four merges are the exception**: they name rows by side, not kind — `i` the input row, `o` the output row, numbered per operand, the merged row bare, a `RowList` the row's name plus `l` (`recordToRecord :: p { | i1 } { | o1 } -> p { | i2 } { | o2 } -> p { | i } { | o }`, `variantToRecord :: p [ | i1 ] { | o } -> p [ | i2 ] { | o } -> p [ | i ] { | o }`), as their per-side bundles in `Data.Profunctor.Row` do, so the four signatures read side by side.
  - **`RecordToRecord.purs`** (×→×) — merge `recordToRecord` (one input row + `OwnedRecordOutputs`; gated on `PUI`, zero-field sides pre-satisfied and inert — `{}` is always known and a contribution of zero fields is no contribution — and **released once per feed**: the broadcast is one step, so a feed changing several fields emits one fresh row, never a torn one); over ecosystem `Strong`: `focusField` (the type-changing field lens — the leaf lift: an editor lifted with it is a whole-row citizen, background retained and re-attached per emission). No co-strength row form: a loop's state is a model field the whole-row `looped` carries.
  - **`VariantToVariant.purs`** (+→+) — merge `variantToVariant` (`OwnedVariantInputs` — one handler per case — + outputs **one row** every operand is typed at, an equality like the `×→×` merge's inputs, no class; the inclusive union it had before could not be split back out of under holes, so two actions sharing an outcome had to name it on each line — CHANGELOG, 2026-10-07); over ecosystem `Choice`: `focusCase` (the value-level case prism, via `prismE`). No knot of its own: an app's loop is cut at its record junction, where the model can be declared.
  - **`RecordToVariant.purs`** (×→+) — strength `Resolving`/`resolve :: p a b -> p (Tuple a c) (Either b c)` (a loop step: `Left` = Done, `Right` = Loop; `PUI`-only instances — the branch is derived **from time**: emissions loop while the UI component is still moving, the last resolves at quiescence, so `coresolve (resolve g >>> seeded (Right c0)) ≈ debounced g` — literally: `debounced`'s body IS this seeded retraction, the loop channel primed by a `seeded` wire, since the raw composite is input-dead; window parameterized via `resolveFor`, whose 300ms default is the instance's quiescence quantum) and its co-strength `Coresolving`/`coresolve` (ties the loop: a terminating fold); merge `recordToVariant` (ungated broadcast, each emission forwarded as it is: the output is **one row** every operand is typed at, an emitter open on its output sitting in it by its label, an action by its declared outcomes); over `Strong`: `joined @l` (case `l` joined with the row its emitter was fed, `{ event, model }` — `first` around the source, closed to case `l` and widened into the block's row; `joined2 @l1 @l2` for a two-outcome source) and `kept`, the fold that keeps that row. No strength or co-strength row form: a stage is typed at one row, so no word narrows it to a sub-record with the rest escaping as a case, and a fold's state is a model field.
  - **`VariantToRecord.purs`** (+→×) — strength `Retaining`/`retain :: p a b -> p (Either a c) (Tuple b c)` (a Mealy/coroutine step; `PUI`-only instances — a stateless function can't retain state) and its co-strength `Coretaining`/`coretain` (ties the state channel: a productive unfold/generator); merge `variantToRecord` (the **copairing**: inputs owned, the output row **shared** — each operand releases the whole row and the merge forwards releases as they come, no gate, no retention, `Applicative m` enough — so a loop's per-case folds merge here, and a status, releasing nothing, is typed at every row); over `Category`: `unshown @l` (the faceless status owning case `l`, the status `PUI`'s `fold @l f` opens with; the fold itself is `PUI`'s, an optic over its status). No strength or co-strength row form, for the mirror reasons.
  - the collection lives one level up, in **`extras/row-profunctor/Data/Profunctor/Acting.purs`** (module `Data.Profunctor.Acting` — beside `Row/`, not under it: rows are the finitary μ-free fragment of the container grammar, `Array = μx. 1 + a×x` is one `μ` later). The class is the minimal carrier primitive `class Profunctor p <= Acting p where actedBy :: Ord k => (a -> k) -> p a b -> p (Array a) (Array b)`, keyed by the element row's **materialized identity field** (rows carry their identity; `Ord k` is the reconciler's Map-indexing requirement — identity semantics remain equality; keys must be unique and are never rendered). Laws in the header: **empty** (fed `[]` emits `[]`; nothing at registration), **singleton retraction** (yanking at the container), **gather gate** (`Array b` withheld until every element spoke, retain-last thereafter), **identity-follows-key** (stateful carriers), **wire** (`actedBy k identity ≈ identity`; composition deliberately *not* preserved — two reconcilers double-gate, so whole element pipelines are lifted, never stages). Positioned honestly: the *type* is the Strong/Choice closure under `μ`, the keyed retaining semantics is extra carrier structure beyond it (a derived traversal would rebuild where this reconciles). The module holds only the **pure algebra** — the class, `instance Acting (->)` (`actedBy _ = map`, so the laws are value-testable) and the label forms `acted @l`/`actedAt @l @t`. A `Maybe` action has no word: the view-model rule admits no `Maybe` field, so nothing could reach it (`optioned`, CHANGELOG 2026-10-14). The carrier machinery lives with the carriers, exactly like the merge instances: **`PUI.purs`** carries `class Hosting m node | m -> node` (what a stateful carrier contributes — instantiate one element component at runtime, plus placement: detach a leaver, restack survivors), the placement-free `Hosting Effect` instance (the probe carrier the `spago test` laws run on), the shared keyed reconciler, the one **generic** `instance Hosting m node => Acting (PUI m)`, and the five vocabulary forms; **`PUI.Web`** carries `instance Hosting Web (Array Node)` (DOM placement over the run of nodes an element's builder appended — one for a wrapped item, several for a bare group, none for `blank`; `appendChild` moves them, so identity follows the key). Design note: doc/collections-profunctor-algebra.md.

    The five divide the ground by **input** (everything at once → `×`; one entity at a time → `+`) and **output** (individual event; aggregate as joint decision; aggregate as running state). Key forms encode the key's ontology: a **label `@l`** on the `×`-members (identity is a materialized model field) and the **`{ key, value }` envelope** on the `+`-members (identity is the structural tag, arriving in the input, so no key function). Each `×`-member takes the **projection** that feeds it; the `+`-members take the projection producing the envelope.

    | Form | Shape | Behaviour |
    | --- | --- | --- |
    | `foreach @l` | `(a -> Array { \| r }) -> PUI m { \| r } { \| r } -> PUI m a a` | the **list of displays**: **keyed and retaining** reconciliation, matched elements re-fed in place, nodes moved with their keys; the elements' releases stay inside and the fed row passes on, so the list is a stage. Written trailing in a container ocular, `ul $ item # foreach @"id" rowsOf`; an array the model stores is the container action, `# actedAt @"key" @"cells"` (there is no `foreachAt`: over a stored array the two are the same type) |
    | `anyOf @l` | `(a -> Array { \| r }) -> PUI m { \| r } [ \| v ] -> PUI m a [ \| v ]` | the **emitting list**, `foreach`'s `×→+` sibling: the same reconciler over elements that emit, every element emission forwarded onto the shared channel, ungated, silent on empty — `tile @"Key entered" @"key" (textAt @"key") # anyOf @"key" keyPad`; MDC2/MDC3's `listOf` is this over picked rows; `anyOfAt @l @t` over a stored array |
    | `anyEdit @l @k` | `(a -> Array { \| r }) -> PUI m { \| r } { \| r } -> PUI m a [ \| v ]` | the **editing list**: the same reconciler over `×→×` elements (a row holding a toggle), a user's edit leaving as case `l` carrying the element row, an element's answer to the list's own feed dropped inside that feed — `×→+` by behaviour without leaning on the knot's guard; `( listItem $ … iconToggle @"Favorite" … ) # anyEdit @"Favorite changed" @"title" visibleMovies # joined …` (movie-browser), the fold writing the edit back by key; a stored array's editors are `acted` |
    | `acted @l` | `p { \| a } { \| b } -> p (Array { \| a }) (Array { \| b })` | the **container action** (Tambara for `Array`): the element is typed at its whole row, key included, and the carrier **re-sets** the key on every emission from the input row via the `Strong` state channel, so identity is unforgeable — whatever an element emits in the key field is replaced. Output is the **gather gate** — withheld until every element has spoken, then whole on any re-choice; editors answer every feed, so a collection of editors comes back whole on every feed and every edit (reorder). A stored array is named, `# actedAt @"id" @"Setlist"` (reorder-html) |
    | `dispatched` | `(i -> { key :: k, value :: a }) -> PUI m a b -> PUI m i { key :: k, value :: b }` | `+→+`: an unknown key instantiates a new case, a known key re-feeds exactly its instance, emissions leave tagged — the targeted-update/stream direction, no whole-array re-feed |
    | `accumulated` | `(i -> { key :: k, value :: a }) -> PUI m a a -> PUI m i (Array a)` | `+→×`, the keyed Mealy: grows per new key in first-appearance order, updates known slots, emits the whole array immediately, input-primed — the board/ledger shape |

    The `+`-members never detach or restack: absence of a key is no signal, so removal and ordering stay array-level concerns upstream.
  - **`Row.purs`** (module `Data.Profunctor.Row`) — the shared floor: the row-constraint vocabulary (`ExclusiveRows` + the runtime-evidence duals `DispatchableVariants`/`MergeableRecords` with `exactRow`, the owned sides bundled into `OwnedVariantInputs`/`OwnedRecordOutputs` — sharing is open (a shared side is one row every operand is typed at, written as one row variable in the merge signature, no class — open on the leaf, closed by the owning side, in all four shapes), responsibility is exclusive, evidence only on owned sides — so each merge signature is its owned side's bundle and its shared side's row; the owned sides also carry the `DisjointLabels` detector, which turns a duplicated label into a custom compile error naming the label), the one split `splitVariant` (the `+→+` merge's dispatch; a record input is shared, so there is nothing to own-split) and every **one-sided word** (a word constraining a single side is a side's, not a shape's, so no shape module imports another): the closed-singleton halves `atField`/`toField`, `atCase`/`toCase`, and the normalization `settled`, all `Profunctor`-only. The floor names no coercion: a stage is typed at one row, and a business function is typed at that row, its signature the view's hint with its tails filled (guardrails L18), so a demo never places a component at another row at the call site; the two one-line reshapings the library needs are written bare at their callers (`lcmap (const {})` in `PUI.Web.eventLeaf`; `rmap expand`, lawful by `Union`, in `PUI.action`, the one word whose row a function names, widening its declared outcomes into its block). The no-nominal-types-in-UI rule (every view-model type anonymous and structural) is stated for application code in `.claude/skills/developing-bambik-apps/writing.md`.
  - **`test/Main.purs`** — value-level `(->)` tests (`focusField`/`toCase`/`unfirstFrom`/`acted`) plus `fold`'s laws on the probe carrier, merge unit laws, gating, the point's answer law, the knot's trace triple and re-entry, and the trace quartet (`unfirst`/`unleft`/`coresolve`/`coretain`) on the `PUI` carrier via a probe harness; **`test/HelloShutterReel.purs`**/**`BusinessOptics.purs`**/**`RestaurantReel.purs`**/**`EntityEventExample.purs`** — `Shutter`/`Reel` as business optics.

### Composition Patterns

- `Semigroupoid.do` (`import QualifiedDo.Semigroupoid as Semigroupoid` in application code — the ecosystem's sugar under its own name) - data-flow pipelines; the block's unit is the wire, `identity`. Every line is a stage, a display included: it releases the row it shows. No bambik-owned block sugar (a `Pipeline.do` was built and withdrawn — CHANGELOG, 2026-10-11)
- `RecordToRecord.do` / `RecordToVariant.do` / `VariantToVariant.do` / `VariantToRecord.do` (qualified-do) - the four row merges
- label-indexed components - every MDC component is a citizen of one shape (`filledTextField @l` ×→×, `button @l` ×→+, `snackbar @l` +→×), so pipeline stages are written directly from components; `group @l` nests a sub-record's editors into the larger model
- variant editing - **the choice and its fields in the model**: a choice whose cases carry fields is a selector field and every case's fields side by side (order-form's `Fulfillment`: `Mode`, `Table`, `Time`, `Address`, `distance`), each field's editor `# inCaseAt @c @"Mode"` existing while its case is selected — what was typed in one case survives switching away and back because it is a model field; the sum, where the business needs only the chosen case, is a view model function of the record. A bracket keeping the other cases' payloads out of the model loses them on every re-broadcast (`bracketed`, CHANGELOG 2026-10-11): state the app needs is a model field (guardrails L14)
- conditional visibility - **case adoption, never a `Maybe`**: `provided @l <classifier>` for case-gated existence — a stored variant field — named by label on the pane's line (`# providedAt @"halted" @"phase"`, `# inCaseAt @"serving" @"display"`), typed by the model row — or, for **derived states**, one variant-returning classifier per rule family whose cases carry their panes' payloads (`# inCase @"taken" usernameStatus`, the classifier's cases its signature; calculator's `readout`, inbox's `messageView` converting `find`'s `Maybe` at the boundary), exclusivity by construction: conditional *data* (a pane whose content only exists sometimes) → `content # inCase @l <classifier>`; conditional *mode of a live editor* inside a `looped` ensemble → the editor pane `# inCase @l <classifier>` (order-form's fulfillment panes, flight-booker's return date, meeting-booker's attendees slider), never a payload pane folded back with an identity setter. `clWhen` stays predicate-driven — it toggles a class (styling), not visibility
- gated displays - live views as pipeline stages (slider readouts, summary lines, data tables): each rung renders per its policy and releases the fed row; read functions are typed at the fed row, their signatures the view's hints with their tails filled

### Separation of Concerns

- **Business Logic** - Row-shaped models, structural throughout, per the application code-style contract in **`.claude/skills/developing-bambik-apps/writing.md`** (see the note at the top of this file — it is the single normative statement, and demos are its executable form): no nominal types in UI, signatures the view's hints with their tails filled, folds as record updates, fold handlers in the Mealy step's own shape `payload -> state -> state`, no business literals in UI code, no pass-through fields, emissions carrying bare data. Below the UI sit plain functions and Aff actions (demo/nguis/order-form/OrderForm.purs), nominal types only where recursion (cells' `Expr` AST — rows can't express μ) or an ecosystem API (`Aff`, `Either`, `Milliseconds`) demands them, and business optics (Shutter/Reel) where state/loop semantics are needed (test/BusinessOptics.purs)
- **Design System** - Oculars in the vocabulary modules (`PUI.Web.HTML`/`SVG`, MDC2, MDC3, Shoelace, Fluent, Bootstrap); the type `Ocular p = forall a b. Optic p a b a b` is declared in **src/PUI.purs** beside its sibling `Action` — the optic transpose (fix the carrier, quantify the data), with its admission law in the header
- **Composition** - UI elements compose orthogonally to the row combinators

## Demo Structure

95 demos on 43 pages, one per app, registered in **scripts/demos.mjs** (the
single source of truth: directory + module + entry per design system, plus
the bundle file and page URL it derives, shared by the bundler, the dev
server, the holey twins, check-determined and the smoke tests). Two suites: **demo/7guis/** (the
[7GUIs](https://eugenkiss.github.io/7guis/) benchmark) and **demo/nguis/**
(popular showcase apps, mostly one combinator each).

**Conventions every demo follows** — stated here once, not per demo:

- **View/view-model module separation** (writing.md): the demo module is the
  view (design-system vocabulary + the view model module); the app's own
  module (`Counter`, in `Counter.purs`) holds the seed and the pure business functions
  and depends only on the domain — and **the view determines it**
  (guardrails L18): with a typed hole for every value the view imports, the
  compiler's hole list is the module's signatures with their tails
  unknown, the module filling them (`npm run check-determined` checks both, every demo). Twins share that module
  *verbatim*, so **a twin diff is view-only by construction**: it sits
  beside its views in the app's one directory
  (`demo/7guis/counter/Counter.purs` beside `MDC2.purs` and `HTML.purs`);
  hello-world (all view) has none. Any further module of an app is named
  the same way, its file the module minus the app prefix (crud's
  `PeopleServer.purs` is `Crud.PeopleServer`, order-dashboard's
  `ControlsMDC3.purs` is `OrderDashboard.ControlsMDC3`). Which module is the view model is the
  registry's to say (scripts/demos.mjs: `viewModel`/`viewModelFile` per
  demo), and the holey sweep, `check-view-model` and `check-determined`
  ask it rather than match a name.
- **Named module + entry, never `Main`** (`Counter.MDC2`/`counter` — the
  module carries the design system, since all 95 demos compile under one
  `spago build`, and the entry is the app's name in every twin).
- **One app, every design system**: an app is one flat directory
  holding a view module per design system, the file named by
  the design system alone and the module by both (`MDC2.purs` is
  `Counter.MDC2`, …, `HTML.purs` is `Counter.HTML`), the shared view
  model (`Counter.purs`) and one page whose header switches between them
  (`counter/?mdc3`); a plain-HTML app (hello-world, restaurant-menu) is
  one `html` variant, and a page with one variant shows no switcher. The
  registry lists an app as its design systems in page order and derives
  the rest from the app's name. The per-variant diff is the
  honest catalog mapping (typography renames per the Material migration guide;
  every vocabulary has `indeterminateLinearProgress`). A twin exists only
  where its vocabulary has the leaves the app needs: crud's pick is
  `listOf` (MDC2, MDC3), and the grids of cells, tic-tac-toe, calculator
  and color-mixer are MDC3's `tile` — no twin builds a leaf from a bare
  element (`clicked`, CHANGELOG 2026-10-15).
- **The app shape** is one loop through the four shapes, `( Semigroupoid.do
  … displays and editors …; RecordToVariant.do { emitters, panes, joined
  picks, an action joined with its button }; VariantToRecord.do { fold @l f …; ( progress # action @l f >>> VariantToRecord.do { outcome statuses, each with its fold } ) } ) # looped # with seed`
  — the model row the seed's signature, every event into its own
  memoryless `fold @l f`, closed to `PUI Web {} model`; what must show at mount
  stands before the ensemble, since a stage after the fold is fed only by
  events; `# with seed` for a flow with no loop; a knot with a
  load action before it takes no seed of its own —
  the load's outcome is the model and `body`'s one feed of `{}` runs the load
  (order-form, crud); a loop with no
  events has no fold; a form of several editors keeps a `# looped` of its
  own inside the app's loop (order-form); the drawer feeds its nav and its
  content the same row, the nav being the list, `joined`, and its fold
  (photo-gallery); an effect whose outcome must not overwrite edits made
  while it ran is `joined` together with its emitter, so the fold sees
  the model the outcome lands on (order-form's estimate).
- **Naming**: MDC2/MDC3 name the component vocabularies, modules, directories
  and UI labels; plain MD2/MD3 is reserved for the design-system specs
  (m2/m3.material.io) in prose.

### 7GUIs — all seven; five in all six vocabularies, crud and cells where their leaves are

| Demo | What it shows beyond the benchmark task |
| --- | --- |
| counter | the floor: one display, one emitter, one fold — `headlineLarge $ text countLine` (×→×, a display releasing the row it shows), `button @"Count"` (×→+), `fold @"Count" increment` (+→×, the case unshown — the heading already shows the count), closed by `looped # with freshCount` |
| temperature-converter | both fields in the model; non-numeric input leaves the other untouched |
| flight-booker | type-changing `select @"Flight type"` over an anonymous variant row; the booking action after its button, both outcomes told by sibling statuses folding the trip `submit` returns, `snackbar @"Flight booked" identity` and `snackbar @"Booking rejected" identity`, each case label the words on the screen — they own the action's two cases, so they close its outcome row |
| timer | `ticks @"tick"` replayed and folded by a total `tick`; `sliderLive` duration re-scoped at runtime |
| crud | **a load before the knot**: `( indeterminateLinearProgress # load loadPeopleCatalogue; capture @"People loaded"; ( editors; list and buttons; folds ) # looped )` — the load's outcome folded in as the model; `listOf @l` (MDC2, MDC3), its pick `# joined`; Aff catalogue actions over a stand-in server in its own module (`PeopleServer`), each typed at the actions block's six-case outcome row and returning its own two, every outcome folded once by its status |
| circle-drawer | **channel-fed SVG canvas** — built once, updated via `attrWith`; container-level `onClickedXY @l`; the diameter a bounded quantity in the model, its slider `# inCaseAt @"chosen" @"selected" # settled resizeSelected` — live-preview resize as a state invariant — and the canvas click `# joined`, an `adjusting` flag coalescing a drag into one undo transaction |
| cells | **channel-fed 31×27 grid** — ~800 cells built once, `attrWith` + `text` in place, the picked key via `tile @l @"key"` (MDC3 only); hand-rolled formula evaluator over an `Expr` AST (nominal, since rows can't express μ) |

The `-html` variants are the **plain-HTML floor**: one container `div` (so
case panes re-attach inside the demo's own DOM), label-indexed leaves like
every vocabulary's (`input @"Name" "text"`), captions as `label`+`staticText`
merges, native `select` and `output`.

### nGUIs — one combinator each

**Flagship.** order-form is the **four-shape showcase**: a load action →
one loop whose body is the four shapes — the `×→×` form in its own
`looped` (whole-row editor stages, sub-records nested as labelled groups
via `group @l` — Identifier, Customer, Fulfillment (the choice `Mode`, a
`tabBar`, beside the Table, Time and Address fields, each `# inCaseAt @c
@"Mode"`), Payment (Total beside Method and Paid) and Kitchen (the
remarks), each label the field, the heading and the accessible group name
at once), the live summary on a blind `card`, the one surface that edits
nothing → a `RecordToVariant.do` of event buttons, Estimate distance
among them (shown only for Delivery, `# provided @"Delivery" …
deliveryAddress`) and the three actions — estimate, submit, receipt —
each `joined` with its button (the submit's two outcomes `joined2`) → the folds (`+→×`), one per case, the
action outcomes told by statuses that fold `kept` (`snackbar @"Order
submitted" kept`, `snackbar @"Order rejected: no total" kept` — the rejection's reason is its case). The **in-form Aff action**: the estimate's
button and action are `joined` together, so its outcome is paired with
the model it lands on and `fold @"Distance estimated"
setDistance` sets only the distance — an edit made while the estimate
ran is kept; `settled staleDistanceForgotten` on the address drops an
estimate made for another address.

**One combinator each** (the knot and the focus pair get a
focused demo apiece):

| Demo | Combinator / point |
| --- | --- |
| auction | `settled` as a running invariant — the highest bid is a model field (`top`), raised by the slider stage's normalizer (`sliderLive @"Your bid ($)" # settled raiseTop`), where a hidden feedback channel once held it |
| checkout | a wizard whose step is a model field, edited by a **packaged selector** — the demo ships `Checkout.StepperMDC2`/`StepperMDC3`, `stepper @"step" (choice @"Cart" <+> choice @"Shipping" <+> choice @"Payment")`, a `selectedAt @l` leaf over `selectorWiring` whose face is Back · dots · Next, the step order the option list's (no catalogue has a stepper: MD1 had one, MD2/MD3 and the other three do not); panes and stepper in a `looped` of their own so a step reaches the panes, the step panes `# inCaseAt @c @"step"` off the stored field, where a hidden terminating fold once looped the step beside the model |
| payment | an **action that retries** — the flaky gateway is retried inside `chargeFlaky`'s `Aff` until approved, the outcome one `charged` case into the fold, the attempt counted a payload the status line reads. A nested variant knot once did the retry |
| ticket-dispenser | the floor of the `+→×` shape — "take a number" replays the model and `fold @"Take a number" issue` advances the counter, a model field (`next`), where a hidden unfold once resumed it. Also the **display pane** showcase: state is a payload-carrying variant field (`display`), so the number and hint panes are pure case adoption off the stored field (`text ticketLine # inCaseAt @"serving" @"display"`), each line reading the number out of the model |
| parcel | a **reusable sub-form over a sub-record** — `group @"Address" addressForm`, the form a named component over its own closed row, the model nesting the address it edits |
| cashbox | `confirmed` as interception: outgoing money detours through a confirmation dialog owning its button's case in the fold block and folding on confirm (`confirmed @"Refund a customer" "Refund" refundStandard $ body1 $ text refundLine`), incoming posts straight to its fold; every button replays the till, so every handler is `{ balance :: Number } -> { balance :: Number }` with its amount baked in (`refundStandard`) |
| potluck | `acted` (the container action) — per-guest dish selectors under one model, each `segmentedButtonUnpicked @"Dish" @"chosen"`; the table's state is the business classifier `menuState` (`complete` with the dishes, `waiting` with the guests still choosing), each case an `inCase` pane — the waiting pane names who is left, the menu prints once the table is complete |
| departures | `dispatched` (+→+ keyed input) — rows appear on first mention, re-feed in place, tagged output drives a last-update line; the board is fed by the `clock` source, so there is no loop, fold or seed |
| scoreboard | `accumulated` (+→× keyed input) — board grows to its key set, points update in place, whole array drives the standings; fed by the `clock` source like departures |
| reorder | keyed reconciliation + `acted`, the container action, as the collection editor — a playlist keyed by track id, element output row excluding the key (the carrier re-attaches it); Rotate and effectful Shuffle move each row's DOM node with its track, so tick, title and focus follow; its plain-HTML twin lays the setlist out as a `dl`, each track a bare `dt`/`dd` pair, so the **run of nodes** an element appended moves with the key (scripts/smoke/tests/node-runs.mjs) |
| here-and-now | **sources** (MDC3 + HTML): four facts the app cannot seed — `geolocated @"position"`, `online @"connection"`, `clock @"time" clockPeriod`, `colorScheme @"scheme"` — merged `RecordToRecord.do` into one row the gate releases at registration, four copy lines reading it; no `looped`, no `with`: a knotless flow fed `body`'s one `{}` |
| incident-report | **a capture** (MDC3): `geolocated`, `clock` and HTML's `filePicked @"Photo"` merged into the report row, a button replaying it and an action filing it, told by a snackbar — a report cannot be filed torn, the gate being the proof |
| order-dashboard | **custom components** (MDC3-only): the demo ships its own `OrderDashboard.ControlsMDC3` module — five label-indexed display controls + a `board` ocular, each taking its read function like the library's own displays (`statTile "Orders placed" ordersCount`, the label stamped as the tile's accessible name), including the packaged-collection-display protocol (`leaderboard`, a `foreach` passing the row on); the model holds only the order stream, every tile a function of it |

**The rest**, grouped by what they exercise: todo-list (`listOf` toggle, `clWhen`,
`segmentedButton` filter), tip-calculator (all-`×→×`, sliders, gated money
readouts), quiz (`provided` panes over one `quizPhase` classifier,
`linearProgress`), tic-tac-toe / calculator (**channel-fed `foreach` grids** — tic-tac-toe also the reset as a restart, `openingPosition` the seed and `reset @"New game" openingPosition` —
cells built once as `tile`s (MDC3), key emitted and `# joined`, folded by the fold),
markdown-previewer (`filledTextArea` + injection-proof preview as recursive
`PUI Web`: `dynamic …` over element oculars, the line after the editor cell, since structure
genuinely varies per block), stopwatch (`ticks` folded by a `tick` that steps only in the `timing` case of
a stored phase variant — a Boolean nobody edits as a Boolean is a phase — with
`# provided` button panes replaying the model and a `foreach` lap list after the folds),
shopping-cart (`dataTable` over `foreach`), password-generator (effectful
`action` returning the model), color-mixer (`sliderLive` channels
driving an `attrWith` swatch), signup-form (`debouncedTextField` username check plus two
variant-returning classifiers via `provided`, replacing five `Maybe`
projections — exclusivity by construction), photo-gallery (`imagePane`, the
channel-fed gallery: a retaining `foreach` over the pictures rather than a
wholesale rebuild), inbox (`listOf` + `dialog` + `banner` — the banner an
ocular over a static shown while the inbox is empty, `( banner $ staticText
"Inbox zero!" ) # inCase @"empty" inboxState`, and the demo whose MDC3 twin
shows the honest catalog gap, MD3 having dropped `banner`, so the twin shows
the line as plain typography; also the **determination pilot**: stored fields named by label (`listOfAt … @"messages"`, `providedAt @"confirming" @"deletion"`), so the view's 17 typed holes report one shared model row whose named fields are fixed — `deletion :: [ confirming :: t2 | t3 ]`, `messages :: Array { id :: t0 | t1 }`, the picked key `t0` too — and the next message id derived from `messages` instead of stored, a choice the tails leave to the model), movie-browser (`tabBar` + `filterChip` filters over one
`visibleMovies` projection, a `foreach` of rows whose in-row `iconToggle`
folds back `# joined` through the fold; the MDC3 twin honestly drops the MDC2
selected-row class, the favorite state riding the toggle),
weather (Aff service with a canned per-city delay), ticker (the **scope** showcase: a `ticks` and a `button # action` inside a `providedAt @"watching" @"phase"` pane — parked, the clock skips its beats and a quote settling meanwhile is dropped; scripts/smoke/tests/scope.mjs walks it), hello-world
(`body $ staticText "Hello, World!"` — the 5 kB bundle floor).

**Vocabulary showcases.** restaurant-menu is the plain-HTML one (the HTML
vocabulary alone: element oculars, `cl`/`:=` decorators, `each` from data,
no seed at all; the fine-dining look is ordinary CSS). espresso-bar is the MDC3 one
(with an MDC2 twin generated in reverse, so its page opens on MDC3). One
per non-Material vocabulary, each a page of one design system, as
order-dashboard (MDC3) is too: product-review (Shoelace's
exclusive star `rating`), meeting-booker (Fluent; also the **no-defaults
showcase** — nothing pre-picked, `…Unpicked @"chosen"` selectors
over named two-case fields seeded `.unchosen {}` beside an optional catering
(`dropdownOptional @"Catering" @"ordered" @"none"`, clearable back to none), no `Maybe` in the booking, the
attendees a bounded quantity *in the model*: the slider exists only once a
room is chosen (`# inCaseAt @"chosen" @"Room"`) and the room dropdown
re-scopes its bounds as an invariant (`# settled seatsInRoom`), so an
incomplete meeting is unbookable by construction), loan-calculator (Bootstrap, all
`sliderLive`).

Verify with `npm run smoke` (scripts/smoke/, headless-Chrome CDP: the leaf-law
bench, the every-demo mount check and the carrier-only laws — no per-demo walks).

## Key Dependencies

- `profunctor-lenses` - Profunctor-based optics
- `foreign` - read a plain value's structure for the structural `Eq`/`Ord` (`Data.Profunctor.Row.Structural`), so the algebra keeps no JavaScript of its own
- `qualified-do` - Syntax sugar for profunctor composition
- `material-components-web` - MDC (Material Design 2) JavaScript library
- `@material/web` - Material Design 3 web components (custom elements, used by `PUI.Web.MDC3`)
- `@shoelace-style/shoelace` - Shoelace/Web Awesome web components (used by `PUI.Web.Shoelace`; pages link its light theme CSS from the matching CDN release)
- `@fluentui/web-components` - Fluent UI v3 web components (used by `PUI.Web.Fluent`; theme tokens ship in the bundle)
- Bootstrap is CSS-only and **not** an npm dependency — `PUI.Web.Bootstrap` is native elements + classes, pages link the Bootstrap 5 stylesheet from the CDN
