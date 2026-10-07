# Bambik

## Overview

A prototype PureScript library implementing **Profunctor User Interfaces** - a novel approach to declarative UI development. The key insight is that profunctors unify optics (data structure navigation) and arrows (data flow), making them ideal for composable UI development.

**doc/guardrails.md is normative**: the strict MUST/MUST-NOT rules for the library and for applications built on it, plus the admission test every proposed feature passes (derivation → laws → subsumption → honesty → reachability → green stack → sync). Its L15 makes the demo suites the compatibility contract: any library change, however internal, must leave every demo compiling, bundling, and behaving correctly — verified by running `spago build`, `spago test`, `npm run bundle-demos`, and `npm run smoke` in full, with demos edited only when the demo itself is the subject of the change. Consult it before adding, changing, or accepting any combinator, class, component, or demo idiom — a change that violates a guardrail is wrong even if it works. Its **L16 import tower** governs imports: only the algebra layer (`PUI`, `Data.Profunctor.*` in bambik) imports the ecosystem's `Data.Profunctor`; vocabulary modules (design systems, `PUI.Web.HTML`/`PUI.Web.SVG`, packaged controls) build from the carrier + the re-exported vocabulary; application code imports neither the ecosystem algebra nor carrier internals, and `grep "import Data.Profunctor (" demo/` stays empty. Its **Part II is a pointer, not a rulebook**: the code-style contract for application code — demos included — is stated once, in **`.claude/skills/developing-bambik-apps/writing.md`** (its *Code style* section: layout, types and values, business functions, wiring). Read that file before writing or reviewing any demo or app module; nothing here or in guardrails.md restates it, and a change to how applications are written edits writing.md and the demos. The deployed restatements are exactly two, both deliberate (deployed HTML cannot read the skill file): the demo pages' `#code-style` note in demo/index.html, and **demo/workflow.html** — writing.md's *Writing order* replayed as a real session (verbatim compiler responses, captured with the pinned compiler; linked beneath every demo's source box). Re-read both against writing.md whenever the contract changes.

## Building

Do `export PATH=$PWD/node_modules/.bin:$PATH` and then `spago build` (tests: `spago test`). 

### PureScript forked compiler

Note the repo builds with the forked PureScript compiler pinned in `package.json` (variant row sugar `[ l :: T | r ]`, `.label` constructors — see doc/variant-sugar.md), so `npm install` first. Nothing in the toolchain depends on a local checkout, so a plain clone builds on any Linux x86_64 machine: the compiler installs from its **GitHub release** (`erykciepiela/purescript` tag `v0.15.16-variant.9`, built from branch `variant-type-sugar` — the release also carries the bare `purs` binary as an asset, and package-lock.json pins the tarball's integrity hash), and the `Prim.Variant`-patched variant library is an ordinary git package in packages.dhall (`with variant.repo`/`.version` → `erykciepiela/purescript-variant` tag `v8.0.0-prim-variant.1`, branch `prim-variant`: `Data.Variant` re-exports the compiler's built-in `Prim.Variant.Variant` instead of declaring its own, so the sugar and `inj`/`on`/`match` share one type). External applications consume bambik as an **ordinary spago git package** pinned to a release tag (`with bambik = { dependencies = [ … ], repo = "https://github.com/restaumatic/bambik.git", version = "v0.1.1" }`) — spago clones the repo and globs `src/**/*.purs`, so no checkout sits beside the app and the demos/docs ride along under `.spago/bambik/<tag>/`. The entry must spell out the library's dependency list (spago does not read a git package's own spago.dhall), but **nothing in the repo duplicates that list**: the bootstrap procedure fetches it from the pinned tag (`raw.githubusercontent.com/restaumatic/bambik/<tag>/spago.dhall`) when it writes the app's packages.dhall, so adding a dependency here needs no companion edit anywhere. A library release is a tag on this repo. The standalone bootstrap flow lives in `.claude/skills/developing-bambik-apps/` (bootstrap.md, which carries the scaffold as inline file contents rather than a stored template), self-contained and copyable out of the repo. Beside bootstrap.md/writing.md/building.md the skill carries **vocabulary.md** (the situation-indexed lookup index into writing.md and the module headers) and **walkthrough.md** (flight-booker read line by line) — pointers, never a second statement of a rule.

### Development loop - watch mode

When introducing code changes, keep `spago build -w` running in the background and read its output after each edit (~0.7s incremental) instead of one-shot `spago build`s per change — plain `spago build -w` covers library, tests, and the demos. Caveats: spago -w reads stdin and dies on EOF, so keep stdin open (never `</dev/null`); run only one watcher over the shared `output/` at a time. Parallel subagents sharing the repo must serialize compiles (`flock <lockfile> spago build`) — and when a serialized build fails, check the per-module `Compiling` lines for *whose* module failed before assuming it's yours. It's recommended to use git workspaces in such cases.

`npm run dev` serves **every** demo from one server at `http://127.0.0.1:1234/`, rooted at `demo/` in the same folder layout the deploy scps to the remote host — local `/7guis/counter-mdc2/` is `/bambik/demo/7guis/counter-mdc2/` there, so every relative link and asset path resolves identically in both places (the server hosts nothing but the demos, so it carries no `/bambik/demo` prefix of its own). The root landing page (demo/index.html) links the two suites. Narrow the bundling to what you're working on by name or set: `npm run dev counter-mdc2 cells-mdc2`, `npm run dev nguis`.

  Auto-rebuild and browser auto-reload throughout (~2s per edit; scripts/dev.mjs — one mtime-polling watcher over src/ and demo/ driving two paths by file type: a `.purs`/`.js` edit runs an incremental `spago build` and esbuild's own watch over `output/` rebundles the affected demos and reloads once the new bundle lands, while a page's `.html` has nothing to compile and reloads immediately — both are handled independently, so an edit to each in the same tick gets a reload *and* a rebuild; reloads reach the browser over an SSE endpoint at `/esbuild` using esbuild's own protocol, injected via the bundle banner so demo pages need no dev-only markup).

  Polling rather than `fs.watch`: inotify costs an instance per directory and Node hits its per-process ceiling well before the ~40 source dirs here, while `recursive: true` silently delivers no events at all on ext4.

  Runtime emission trace: set `window.__bambikTrace = true` (or `localStorage.setItem("bambik-trace", "true")`) in the browser console to log every propagation decision — stage-to-stage flow, `looped` re-feeds/swallowed echoes, and gate-withheld emissions with the sibling fields they wait for. Independent of the trace flag, every knowledge gate carries a **starvation watchdog**: a gate that withholds and is never fed within 3s prints one `console.warn` naming the gate and (for the record merges) the exact missing fields, with the fix (`seeded`/`announce`, or looping the state through the model) — and the browser sink logs the missing fields' stamped host elements beside the message (found by `name`/`aria-label`, clickable in DevTools), so an unprimed gate is a named failure pointing at its place on the page, not a blank screen (and an unprimed *entry* is now a compile error: `body` demands input `{}`) (off until a carrier adopts the host switches — `PUI.Web.adoptHostDiagnostics`, called at `body`/`runComponentInNode`, so a headless `spago test` is silent; opt out with `window.__bambikNoWarn = true`).

Verification stack: `spago test` (first the reading-order demos' view model claims — `<Demo>ViewModelTest.purs` beside counter's, temperature-converter's, flight-booker's, todo-list's, checkout's and order-form's view models, each a pure list of named claims, listed on their pages after the view model — then value-level law tests over probes — merge units, gating, exactness, the trace quartet, plus the audit laws of doc/observational-semantics.md: the seeded retractions and the three raw `×`-side deadlocks, `Looping`'s yanking/conjugation/idempotence with the `dimap f f` counterexample, the named `Strong` and interchange deviations, merge symmetry and mixed-merge associativity, the container action's wire law, the `(->)` diagonal merges as pure equalities, and the observation-level laws — boundary interchange on the nose with its exact common stream, one-feed-one-release (a two-field broadcast releasing once with no torn row; nested merges releasing once; and the disjoint-operand shape it is reachable from — two `focusField @l` editors merged in parallel is a **type error**, `OwnedRecordOutputs` wanting disjoint labels where a whole-row citizen claims the whole row, which is why this law has no demo-level test; plus the input-side prediction — the feed law belongs to **inclusive** record input, so `+→×`, whose input is dispatched one case to its one owner, carries every ingredient of the torn row except a broadcast and provably cannot tear), `⊑`-monotonicity of `>>>` and `⊗`, the container action's inner-surface laxity, `bracketed`'s section–retraction on the order-form pair, the Ocular admission law and its failure for a capturing decorator, `announce`'s naturality — and **test/Exhaustive.purs**, the bounded exhaustive check: the 34 laws (symmetry, associativity, both units, `⊑`-monotonicity and projection's input half at all four shapes; exactness and conformance to `PUI.Gate`'s pure step at `×→×`; arming at `×→+`; feed-idempotence and one untorn release per feed at `×→×`; and the container action as the same gate over runtime labels — `acted`'s conformance to the step rekeyed per feed shape, its `⊑`-monotonicity, the wire law, feed-idempotence and one untorn release per feed with `[]` the empty array's) over **every** script to length 6 (two operands or elements) or 8 (three) with a fresh token per event — complete by the data-independence argument of doc/observational-semantics.md §9, about 50 s of the run); `npm run smoke` (scripts/smoke/ — the committed headless-Chrome CDP harness: serves the repo, launches an isolated throwaway Chrome, runs scripts/smoke/tests/*.mjs. **Only what the laws cannot reach lives here** (2026-09-26: the per-demo walks were deleted — an app's wiring is implied by the laws and its own code): the **leaf-law bench** (tests/leaf-laws.mjs over demo/laws/: every published component alone, Repetition and Answer per shape, the platform's own input on every editor — typing and arrow keys and mouse clicks through CDP, picks for selects — storing a changed row that keeps the rest, every status showing its event, every `…Optional` selector clearing to its none case with nothing left checked, emitters replaying on click), the **mount check** (tests/demos-mount.mjs: every registered demo mounts, throws nothing and logs no warning or error past the 3s starvation watchdog, and is dressed by its vocabulary's `body` — about 40 s for all pages, opened ten at a time), the **carrier-only laws** (tests/acting-laws.mjs: the container action's keyed reconciliation, identity following the key in the real DOM), the **axe-core stamp audit** (tests/a11y-laws.mjs: the name-and-reference rules over `#demo-column` on the counter twins, order-form and both espresso-bar twins — the stamp invariant as someone else's checker), the **tooltip walk** (tests/tooltip.mjs: hover shows, leave hides, and leaving after a click still hides, on both Material twins) the **textarea overflow** regression (tests/textarea-overflow.mjs), and the **holey sweep** (tests/holes.mjs, guardrails L18: every demo's holey twin — its `*Logic` modules stubbed to bare untyped `hole`s by scripts/holes.mjs, every other module unchanged over the real library, built under holey.dhall into the gitignored `.holey/` by `npm run bundle-demos` — mounts clean from its view alone and reaches no hole when every control is clicked, pointed at, typed into and picked; `npm run check-holes` regenerates and runs just this); the value-level `Acting` laws (empty announces `[]`, singleton retraction, gather gate, keyed retention) run in `spago test` on `PUI Effect` probes; bundle the demos first, filter with `npm run smoke -- <name>`); `npm run check-view-model` (scripts/check-view-model.mjs — the view-model rule of writing.md's *Types and values* made mechanical: no `Maybe` field and no `Boolean` field off the Boolean-editor allow-list in any demo view model module); `npm run check-determined` (scripts/check-determined.mjs — L18's determination half made mechanical: every demo's view compiled with a typed hole for every imported value, the compiler's hole list read back one declaration per round, failing on any unknown, on any exported view model signature that is not its hint verbatim, and on any export no view imports (2026-10-03; a name two lines report at different rows is the one `forall` case and is not compared); `node scripts/check-determined.mjs <demo> …` names demos, about a minute for all; `--write` writes the signatures instead of comparing them — each the hint in the layout of scripts/signature-layout.mjs, a missing one inserted, an unwritten name appended over a runtime `hole`); `npm run api-docs` generates the browsable API reference into `generated-docs/md/` (gitignored) from the module headers — the single source of truth for combinator contracts.

## Building & Deploying Demos

1. Verify the forked compiler: `node_modules/.bin/purs --version` must report `0.15.16 [development build ...]`; if it shows stock `0.15.15`, run `npm install` (stock purs fails with "Module Prim.Variant was not found").
2. Bundle for deploy: `npm run bundle-demos` (minified, all demos; `node scripts/bundle.mjs <name|set>` for a subset) — use `npm run dev` for watch mode, not for deploys. **Every** demo is a named module (`OrderForm`, `Counter`, `Cells`, `TodoList`, ...) entered at its own function (`orderForm`, `counter`, `cells`, `todoList`, ...) — no module is `Main`, so they all compile together under one plain `spago build` — and each bundles from the shared registry in **scripts/demos.mjs** (the single source of truth for directory + module + entry, shared with the dev server), which synthesizes the esbuild entry per demo because `spago bundle-app` can only call `Main.main`.
3. Deploy: `npm run deploy-demos` — scps demo/index.html, demo/workflow.html and both suite directories to host `xyz` (root@erykciepiela.xyz, see `~/.ssh/config`) at `/var/www/html/bambik/demo/`.
4. Verify: `http://erykciepiela.xyz/bambik/demo/<d>/` returns 200 (plain HTTP only).

Every demo page is pure structure over **shared chrome**: `demo/page.js` (loaded as `../../page.js`) fetches the listing named by `<body data-source="CounterMDC2.purs">` (space-separated filenames render several listings — the first fills the existing box, each further file gets a filename heading + listing, the header readout sums the sizes; order-dashboard-mdc3 shows its app and its packaged controls module this way), fills the header's source/bundle size readouts, folds every long type row repeated in a listing after the view's into a chip of its labels (click to expand, "unfold all" restores the text exactly), notes the vocabulary words the view imports beyond the earlier demos of README's reading order (each linked into vocabulary.md), and groups the running demo with its tracing note into one `#demo-column` — on a page marked `<body data-surface>`, inside the vocabulary's card surface built by page.js (the app's surface is the page's, so no app wraps itself in a card) — the demo mounts into `<body>` at runtime with no marker class of its own, so it cannot be wrapped in static markup and is collected once, then the observer disconnects. The two notes that used to be pasted into all 34 pages now live once in **demo/index.html** as `#code-style` and `#tracing` sections, linked from beneath the source box and beneath the running demo respectively; the source-box note also links **demo/workflow.html**, the writing-session showcase.

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

The rows a pipeline operates over hold **state, not copy** (guardrails L17): **copy is a function, not a field** — a display whose content *is* copy takes its read function at the leaf and no label (`text balanceLine`, `text _.title`), the function living in the view model module, so the view line names its own writer and the screen's copy is unit-testable in `spago test`. A display that renders a *number* takes a read function too (`progressBar elapsedFraction`) — a fraction is *derived*, and derivation is the same act as formatting — keeping its label as the **accessible name** only; quantity *editors* are untouched (`sliderLive @"Duration"` still names its field). `settled` is left with invariants among *edited* fields, and in the demos every surviving `# settled` sits on an editor. doc/research-copy-is-a-function.md is the rationale (it partially reverses doc/research-presentation-model.md, keeping its testability motivation and its `settled` half). Fixed copy is **static or constant** (writing.md): a static is on screen before and regardless of any data and is a value, never a label (it names no field and no case): `staticText "Hours" :: PUI Web {} {}` (2026-10-07; a type `staticText @"Hours"` before, with a value-level `staticString` beside it, now folded into it), never a model field; a constant shows only through data (a sentence's glue, a pane's message) and lives in a copy function (`text faultLine # shownWhen @"faulty" readout`).

### Key Source Files

- **src/PUI.purs** — the core profunctor and the carrier-independent algebra's
  hub. Instances: `Profunctor`, `Strong`, `Choice`, `Semigroupoid`, `Category`
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
  is why `Looping` is its own class while the `+` side needs none.
  **One knot, 2026-10-06**: a loop through the four shapes is cut at its
  record junction — `looped` (the model re-enters whole, `Looping`'s
  method), closed by `with` and the model its first stage takes
  (`# looped @( … ) # with seed`); an event re-enters through its fold.
  A variant knot (`cycled`, `Cochoice`'s derivation, 2026-10-05) lasted a
  day: a loop cut at a variant junction was a second way of writing the
  same loop whose model no line of the view could declare (the knot's row
  is the event variant, the model a payload inside it), so it bought
  only the reuse of one operand (crud's load, tic-tac-toe's New game) at
  the price of `fold`'s second row argument. The field-level seeded forms
  `feedback @l`/`folding @w @l`/`unfolding @w @l` and the exits-typed
  `iterate` are DELETED too: a looped state is a model field, a shape
  change inside a loop is an explicit stage (`fold`, `toCase`), never the
  knot's own conversion; the co-strength classes and their optics stay in
  extras as the value-level laws.
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
  input shape how it is discharged), §3.2's account of what a `×→×` gate
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
  laws are about **shape**, and since 2026-09-15 every published component
  has the shape of its behaviour: `bracketed @l` lifts its variant editor
  into the record field it edits (a variant at channel position is an
  event, inside a field a value), and every occurrence source — `clicked
  @l f`, `listOf @l @k`, `onClickedXY @l`, HTML's `button @l` — emits as
  case `l`, so no record-shaped source exists; what the type still cannot
  enforce is that a `×→+` leaf never emits inside its own feed, which is
  the vocabulary provider's conformance.
  **Stateful
  instances require `MonadEffect m`** — `Strong`, `Choice`, the trace
  quartet, `Category`'s wire, `Seeding`, `Looping`, the gated merges and
  the stateful combinators allocate their `Ref`s in the construction monad
  at instantiation, the phase state belongs to; the stateless ones
  (`Profunctor`, `Semigroupoid`, `Cochoice`, the ungated merges) keep
  `Functor`/`Apply`, and the only `unsafePerformEffect`s left in the
  algebra are the three module-level diagnostics switches. Also `Hosting` and
  the generic `Acting (PUI m)` instance (see the collection bullet), and the
  **development diagnostics** — the private `tr` (emission trace) and
  `gateGuard` (starvation watchdog), instruments pointed at the algebra rather
  than part of it, living here because `PUI` is their only caller and they
  cannot sit under `PUI.Web`, which imports `PUI`. Both switches and the log
  sink are parameters (`setTracing`/`setDiagnostics`/`setSink`, no-ops until
  installed), so the algebra carries **no JavaScript**; a carrier installs them
  (`PUI.Web.adoptHostDiagnostics`). The module header carries the
  pipeline-semantics doc; `npm run api-docs` generates the per-combinator
  contracts from the headers.
- **Holes against the real library** (guardrails L18) — every view runs with
  its logic stubbed to bare untyped `hole`s (`PUI.Web.hole`, which throws on
  every touch but the field `__bambikHole`, read by the pure `isHole` in
  `Data.Profunctor.Seeding`), importing the real modules unchanged. What makes
  that possible: a stage is typed at **one row** (no `Union` subsumption —
  a business function is typed at the stage's row, its signature the
  view's hole hint verbatim — `countLine :: { counted :: Int } -> String`,
  2026-10-03, open-row footprints before — checked by unification), a shared record input is an equality
  (`SharedRecordInputs`), selectors and keyed collections compare
  structurally (`Data.Profunctor.Row.Structural`, a key read through the
  ecosystem `Foreign`), and the seven words that consume a logic value when built
  (`announce`, `ticks`, `debouncedTextField`, `action`, `foreach` and
  `each`) treat a hole as absent.
  The record of how it was chosen (twin modules, free rows, and this) is
  doc/research-holey-weak-types.md. Its converse is the **determination**
  half of L18 (2026-10-01 on inbox, every demo 2026-10-02): with a typed
  hole for every value the view imports, the compiler's hole list is the
  view model module's signatures with nothing unknown — because the model
  row is declared once, where the model first appears (the seed line,
  `# looped @( counted :: Int ) # with freshCount`; or a load action's
  outcome, `action @{ … } loadPeopleCatalogue`, when the load stands
  before the knot — crud, order-form), and every derived row where it is introduced, as a
  visible type argument after the anchor: a classifier's cases on its
  first pane (`provided`/`shownWhen`/`inCase @l @s`), an action's outcome
  (`action @t`), a projection's element row (`listOf`/`foreach`/
  `shownEach @r`), a payload (`with @a`), a bracketed editor's state
  (`bracketed @v @s`), and option lists
  closed by `<+>`; a stored field is read by a plain accessor
  (`_.messages`, `# provided @"confirming" _.deletion`), typed by the
  model row. `npm run check-determined` (scripts/check-determined.mjs)
  replays it for every demo.
- **src/PUI/Gate.purs** — the knowledge gate as **one pure Mealy machine**
  over runtime **participants**: `gateStep :: Ord k => GateState k v ->
  GateInput k v -> Tuple (GateState k v) (GateOutput k v)`, in a module with
  no `Effect` in it. Participants are keys each holding one slot; the gate
  releases the slots in participant order once every participant is known,
  retain-last thereafter. The `×→×` merge enrols its **owned field
  labels** (2026-09-15: participants are fields, not sides, so the
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

  The **vocabulary** it defines or re-exports (contracts in the headers):

  | Word | What it is |
  | --- | --- |
  | `silence` | the silent UI component, `×→+` shaped `{ \| i } → [ \| o ]`; silence forced by parametricity — the event merge's unit, and what an emitter removed by `provided` observationally *is* (not an operand `provided` is built from: copairing with silence leaves the source's emission channel connected, so absence is `Hosting`'s); a class member because the type (variant output only) is the honesty boundary |
  | `blank` | the faceless leaf `a → {}`, the wire's `lcmap`-closure at **every** input since `{}` is terminal: at record input the display reading `()` of the row (elements whose whole face is decorators), at variant input the status rendering no occurrence (`action`'s progress slot when there is no indicator, `blank # action …`); neither merge family awaits a zero-field contribution, so one word serves both |
  | `static` | an element with nothing in it — an ocular applied to the wire, pinned `{} → {}` (`static (span >>> cl "ripple")`); with `staticText`/`staticHTML` the three statics |
  | `announce` | the **point**, `Seeding`'s one primitive: one registration emission of `a` out of the terminal record, feeds ignored — what `with` closes over |
  | `with` | discharge a chain's initial obligation, `announce a >>> w`, at any seed type (2026-10-05): a model into the knot (`looped @( … ) # with seed`) or into a knotless flow (`# with @{ … } invitation`, potluck); its own input is ignored, so it sits at any row, and a hole seed is never announced. Never `with {}`: `body` feeds `{}` once itself, so a load action standing before the knot runs on that feed (2026-10-06, nine redundant `# with {}` lines dropped) |
  | `seeded` | the seeded echo wire, derived from the point through `Choice` (`dimap Right (either identity identity) (left (announce a))`): pass-through plus one emission of the seed |
  | `looped` | the **knot**: the `×`-diagonal self-trace (`Looping`'s method `looping` under a face whose visible argument is the loop's row, `looped @( counted :: Int )`), re-entrancy-guarded; the model re-enters whole. `mvu` (`with seed (looped w)`) was DELETED 2026-10-05 — the acronym names an architecture the loop does not need, and the two words are as concise |
  | `settled` | `rmap`-only normalization of the row, the normalizer an update of the stage's row in the logic |
  | `updated` | the input-primed Mealy update stage: fold each event emission of a wrapped `×→+` component into the retained value; **derived** (2026-09-27) — the wire and `looped` of `first`'s fold, merged by the `×→+` merge and collapsed to the row. Since 2026-10-04 library-level, not app vocabulary: an app's events reach its one `fold`; `updated` remains `edited`'s plumbing and the stage inside a `bracketed` editor (order-form's estimate) |
  | `applied` | `updated (const f)`, the payload-less occurrence stage; library-level since 2026-10-04 — in an app a replaying button's handler is the row update in the fold (`"Add": addTodo`) |
  | gated displays | displays are pipeline stages natively, typed `p { o \| rest } { o \| rest }` — a pass-through whose **release is the fulfillment witness**, gate policy baked into the component. The family (`PUI.Web` unless noted): `shown content` (ambient structured content — chrome registers at build, renders per feed, releases always), `shownWhen @l f content` (display pane: attach on relevance, release always), `inCase @l f editor` (the editor pane — `shownWhen`'s editor sibling; a carrier primitive, the pane's channel beside the wire's, since its content emits the row the owned merge would reject), `shownEach @l proj item` (keyed collection), `confirmed @l title $ content` (MDC2/MDC3 — the witness rung: modal, flow withheld until the user confirms). Content slots accept only `{}`-output components, keeping the no-silent-loss law; `observed` unchanged |
  | `muted` | the counit: render, and **deliberately discard** the component's output (`rmap (const {})`) — the visible form of what no stage may do silently; `# muted` writes off a genuinely emitting assembly (a `foreach` forwarding its elements inside a packaged control, scoreboard's summary group) so it can end at `{}` |
  | `observed` | the gated displays' `+`-diagonal sibling: every event forwards once at feed time; the status's own emissions are dropped (events are one-shot); **derived** (2026-09-27) — the wire and `left (status >>> silence)` under the `×→+` merge |
  | `every` | the heartbeat wire, `ticks` under `replaying` folded by `updated` in a `looped` (derived 2026-09-15); library-level since 2026-10-04 — in an app the tick is an ensemble operand, `blankStatus @"tick" # ticks tickPeriod`, folded by a total `tick` |
  | `ticks` | the tick source **opened by its status** (`blankStatus @"Clock ticked" # ticks tickPeriod`, 2026-10-07): every period the row it was last fed leaves as the status's case, told to the status first — replay built in, as under `clicked`; feeds retained, nothing emitted inside a feed. Before, `ticks @l period :: p {} [ l :: {} ]` under `# replaying @l identity` |
  | `replaying @l f` | **replay is `Strong`'s retention**: `first` around an occurrence source (a source emitting `[ l :: {} ]` with no payload), the fed row riding the state channel and joining each occurrence as `f` of it, leaving as case `l`; the `×→+` leaf's replay-last-value protocol as the primed `Strong` law — `clicked @l f w` *is* `replaying @l f` of a private click source (2026-09-15) |
  | `fold f` | **one case folded into the record** (`+→×`), **opened by its status** (experiment 2026-10-07, branch experiment/fold-status): `snackbar @"Person created" personCreatedLine # fold identity`, as a run is opened by its progress indicator (`indeterminateLinearProgress @l # action f`) — an optic over the status slot in `PUI`, derived as the status and the wire under the `×→×` merge; the status names the case (its closed singleton row is the fold's input) and shows each occurrence, `f` of the payload is the row released, so a loop's folds are one per event case, merged in `VariantToRecord.do`, each releasing the whole next model (the merge's output row is **shared**, 2026-10-04: the copairing of the coproduct, each release forwarded whole as it comes, where the `×→×` merge owns its fields exclusively; a status beside the folds releases nothing and is typed at every row, like `silence`). Memoryless: a replaying emitter's payload is the row it was fed, a payload event arrives `joined @l` with it, an effect returns the model (`fold @"created" identity`). History: exported 2026-10-02; a seeded retaining fold (2026-10-03) and a whole-variant `fold (match …)` (2026-10-04) both gave way to this form. A second visible row argument and a hole-absent handler (both 2026-10-05, for a loop seeded by an event into the fold) went with the variant knot on 2026-10-06 |
  | `joined @l` | **an event joined with the row its emitter was fed** (`×→+`, 2026-10-04): `first` around the source, the row riding the state channel and leaving beside the payload as `{ event, model }` under the same case — a list pick, a canvas click, a pane's button; `replaying @l f` is the payload-less case |
  | adopter family | `asField`/`subStrong`/`subChoice`/`toCase`/`atCase`, plus `acted`/`optioned` — `focusField` is deliberately **not** re-exported (2026-09-05): the leaf lift is design-system plumbing (every vocabulary editor is `focusField @l`-lifted inside, `group @l` carries sub-model nesting), living beside `widenRecordInput`; `grep "focusField @" demo/` is empty — the view-side read adopters `projection` (2026-08-31) and `projected` (2026-09-02) are both DELETED: copy is a read function at the leaf (`text lineOf`), never an adopter bracket — doc/research-copy-is-a-function.md. **business functions are arguments of leaves, never adopters** (2026-09-27): a status is label-indexed at its business case and takes its copy function (`snackbar @"booked" bookedLine`, read "the booked snackbar"; mutually exclusive outcomes are sibling statuses, each owning its case) and an emitter emits its own case — `toCases`/`forCases` are DELETED and `forCase` is no longer re-exported, surviving in `VariantToVariant` as the status face's plumbing, `focusField`'s variant twin |

  Durations are structural `{ ms :: Number }` throughout (`every`, `debounced`,
  `resolveFor`, `debouncedTextField`) — never `Milliseconds`.
  `synced`, `latch` and `constantly` are **deleted**: ensembles are `bracketed`
  record merges, and `constantly` was `()`-subsumption in disguise (positions
  whose mechanism subsumes take `blank` directly; constant catalogues enter
  through the consuming mechanism's projection argument).
- **src/PUI/Web.purs** - the carrier **and the root of the web layer**: DOM monad (`Web = StateT DOM Effect`), `Node`, DOM building blocks (`element`, `attachable`, `runDomInNode`) and FFI, plus the **element-neutral vocabulary** — every word that names no element: the decorators (`attr`/`:=`, `attrDyn`/`:=>`, `cl`, `attrWith`, `clWhen`, `init`), the text leaves (`text`, `textOf`, `staticText`, `staticHTML`), the occurrence sources (`clicked`, `onClickedXY`), `provided` and the gated rungs (`shown`/`shownWhen`/`inCase`/`shownEach`), and the structure builders (`dynamic`, `each`, `el`), and `hole` (guardrails L18) — so SVG and every design system use them without importing the HTML vocabulary (moved out of `PUI.Web.HTML` 2026-09-23; the interaction table under the HTML bullet lists them). No *element* lives here. Everything browser-specific is a submodule of it: the element vocabularies `PUI.Web.HTML`/`PUI.Web.SVG` and one module per design system (`PUI.Web.MDC2`, `PUI.Web.MDC3`, `PUI.Web.Shoelace`, `PUI.Web.Fluent`, `PUI.Web.Bootstrap`), all under **src/PUI/Web/** — so the carrier-independent algebra (`PUI`, `Data.Profunctor.*`) stays visibly separate from the web specialization
- **src/PUI/Web/HTML.purs** — the 1-1 HTML vocabulary over the carrier: the element oculars and HTML's native controls, and nothing that names no element (that is `PUI.Web`'s).

  **Entry**: `body :: PUI Web {} o -> Effect Unit` registers the wiring and
  then feeds `{}` **once** (2026-09-15) — the terminal record's one value,
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
  (the collection combinators `foreach`/`edited`/`acted` live one level
  lower still, at the **PUI level** — see the container-action bullet):

  | Word | What it does |
  | --- | --- |
  | `attrWith` | value-computed attribute — the channel-fed counterpart of static `attr`/`:=`, so a cell's style/coord/colour updates in place through the channel rather than by rebuilding a closure |
  | `clicked @l f` | click emitter for any element, emitting `f` of the row last fed as case `l` (`button`'s replay-last-value protocol — replay is lawful over records only, so the payload is a row, and the channel is a variant, so the source is `×→+` by shape); **derived** since 2026-09-15 as `replaying @l f` of a private click source (`occurrences`, `p i [ occurred :: {} ]`, at `{}` the point's dual), so the replay `Ref` is `Strong`'s retention and a click before any feed is the primed `Strong` law's withheld emission; its content is fed the row it replays — a multi-reader content names its shared reading once in a *face* function typed at the element row, closed |
  | `provided` | the **emitter pane**: case-gated existence of a `×→+` content (`PUI Web { \| a } [ \| o ] -> PUI Web { \| i } [ \| o ]`) — its argument is a classifier (a stored variant field via a closed accessor, or a variant-returning business function), content attached and fed the case payload on case `l`, detached on every other case, where its silence is `×→+`'s answer. Its siblings are the panes for the other contents — `shownWhen` (displays) and `inCase` (editors) — all three over one private mechanism, `attachedOn` (2026-09-23; before, `provided` took any content and broke Answer on a detached display). There is no `Maybe` form: a state a pane depends on is a variant with named cases, so mutually exclusive states are exclusive by construction and the view line names the state it shows. It **detaches**, so it is a pipeline stage, not a gated-merge operand; takes the classifier's variant as a visible type argument after the case (`provided @"confirming" @( confirming :: {}, silent :: {} ) _.deletion`), as `shownWhen`/`inCase` do, so the view declares the states a pane chooses among (L18's determination half) |
  | `clWhen` | value-dependent class — styling, deliberately last-element-only |
  | gated display rungs | `shown content` (ambient structured content, registered at build), `shownWhen @l f content` (display pane), `inCase @l f editor` (editor pane), `shownEach @l proj item` (keyed collection) — each `p { o \| rest } { o \| rest }`, releasing the fed row per its policy; `confirmed` (the witness rung) lives in the design systems; `shownWhen`/`inCase` take the classifier's variant and `shownEach` its element row as visible type arguments after the anchor (2026-10-01) |
  | `onClickedXY @l` | container-level pointer-down coordinates (local/viewBox `{ x, y }`) for canvases, emitted as case `l` |

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
  (`Cons l Number`, fraction 0–1) and the `output` status (the one fixed
  canonical row here, `[ event :: String ] → { | r }` — HTML's element for the
  result of a user action, shown in place since plain HTML has nothing
  self-dismissing), joined 2026-09-05 by `input @l` and `textArea @l` (the
  focus-guarded text editors, `Cons l String` — no caption chrome of their
  own, a caption staying a sibling `label`+`staticText` merge). With that,
  **`focusField` left the application surface entirely** (no longer re-exported
  from `PUI`): the leaf lift is design-system plumbing in every vocabulary,
  the plain-HTML floor included. The scalar `radioButton` (`Maybe a → a`)
  is deleted (2026-09-23): **shape is the type** (guardrails L3) — every
  exported component ends in one of the four row forms, each side a record
  or a variant — app-packaged controls included. A word that would need
  two shapes is two words: the pane is `provided` (emitters),
  `shownWhen` (displays) or `inCase` (editors). Only the decorators
  `clWhen`/`attrWith`, like the oculars, keep the shape they decorate.

  **Structure computed from data is `PUI Web` all the way down** — no markup
  DSL — in two regimes:

  1. **Fixed structure, changing values** (grids, an SVG canvas): feed the
     structure as data through the retaining `foreach` and compute each
     element's content/attributes from its fed value — `text f` reading the row
     for content, `attrWith` for style/coords, `clicked @l _.key` to emit
     identity. Built once, updated in place: no wholesale rebuild, no `data-*`.
  2. **Structure that genuinely varies with the data** (markdown blocks): the
     closure builders `dynamic` (a whole component per value,
     `el ("h" <> show level)`) and `each xs build` (a closure-known list pinned to `{}`) rebuild per feed.
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
  `+→×` statuses (`snackbar`/`toast`/`messageBar`/`banner`), plus the
  **selectors** (`select`, `radioButton`/`radioGroup`, `segmentedButton`,
  `dropdown`) — `×→×` editors of field `l`, each in three words
  (2026-09-26): the plain word's field holds the option itself (lifted
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
  made case either way.
  History: a `×→×` leaf silent on `Nothing` until 2026-09-23, then a
  `×→+` picker completed by stages, then briefly a selection-prism
  argument (`required`/`optional @c`).
  *Oculars* are shape-preserving decorators with no model of their own
  (`card`/`cardActions`, dialogs, lists, typography, elevations) — and a
  **surface ocular carries no copy config**: MD2 gives a card twelve optional
  structure classes and no heading, MD3's card element is a bare `<slot>`, so
  `card` is a plain `Ocular` in all five vocabularies. A card whose content
  is one model sub-record is not chrome but the **labelled group** `group @l`
  (MDC2/MDC3, admitted 2026-09-04 — guardrails L3): `focusField @l` fused with
  the card surface, the label the field, the heading verbatim and the
  accessible group name (`role="group"`); the fusion criterion (a label fuses
  exactly where it does work `# focusField @l` alone cannot) is recorded at
  L3. The blind `card` is for content that **edits nothing** (2026-09-28):
  order-form's summary, product-review's preview, meeting-booker's plan,
  loan-calculator's repayment figures. Editors never share a blind card
  (they share a sub-record), and an app never wraps itself whole in one: the
  surface an app is shown on is its **page's** — demo pages opt in with
  `<body data-surface>` and demo/page.js builds the vocabulary's card around
  the mounted demo, so an entry reads `body $ …`, never `body $ card $ …`.
- **Every leaf is label-indexed** (L3) and captions itself from that label
  verbatim; editors also stamp it as the host `name`. Config overrides carry
  real copy the label cannot be — the key is `floatingLabel:` on the MDC text
  fields and `select`, plain `label:` elsewhere.
- **Leaf-echo protocols** are identical across all five: focus-guarded text
  fields (model updates never clobber the field being typed in, and the channel
  stays live), per-feed display echo (the `{}` answer to a feed and nothing
  at registration — inert to every gate, real to sequencing), selectors
  answering every feed with the row, emitters firing nothing inside a feed, and `clicked`'s replay-last-value
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
- **Every vocabulary is entered through its own `body`** (2026-09-06),
  `PUI Web {} o -> Effect Unit` exactly like `PUI.Web.HTML.body`: it does the
  catalogue's page-level setup at mount — MD2's `mdc-typography` baseline,
  MD3's typescale stylesheet, Fluent's theme, Shoelace's icon base path — and
  then mounts; Bootstrap, needing none, names the plain one as its own. No
  vocabulary acts on the page at import time any more, and a twin's entry
  line switches design system with the rest of its import. The app bar and
  drawer stay oculars inside it: a shell is chrome, the root is the mount.

Per-catalogue deltas:

| Module | Basis | Deltas worth knowing |
| --- | --- | --- |
| `PUI.Web.MDC2` | `material-components-web`: documented markup + a foundation instance (`newComponent material.x."MDCX"`) wired through its documented properties/events; text fields write through the foundation's `value` so label float stays foundation-managed | the fullest catalogue: `indeterminateLinearProgress`/`indeterminateCircularProgress` are **statuses** (`[ started :: {}, ended :: {} ] → {}`, mirrored in MDC3 — 2026-09-13: `action`'s progress slot dispatches the run's two occurrences, no model owns a `busy`, and a status owes the channel nothing, so the slot left the gated broadcast entirely; the earlier `{ busy :: Boolean }` was a two-case phase written as a Boolean nobody edits), `listOf` (a **dynamic collection component**, `listOf @l @k @r provided rowsOf item :: PUI Web { \| i } [ \| s ]` (the element row `r` a visible type argument after the key, `Cons k key rest r`, `Cons l key () s`: the rows keyed by their field `k` — a named field of the row the projection builds, never a pick function, since the UI decides its view model — and a clicked row leaving as case `l` carrying that key) — keyed `foreach @k` retention, MD2 selected styling an optional `selected` predicate), `dataTable`/`dataRow`/`dataCell`, `imageList`/`imagePane` (the channel-fed sibling of the static `imageListItem`), `layoutGrid`, `topAppBar`, `drawer` (permanent, with a **live nav slot**: nav is the first stage and content the second, so the nav's release feeds the content and a feed is released once), `tooltip`, `banner`, `tabBar` (the same-type selector with unconditional echo — the `looped`-ensemble citizen), `menu`/`menuItem`, `chipSet`/`filterChip`, `iconToggle`, `dialog`/`simpleDialog` (modal protocol: **open on feed, close on emission**), `group @l` (the labelled model group — card surface + heading + `focusField @l` in one word, label stamped as the accessible group name; mirrored in MDC3) |
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

  every canonical-row leaf is **label-indexed**: the business label is a visible type argument on the leaf itself — `filledTextField @"First name" {}`, `select @"Milk" cfg opts`, `button @"Submit order" {}` — so a merge operand or emitter states its row once, at the leaf, and nothing in application code ever says `value`/`clicked`/`event`.

  Business functions are **arguments of leaves**, never adopters (2026-09-27): a display takes its read function, a status its business case and copy function (`snackbar @"booked" bookedLine` — `forCase @l`, which lifts the canonical `[ event :: String ]` face, is vocabulary plumbing like `focusField`), and an emitter emits its own case, the business outcome decided where the case is consumed. `toCases` and `forCases` are DELETED. `forField` and `asCase` are DELETED — their rename job moved onto the leaf. `projection`/`projected` are DELETED too (2026-08-31 and 2026-09-02): **copy is a function, not a field** — a display whose content is copy takes its read function at the leaf and carries no label at all (`text progressLine`, `text _.title`; `forProperty` and `atField`, the record-input readers, are DELETED too, 2026-09-27 — unreached), so the screen's copy is one pure logic function under unit test and the view line names its writer (doc/research-copy-is-a-function.md).

  The label also names **and captions** the component: every label-indexed leaf **stamps its label on its host element** — `name` on form citizens, `aria-label` on quantity displays — whose label is that accessible name and nothing else, the value arriving as a read function (the stamp invariant, stated at `OptCaption` in `PUI.Web` — whose second half is the accessible name, the caption **verbatim**: browsers compute names from rendered text, so faces whose catalogue styling transforms the caption stamp `aria-label` with it verbatim — MDC2's button family, tabs, segments and menu anchor, whose MD2 uppercase once computed `@"Count"` as "COUNT" — and so do faces the platform cannot name by itself: MD3's checkbox and switch sit inside a wrapping `<label>` whose association never reaches the input in their shadow root, so the switch stamps its label and the checkbox the rendered text of its content; `text` is outside the family entirely — no label at all, and under host diagnostics a bare `text` comment marker) — and every captioned leaf — editors' `floatingLabel`/`label` and the whole `×→+` emitter family (`button`/`outlinedButton`/`textButton`/`elevatedButton`/`tonalButton`/`fab`/`iconButton`/`menuItem`, in all six vocabularies) — defaults its caption to the label **verbatim** (`OptCaption` in `PUI.Web`, shared by all six vocabularies; the MDC modules add their own `OptLabelIcon`/`OptLabel`/`OptIcon`/`OptSelected` for their richer faces). **Nothing derives a caption from an identifier** — `humanizeLabel` is DELETED: a label *is* the copy it draws, so it is written as such and is usually a quoted string, since human copy is no identifier (`filledTextField @"First name" {}`, `button @"Submit order" {}` drawing those words and emitting `[ "Submit order" :: _ ]`, quoted at every mention — `atCase @"Submit order"`, `match { "Submit order": … }`). Because a leaf's label is the model field it edits, **the business rows carry the same quoted labels** (`{ "First name" :: String }`), whose one syntactic cost is that a quoted label cannot appear in a **record pun** — the view model modules bind explicitly instead (`createPerson { "Name": name, "Surname": surname, people }`), while field access, accessor sections and update syntax are unaffected. **No demo passes an emitter `label:`**: where two buttons would share one handler under different words, the buttons are two business actions — each emits its own self-describing case and the fold block applies the one handler to both (checkout's `snackbar @"Next" steppedOnLine # fold stepTo`, `snackbar @"Back" steppedBackLine # fold stepTo`), so each button reads as what it does. For emitters `label:` is left only for a glyph-only face (`fab { label: Nothing }`).

  For **editors** the same rule holds, and **no demo passes a caption config at all**: the label carries the full copy, punctuation and units included — `filledTextField @"Start date (DD.MM.YYYY)" {}`, `sliderLive @"Amount (€)" {}`, `filledTextField @"Formula (e.g. =SUM(A0:A5)*2)" {}`, `filledTextField @"What needs to be done?" {}`. The type argument *is* the caption, so a leaf never states its copy twice. Selector **options** follow the same rule via `choice` (in `PUI.Web`): a choice states its copy once, at its case, and the `{ value, label }` echo disappears — `dropdown @"Room" {} (choice @"Focus pod (4 seats)" <+> choice @"Boardroom (12 seats)")`. `choice @l` is a one-option array over the closed singleton row of its case and `<+>` joins option lists uniting their rows (2026-10-02), so the row a selector's field holds is closed by the list itself and the options stay an ordinary array whose order is the order written — deliberately **not** the variant row's, which the compiler sorts alphabetically, while option order is a design decision (rooms by size, durations by length). The vocabulary still keeps `floatingLabel:`/`label:` in its signatures for real copy a label genuinely cannot be — localized wording above all.

  An editor whose text is *derived* from sibling fields keeps the derived texts as model fields and normalizes them into each other with `settled` (temperature-converter holds both `@celsius` and `@fahrenheit` texts — a label is any string, so where a symbol *is* the conventional caption it is written as one — each field's stage running `# settled fromCelsius` / `# settled fromFahrenheit`, a failed parse leaving the sibling untouched)

  **A visible `@"…"` names a row label — a field or a case — and nothing else** (2026-10-07): a title, a caption, a column heading, a tip or a static's text is a String value (`topAppBar "Espresso Bar"`, `confirmed "Refund" "Refund the customer?"`, `columnHeader "Qty"`, `staticText "Hours"`, `drawer { title, subtitle }`), and a quantity display takes only its read function (`linearProgress elapsedFraction`); the one visible argument that is neither is a **declared row**, below.

  The closure of the label discipline is the **anchor invariant** (writing.md *The anchor invariant*; library-side face and rejected-design record in guardrails L3): every view line names exactly one semantic anchor — a model **field** (`@l` on an editor/selector), a business **case** (`@l` on an emitter, pane or status), a **named read function** (a display's content), or **nothing** (chrome) — line ↔ named symbol in the model or view model module, deliberately never line ↔ field (a symbol on an ocular was rejected 2026-09-03: no model, nothing to anchor); and each anchor sits **in its own position** (2026-09-28): a field or case as the leaf's type argument, a read function as a display's positional argument, so every leaf reads as a noun phrase — word, anchor, then its positional arguments (`snackbar @"booked" bookedLine`, `topAppBar "Espresso Bar"`) — and records hold only optional presentation or same-typed values whose names prevent a swap, never an anchor or a required value; a leaf never makes the application name its rows with vocabulary field names (L3's canonical-labels rule: it takes a read function, `imagePane developedShot`)
- **extras/** - the layer the shape modules stand on, extracted so `Data.Profunctor.Row.*` holds only what mentions a row, laid out **exactly as the ecosystem lays out its own**, and kept outside `src/` under separate source roots because claiming an ecosystem module name is a claim about what the module *is*. Everything here but `extras/variant` is **unreached by the library, the demos and the tests** (L14 would otherwise prune it) but stays in the build glob so it cannot rot; all of it but `Cont` is also non-row and non-carrier, mentioning neither `PUI`, a row nor a carrier.
  - **extras/profunctor/Data/Profunctor/{Resolving,Coresolving,Retaining,Coretaining}.purs** — one *class* per module beside `Data.Profunctor.Strong`/`.Costrong`, so the coined strengths and their co-strengths are four separate files, stated positionally with `Tuple`/`Either` or at bare `a b`, no row in sight. Pure **complements of the ecosystem's own**: liftable into `purescript-profunctor` unchanged.
  - **extras/profunctor/Data/Profunctor/Cont.purs** — the root's one **carrier**, and so its one member that is *not* liftable: the CPS profunctor `Cont r a b = (b -> r) -> (a -> r)`, the repo's only *pure* carrier of the row algebra — a timeless model where the merge gate is continuation nesting rather than a pair of `Ref`s. Its header inventories, **exhaustively** over every profunctor subclass in the repo and the ecosystem, which classes it validly inhabits (`Strong`, `Choice`, `Category`, `Wander`, `Acting`, `Cochoice`, `Seeding` — the point as the wire's answer at `{}`, inhabited since 2026-09-15 — `RecordToRecord`, `VariantToVariant`, `Monoid r => RecordToVariant`, plus the degenerate `Resolving`/`Coretaining`) and which it provably cannot (`Costrong`, `Coresolving`, `Retaining`, `VariantToRecord`, `Looping`, `Closed`), each with its reason — so an absent instance is a stated impossibility, never an unwritten one, and those impossibilities are exactly why `looped` is a primitive and the mixed co-strengths have no row forms. It builds (it was parked until 2026-08-11, which is what let the inventory drift), so a move in the `Data.Profunctor.*` layout that invalidates it is a compile error rather than silent rot.
  - **extras/lenses/Data/Lens/{Colens,Coprism,Shutter,Coshutter,Reel,Coreel}.purs** — one *optic* per module beside `Data.Lens.Lens`/`.Prism`, each carrying its type, its collapsed constructor and its `*E` existential encoding at arbitrary `s t a b`. `Colens`/`Coprism` are the optics of the *ecosystem* classes `Costrong`/`Cochoice`, which `profunctor-lenses` never built, so they too are liftable as they stand; `Shutter`/`Coshutter`/`Reel`/`Coreel` are coined class and optic alike, so each would travel with its class. Plus **extras/lenses/Data/Lens/Prism/Existential.purs** (`prismE`, the existential constructor of the ecosystem's own `Prism`, which `Data.Lens.Prism` does not export — it *extends* that family rather than shadowing it, which is why it is not named `Data.Lens.Prism`): the purest complement in the tree, since both the optic and its `Choice` are already the ecosystem's.
  - **extras/variant/Data/Variant/Case.purs** — the **value-level label read** `caseText` (the case label of a variant value, verbatim — `unvariant` + `reflectSymbol`, a composition `purescript-variant` never exported, so liftable unchanged; law `caseText (inj @l a) = reflectSymbol (Proxy @l)` in the header). The one extras module that is demo-reached and law-tested rather than parked: under L3 a case label *is* the copy it draws, so application code reads labels back with `caseText` instead of `match`-restating them (espresso-bar's summary, order-form's "Paying by cash", potluck's menu, meeting-booker's booked line, product-review's preview) — options are therefore labeled as the exact copy the line needs (`choice @"with oat milk"`, `choice @"cash"`), while a map that does real work (meeting-booker's `roomText` shortening, tic-tac-toe's glyphs, signup-form's sentences) stays a named copy function. The rule as applications read it is writing.md's *A label is read back, never restated*; `Data.Variant.Case` counts as a domain module, importable from logic and view alike.
  - **extras/row-profunctor/Data/Profunctor/{Row,Row/*,Acting,Seeding}.purs** — the **row-profunctor algebra itself**, and a different claim from the two roots above: not anyone's complement but bambik's own invention, yet still carrier-agnostic (the algebra of merging labelled rows; `PUI` is one carrier that satisfies it, `(->)` another — it now carries both **diagonal** merges, `RecordToRecord` and `VariantToVariant`, so their unit/associativity/symmetry laws run as pure equalities, while the two mixed merges stay `PUI`-only by necessity: `silence` cannot fabricate a case and `variantToRecord` needs retention). With this root out of `src/`, **`src/` holds exactly the carrier and its vocabularies** — `PUI`, `PUI.Web` and `PUI.Web.*` — so the split reads: `src/` is the UI library, `extras/` is the algebra it stands on.
  - All five roots are covered by the single glob `extras/**/*.purs` in spago.dhall's `sources` beside `src/**/*.purs`, and watched by scripts/dev.mjs. **Downstream caveat**: spago globs a git dependency as `.spago/<pkg>/<ver>/src/**/*.purs` — hardcoded, ignoring the package's own `sources` (the same reason the bootstrap must spell out the dependency list) — so modules outside `src/` are invisible to a consuming app. A tag carrying this layout therefore needs the app's own `sources` to add `.spago/bambik/<tag>/extras/**/*.purs`, which is why bootstrap.md's spago.dhall step carries that second glob; one glob covers all four roots, and it is tag-pinned, so it moves with `bambik.version`. The row layer's combinators are these optics at row granularity (`cycled` a `Coprism`, `subResolving`/`subRetaining` a `Shutter`/`Reel`; `Colens`/`Coshutter`/`Coreel` had row forms — `feedback`/`folding`/`unfolding` — until 2026-10-05, when the knots became two)
- **extras/row-profunctor/Data/Profunctor/** - `Seeding` (**the point, row-forced in value and carrier-timed in moment** — restated 2026-09-15: `class (Category p, Choice p) <= Seeding p` with `announce :: a -> p {} a`; law **answer**: `announce a ≈ lcmap (const a) identity` — `{}` has one value, so by Repetition answering it once and every time coincide, and the point is the wire's answer to the terminal record; law **early**: a carrier with a registration moment answers before any feed, which is the operational ⊥ a seeded `×`-trace needs — the one thing a timeless carrier lacks; so `(->)` has the instance `announce = const` and `Cont` answers its continuation, while `seeded a`, the pointed wire derived through `Choice`, is `identity` on `(->)`; `with` closes over it, and `body` feeds the closed app `{}` once), `Looping` (**self-reference as carrier structure**, `Seeding`'s sibling: `class Profunctor p <= Looping p` with the row-shaped `looped :: p { | r } { | r } -> p { | r } { | r }`, the `×`-diagonal self-trace no ecosystem class reaches — gated `unfirst` deadlocks on self-feed; laws are the trace axioms at the diagonal (yanking, dinaturality, idempotence); no `(->)` instance — feedback on a timeless carrier is `fix`, a computation; carries `mvu` and `bracketed` as its carrier-agnostic derivations) + the `Row/` layer; everything else was dissolved or deleted
- **extras/row-profunctor/Data/Profunctor/Row/** - Row profunctors over `Record`/`Variant`: four shape modules, each carrying its **shape class** — the binary merge, the one genuine per-carrier primitive; no class carries a unit of its own: the unit laws are conditional on the carrier (*if* `p` is a `Category`, a wire into the unit object must play well with the merge — the `{}` wire at the merge's row for `×→×` (operands share one input row), `identity @(Variant ())` for `+→+`, and that wire entered from the empty variant for `+→×`, `lcmap case_ identity`), and only `×→+` keeps a class-member unit, `silence`, the one no wire reaches — with qualified-do sugar (`bind`/`discard`). Everything kept is reached by a demo or a law test (L14); laws are stated in the module headers. **Type variables name their kind (2026-10-03):** `r` is a record row (`{ | r }`), `v` a variant row (`[ | v ]`), `b` the background row of a `Cons` (`Cons l a b r`, `Cons l a b v`), `rl` a `RowList`; when a signature has several rows of one sort they are numbered in order of appearance and the result's row is bare (`p { | r1 } { | r2 } -> p { | r1 } { | r3 } -> p { | r1 } { | r }`), a visible row argument keeps the same letter (`@r`, `@v`); plain types stay `a b c`, a focus type `f`, a Symbol `t`/`l`/`c`. The earlier photographic schema (`Cons l f b s`) and the `i`/`o`/`big`/`wider`/`narrow`/`rest` names are retired; the design-system modules follow the same rule for their leaves (`Cons l a b r`).
  - **`RecordToRecord.purs`** (×→×) — merge `recordToRecord` (`SharedRecordInputs` + `OwnedRecordOutputs`; gated on `PUI`, zero-field sides pre-satisfied and inert — `{}` is always known and a contribution of zero fields is no contribution — and **released once per feed**: the broadcast is one step, so a feed changing several fields emits one fresh row, never a torn one);

    over ecosystem `Strong`: `subStrong` (sub-record focus, background carried), `focusField` (the type-changing field lens — the leaf lift: an editor lifted with it is a whole-row citizen, background retained and re-attached per emission, which is what dissolved `completed`);

    over the **unit** (the `{}` wire at the merge's row, `blank` — exactly, since the gates ignore a zero-field contribution): `with` (`announce a >>> w` over `Seeding` — discharge the initial-state obligation; `with seed (looped @( … ) w)` is the app shape, closed to `{}`), plus `settled` (`rmap`-only normalization of the row, the normalizer an update of the stage's row in the logic);

    over bare `Profunctor`: `muted` (the counit — render and deliberately discard, `rmap (const {})`; the explicit word the gated content slots' `{}` demand points to);

    the **knot** is `Looping`'s `looped` (below), closed by `with`; the pointed trace `PointedCostrong` (`extras/profunctor/Data/Profunctor/PointedCostrong.purs`, 2026-09-27: `unfirstFrom :: c -> p (Tuple a c) (Tuple b c) -> p a b`, `unfirst` with its state channel started at `c`; laws yanking and sliding; instances `(->)`, `Cont` and `PUI m`) keeps the value-level law only — its row form `feedback @l seed` (one hidden state field) was DELETED 2026-10-05: a looped state is a model field (auction's `top`).
  - **`VariantToVariant.purs`** (+→+) — merge `variantToVariant` (`OwnedVariantInputs` — one handler per case — + outputs **one row** every operand is typed at, an equality like the `×→×` merge's inputs, no class: since 2026-10-07 on branch experiment/fold-status, before that the inclusive union `SharedVariantOutputs`, which no operand's row could be split back out of under holes, so two actions sharing an outcome had to name it on each line); over ecosystem `Choice`: `focusCase` (the value-level case prism, via `prismE`), `subChoice` (**sub-variant focus**, `subStrong`'s transpose completing the wrap family's `+→+` corner: the wrapped profunctor handles the focus cases, background cases pass untouched — cashbox's money events detour through confirmation dialogs while its audit event flows straight to the fold) and `splitVariant` (the dispatch helper `VariantToRecord.subRetaining` and `subChoice` share); (`bracketed` moved to `RecordToRecord.purs` 2026-09-15: its result is a record-shaped field editor); over bare `Profunctor`, every variant-side adopter — a word lives in the module of the sides it constrains, so the one-sided ones sit in the diagonal module (2026-09-27, when `toCase` moved here from `RecordToVariant` and `forCase` from `VariantToRecord`): `atCase` (adopt a bare-input UI component as the owner of input case `l` — the closed-singleton unwrap at `+`), `toCase` (introduce a **bare** output as case `l`, the output-side dual of `atCase`; over a record-reading component it is the `×→+` merge pinned at its unit, which is why `recordToCase` was deleted; dissolves the `rmap`-style payload lambda at collection sites: `listOf {} entries item # toCase @"picked" _.key`), and `forCase @l` (render business case `l` into a single-case status's own case — **vocabulary plumbing**, not re-exported by `PUI`: every status is its `[ event :: String ]` face under `forCase @l f`, so the application writes `snackbar @"booked" bookedLine`); nothing over `Cochoice`: its retraction holds raw here, and its row form `cycled` (the variant knot, 2026-10-05, replacing `iterate`) was retired 2026-10-06 — a loop is cut at its record junction, an event re-entering through its fold.
  - **`RecordToVariant.purs`** (×→+) — strength `Resolving`/`resolve :: p a b -> p (Tuple a c) (Either b c)` (a loop step: `Left` = Done, `Right` = Loop; `PUI`-only instances — the branch is derived **from time**: emissions loop while the UI component is still moving, the last resolves at quiescence, so `coresolve (resolve g >>> seeded (Right c0)) ≈ debounced g` — literally: `debounced`'s body IS this seeded retraction, the loop channel primed by a `seeded` wire, since the raw composite is input-dead; window parameterized via `resolveFor`, whose 300ms default is the instance's quiescence quantum) and its co-strength `Coresolving`/`coresolve` (ties the loop: a terminating fold; its row form `folding @w @l` was DELETED 2026-10-05 — a wizard's step is a model field folded by its buttons' cases, checkout); merge `recordToVariant` (ungated broadcast); over `Resolving`: `subResolving`; plus `silence` (the unit's `dimap`-closure at any rows — the silent UI component, forced by parametricity), `armed` (the **emit stage** — marks an event ensemble as fed the row its emitters replay, whole; a consumer is typed at the payload row), `joined @l` (2026-10-04: an event joined with the row its emitter was fed, `{ event, model }` — `first` around the source). `backgroundProperty` and `recordToCase` are DELETED (2026-09-27): the first unreached and derivable from `subResolving` at a singleton row, the second exactly `toCase @l identity`.
  - **`VariantToRecord.purs`** (+→×) — strength `Retaining`/`retain :: p a b -> p (Either a c) (Tuple b c)` (a Mealy/coroutine step; `PUI`-only instances — a stateless function can't retain state) and its co-strength `Coretaining`/`coretain` (ties the state channel: a productive unfold/generator; its row form `unfolding @w @l` was DELETED 2026-10-05 — a counter is a model field, ticket-dispenser); merge `variantToRecord` (the **copairing** since 2026-10-04: inputs owned, the output row **shared** — each operand releases the whole row and the merge forwards releases as they come, no gate, no retention, `Applicative m` enough — so a loop's per-case folds merge here, and a status, releasing nothing, is typed at every row like `silence`; before, outputs were owned and the merge gated like `×→×`); over `Category`: `fold` (moved to `PUI` as a status optic on branch experiment/fold-status, 2026-10-07), one case folded into the record, memoryless since a replaying emitter's payload is the row it was fed and a payload event arrives `joined @l` with it (a whole-variant `fold (match …)` lasted a day, 2026-10-04, before the per-case form returned for parity with the other leaves); over `Retaining`: `subRetaining`. `focusCase @l @w` and `backgroundCase` are DELETED (2026-09-27: unreached, and derivable from `subRetaining` at a singleton row), so the mixed modules hold only their strength, their trace and the words spanning both sides.
  - the collection lives one level up, in **`extras/row-profunctor/Data/Profunctor/Acting.purs`** (module `Data.Profunctor.Acting` — beside `Row/`, not under it: rows are the finitary μ-free fragment of the container grammar, `Array = μx. 1 + a×x` is one `μ` later). The class is the minimal carrier primitive `class Profunctor p <= Acting p where actedBy :: Ord k => (a -> k) -> p a b -> p (Array a) (Array b)`, keyed by the element row's **materialized identity field** (rows carry their identity; `Ord k` is the reconciler's Map-indexing requirement — identity semantics remain equality; keys must be unique and are never rendered). Laws in the header: **empty** (fed `[]` emits `[]`; nothing at registration), **singleton retraction** (yanking at the container), **gather gate** (`Array b` withheld until every element spoke, retain-last thereafter), **identity-follows-key** (stateful carriers), **wire** (`actedBy k identity ≈ identity`; composition deliberately *not* preserved — two reconcilers double-gate, so whole element pipelines are lifted, never stages). Positioned honestly: the *type* is the Strong/Choice closure under `μ`, the keyed retaining semantics is extra carrier structure beyond it (a derived traversal would rebuild where this reconciles). The module holds only the **pure algebra** — the class, `instance Acting (->)` (`actedBy _ = map`, so the laws are value-testable), and `optioned` (the `Maybe = 1 + a` action via the Array embedding). The carrier machinery lives with the carriers, exactly like the merge instances: **`PUI.purs`** carries `class Hosting m node | m -> node` (what a stateful carrier contributes — instantiate one element component at runtime, plus placement: detach a leaver, restack survivors), the placement-free `Hosting Effect` instance (the probe carrier the `spago test` laws run on), the shared keyed reconciler, the one **generic** `instance Hosting m node => Acting (PUI m)`, and the five vocabulary forms; **`PUI.Web`** carries `instance Hosting Web Node` (DOM placement — `appendChild` moves nodes, so identity follows the key). Design note: doc/collections-profunctor-algebra.md.

    The five divide the ground by **input** (everything at once → `×`; one entity at a time → `+`) and **output** (individual event; aggregate as joint decision; aggregate as running state). Key forms encode the key's ontology: a **label `@l`** on the `×`-members (identity is a materialized model field) and the **`{ key, value }` envelope** on the `+`-members (identity is the structural tag, arriving in the input, so no key function). Each `×`-member takes the **projection** that feeds it; the `+`-members take the projection producing the envelope.

    | Form | Shape | Behaviour |
    | --- | --- | --- |
    | `foreach @l` | `(i -> Array { \| a }) -> PUI m { \| a } o -> PUI m i o` | the collapsed/sum-flavored form: **keyed and retaining** reconciliation, matched elements re-fed in place, nodes moved with their keys; forwards each element emission onto the shared channel, ungated, silent on empty. Written trailing in a container ocular, `ul $ item # foreach @"id" rowsOf` |
    | `acted @l` | `p { \| a } { \| b } -> p (Array { \| a }) (Array { \| b })` | the **container action** (Tambara for `Array`): the element is typed at its whole row, key included, and the carrier **re-sets** the key on every emission from the input row via the `Strong` state channel, so identity is unforgeable — whatever an element emits in the key field is replaced. Output is the **gather gate** — withheld until every element has spoken, then whole on any re-choice |
    | `edited @l` | `PUI m { \| a } { \| a } -> PUI m (Array { \| a }) (Array { \| a })` | the **collection editor** (derived 2026-09-27: `updated` folding a `foreach` of `first`-keyed elements): element emissions folded back in by key, whole array emitted **immediately**, input-primed (the retained fed array supplies unedited slots). The element is a whole-row stage over its element row and the carrier re-sets its key on each emission — an element cannot change its own identity |
    | `dispatched` | `(i -> { key :: k, value :: a }) -> PUI m a b -> PUI m i { key :: k, value :: b }` | `+→+`: an unknown key instantiates a new case, a known key re-feeds exactly its instance, emissions leave tagged — the targeted-update/stream direction, no whole-array re-feed |
    | `accumulated` | `(i -> { key :: k, value :: a }) -> PUI m a a -> PUI m i (Array a)` | `+→×`, the keyed Mealy: grows per new key in first-appearance order, updates known slots, emits the whole array immediately, input-primed — the board/ledger shape |

    The `+`-members never detach or restack: absence of a key is no signal, so removal and ordering stay array-level concerns upstream.
  - **`Row.purs`** (module `Data.Profunctor.Row`) — the shared floor: the row-constraint vocabulary (`InclusiveRows`/`ExclusiveRows` + the runtime-evidence duals `DispatchableVariants`/`MergeableRecords` with `exactRow`, bundled into the per-side classes `SharedRecordInputs`/`SharedVariantOutputs`/`OwnedVariantInputs`/`OwnedRecordOutputs` — sharing is open (a shared record input one row every operand is fed whole, an equality; a shared variant output the inclusive union), responsibility is exclusive, evidence only on owned sides — so each merge signature is two words, one per side; the owned sides also carry the `DisjointLabels` detector, which turns a duplicated label into a custom compile error naming the label) + the two `dimap`-only widening reshapings (`widenRecordInput`/`widenVariantOutput`) the `PUI` merge instances build on — `widenRecordInput` is **library plumbing, not vocabulary** (a coercion, no longer a `Union`): it is deliberately not re-exported from `PUI`, because no stage subsumes — a stage is typed at one row, and a business function is typed at that row, its signature the view's hint verbatim (guardrails L18), so a demo never coerces at the call site. The no-nominal-types-in-UI rule (every view-model type anonymous and structural) is stated for application code in `.claude/skills/developing-bambik-apps/writing.md`.
  - **`test/Main.purs`** — value-level `(->)` tests (`subStrong`/`focusField`/`toCase`/`unfirstFrom`) plus `fold`'s laws on the probe carrier, merge unit laws, gating, and the trace quartet and its row forms (`unfirst`/`unleft`/`coresolve`/`coretain`/`looped`) on the `PUI` carrier via a probe harness; **`test/HelloShutterReel.purs`**/**`BusinessOptics.purs`**/**`RestaurantReel.purs`**/**`EntityEventExample.purs`** — `Shutter`/`Reel` as business optics.

### Composition Patterns

- `Semigroupoid.do` (`import QualifiedDo.Semigroupoid as Semigroupoid` in application code — the ecosystem's sugar under its own name) - data-flow pipelines; the block's unit is the wire, `identity`
- `RecordToRecord.do` / `RecordToVariant.do` / `VariantToVariant.do` / `VariantToRecord.do` (qualified-do) - the four row merges
- label-indexed components - every MDC component is a citizen of one shape (`filledTextField @l` ×→×, `button @l` ×→+, `snackbar @l` +→×), so pipeline stages are written directly from components; `focusField`/`subStrong` nest sub-composites into larger aggregates
- variant editing - **record-shaped editor state**: the model keeps the variant, the editor keeps every payload; `bracketed @l <stateOf> <caseOf>` wraps `Semigroupoid.do { selection component; payload panes }` and lifts the result into field `l` (the variant in via a state function seeding absent payloads, out via a projection on the selection, self-traced in between) — each pane a whole-row editor stage `# inCase @l <selectionOf>` (the editor pane: existence gated on the selection's case, the rest of the row carried by the leaf's own `focusField @l` lift — no fold, no setter); consistency via the self-trace re-broadcast; unit-payload variants need only the bracket around one selection component
- conditional visibility - **case adoption, never a `Maybe`**: `provided @l <classifier>` for case-gated existence — a stored variant field — read with a plain accessor on the pane's line (`# provided @"confirming" _.deletion`, `# shownWhen @"serving" _.display`), typed by the model row the seed line declares — or, for **derived states**, one variant-returning classifier per rule family whose cases carry their panes' payloads (`# shownWhen @"taken" usernameStatus`, the classifier's cases declared on its first pane; checkout's `checkoutStep`, calculator's `readout`, inbox's `messageView` converting `find`'s `Maybe` at the boundary), exclusivity by construction: conditional *data* (a pane whose content only exists sometimes) → `pane # shownWhen @l <classifier>`; conditional *mode of a live editor* inside a `looped` ensemble → the editor pane `# inCase @l <classifier>` (order-form's fulfillment panes, flight-booker's return date, meeting-booker's attendees slider), never a payload pane folded back with an identity setter. `clWhen` stays predicate-driven — it toggles a class (styling), not visibility
- gated displays - live views as pipeline stages (slider readouts, summary lines, data tables): each rung renders per its policy and releases the fed row; read functions are typed at the fed row, their signatures the view's hints verbatim

### Separation of Concerns

- **Business Logic** - Row-shaped models, structural throughout, per the application code-style contract in **`.claude/skills/developing-bambik-apps/writing.md`** (see the note at the top of this file — it is the single normative statement, and demos are its executable form): no nominal types in UI, signatures verbatim from the view's hints, folds as record updates, fold handlers in the Mealy step's own shape `payload -> state -> state`, no business literals in UI code, no pass-through fields, emissions carrying bare data. Below the UI sit plain functions and Aff actions (demo/nguis/order-form-mdc2/OrderFormMDC2.purs), nominal types only where recursion (cells' `Expr` AST — rows can't express μ) or an ecosystem API (`Aff`, `Either`, `Milliseconds`) demands them, and business optics (Shutter/Reel) where state/loop semantics are needed (test/BusinessOptics.purs)
- **Design System** - Oculars in the vocabulary modules (`PUI.Web.HTML`/`SVG`, MDC2, MDC3, Shoelace, Fluent, Bootstrap); the type `Ocular p = forall a b. Optic p a b a b` is declared in **src/PUI.purs** beside its sibling `Action` — the optic transpose (fix the carrier, quantify the data), with its admission law in the header
- **Composition** - UI elements compose orthogonally to the row combinators

## Demo Structure

102 pages over **40 app families**, registered in **scripts/demos.mjs** (the
single source of truth: directory + module + entry, shared by the bundler and
the dev server). Two suites: **demo/7guis/** (the
[7GUIs](https://eugenkiss.github.io/7guis/) benchmark) and **demo/nguis/**
(popular showcase apps, mostly one combinator each).

**Conventions every demo follows** — stated here once, not per demo:

- **View/view-model module separation** (writing.md): the demo module is the
  view (design-system vocabulary + the view model module); a
  `<Demo>ViewModel` module holds the seed and the pure business functions
  and depends only on the domain — and **the view determines it**
  (guardrails L18): with a typed hole for every value the view imports, the
  compiler's hole list is the module's signatures, verbatim, with
  nothing unknown (`npm run check-determined` checks both, every demo). Renamed from `<Demo>Logic`
  (inbox 2026-10-01, the rest 2026-10-02). Twins share that module
  *verbatim* from the unsuffixed sibling directory
  (`demo/nguis/inbox/InboxViewModel.purs`, fetched by pages as
  `../inbox/InboxViewModel.purs`), so **a twin diff is view-only by
  construction**. Single-variant demos keep it beside the app; helloworld
  (all view) has none.
- **Named module + entry, never `Main`** (`CounterMDC2`/`counterMDC2`), so
  every demo compiles under one `spago build`.
- **Vocabulary suffix = the design-system switch**: `-mdc2`/`-mdc3`/
  `-shoelace`/`-fluent`/`-bootstrap`/`-html` sibling directories over the same
  logic. demo/page.js probes the siblings and injects a switcher listing the
  ones that exist; unsuffixed pages get none. The per-variant diff is the
  honest catalog mapping (typography renames per the Material migration guide;
  vocabularies lacking `listOf` build selectable lists as a keyed `foreach` of
  `clicked @l` rows; every vocabulary has `indeterminateLinearProgress` since 2026-10-07).
- **The app shape** is one loop through the four shapes, `( Semigroupoid.do
  … displays and editors …; RecordToVariant.do { emitters, panes, joined
  picks }; ( VariantToVariant.do { status # action f # atCase @l … } ) # subChoice; VariantToRecord.do { status @l line # fold f …; statuses } ) # looped @( … ) # with seed`
  — the model row declared where the model first appears, every event into its own
  memoryless `status @l line # fold f`, closed to `PUI Web {} model`; what must show at mount
  stands before the ensemble, since a stage after the fold is fed only by
  events; `# with @{ … } seed` for a flow with no loop; a knot with a
  load action before it carries no row and takes no seed of its own —
  the action's outcome declares the model and `body`'s one feed of `{}` runs the load
  (order-form, crud); a loop with no
  events has no fold; the drawer's nav folds its pick in place with
  `updated`, its content being fed from it (photo-gallery).
- **Naming**: MDC2/MDC3 name the component vocabularies, modules, directories
  and UI labels; plain MD2/MD3 is reserved for the design-system specs
  (m2/m3.material.io) in prose.

### 7GUIs — all seven in all six vocabularies

| Demo | What it shows beyond the benchmark task |
| --- | --- |
| counter | the floor: one display, one emitter, one fold — `headlineLarge (text countLine) # shown` (×→×), `button @"Count" {}` (×→+), `snackbar @"Count" countedLine # fold increment` (+→×, the status opening the fold), closed by `looped @( counted :: Int ) # with freshCount` |
| temperature-converter | both fields in the model; non-numeric input leaves the other untouched |
| flight-booker | type-changing `select @"Flight type" {}` over an anonymous variant row; both outcomes carry bare payloads into two sibling statuses, `snackbar @"booked" bookedLine` and `snackbar @"rejected" rejectedLine` |
| timer | `ticks @"tick"` replayed and folded by a total `tick`; `sliderLive` duration re-scoped at runtime |
| crud | **a load action before the knot**: `( indeterminateLinearProgress # action loadPeopleCatalogue; snackbar @"People loaded" … # fold identity; ( editors; list and buttons; folds ) # looped @( … ) )` — the load's outcome folded in, the knot declaring the model; `MDC2.listOf @l` (keyed `foreach` of `clicked @l` rows elsewhere), its pick `# joined @"Person picked"`; Aff catalogue actions over a stand-in server in its own module (`PeopleServer`), each typed at the actions block's six-case outcome row and returning its own two, every outcome folded once by its status (experiment 2026-10-07) |
| circle-drawer | **channel-fed SVG canvas** — built once, updated via `attrWith`; container-level `onClickedXY @l`; the diameter a bounded quantity in the model, its slider `# inCase @"chosen" _.selected # settled resizeSelected` — live-preview resize as a state invariant — and the canvas click `# joined @"picked"`, an `adjusting` flag coalescing a drag into one undo transaction |
| cells | **channel-fed 31×27 grid** — ~800 cells built once, `attrWith` + `text` in place, clicked key via `clicked @l _.key`; hand-rolled formula evaluator over an `Expr` AST (nominal, since rows can't express μ) |

The `-html` variants are the **plain-HTML floor**: one container `div` (so
case panes re-attach inside the demo's own DOM), label-indexed leaves like
every vocabulary's (`input @"Name" "text"`), captions as `label`+`staticText`
merges, native `select` and `output`.

### nGUIs — one combinator each

**Flagship.** order-form is the **four-shape showcase**: load action →
`×→×` `looped` form (whole-row editor stages, sub-records nested as labelled
groups via `group @l` — Identifier, Customer, Fulfillment (its variant
under `"Mode"`, `bracketed @"Mode"` doing that field's lift), Payment
(Total beside Method and Paid) and Kitchen (the remarks), each label the
field, the heading and the accessible group name at once, and the live
summary on a blind `card`, the one surface that edits nothing; variant editors as `bracketed @l`
pipelines of `tabBar`/`segmentedButton` + `inCase` editor panes; an **in-form Aff action** — the delivery distance is
estimated on a button, `button @"Estimate distance" {}` →
`action estimateDistance # atCase` → `updated`; the estimate records the
address it was made for and `settled staleDistanceForgotten` keeps that
invariant, so an address edit drops it — the effect runs on an occurrence,
never on the loop's broadcast, and `settled` normalizes, never reacts) → gated live summary
→ `×→+` event buttons `# armed` → each backend action (`+→+`) followed by its
own status snackbars (`+→×`), so each action's outcomes are named where it is.

**One combinator each** (the knot and the focus pair get a
focused demo apiece):

| Demo | Combinator / point |
| --- | --- |
| auction | `settled` as a running invariant — the highest bid is a model field (`top`), raised by the slider stage's normalizer (`sliderLive @"Your bid ($)" {} # settled raiseTop`); until 2026-10-05 a hidden `feedback @"top"` channel |
| checkout | a wizard whose step is a model field — Next/Back are **two business actions**, each `# provided` on the step classifier and `# joined` with the model, folded by one `stepTo`; until 2026-10-05 `folding @"next" @"step"` looped the step silently beside the model |
| payment | an **action that retries** — the flaky gateway is retried inside `chargeFlaky`'s `Aff` until approved, the outcome one `charged` case into the fold, the attempt counted a payload the status line reads. Also the **`observed`** showcase: `snackbar @"Charge card" chargingLine # observed` narrates the charge on its way to the action while the event passes on. Until 2026-10-06 the retry was a nested variant knot (`cycled`) |
| ticket-dispenser | the floor of the `+→×` shape — "take a number" replays the model and `snackbar @"Take a number" ticketTakenLine # fold issue` advances the counter, a model field (`next`); until 2026-10-05 `unfolding @"resume" @"next"` resumed it through a `Reel`. Also the **`shownWhen`** showcase: state is a payload-carrying variant field (`display`), so the number and hint panes are pure case adoption off the stored field (`# shownWhen @"serving" _.display`) |
| parcel | `subStrong` — a reusable address sub-form as a citizen over its own closed row, background field threaded |
| cashbox | `subChoice` — selective interception as UX: outgoing money detours through confirmation dialogs, incoming posts straight to the fold; every button replays the till, so every handler is `{ balance :: Number } -> { balance :: Number }` with its amount baked in (`refundStandard`) |
| potluck | `acted` (the container action) — per-guest dish selectors under one model, each `segmentedButtonUnpicked @"Dish" @"chosen"`; the table's state is the business classifier `menuState` (`complete` with the dishes, `waiting` with the guests still choosing), each case a `shownWhen` pane — the waiting pane names who is left, the menu prints once the table is complete |
| departures | `dispatched` (+→+ keyed input) — rows appear on first mention, re-feed in place, tagged output drives a last-update line |
| scoreboard | `accumulated` (+→× keyed input) — board grows to its key set, points update in place, whole array drives the standings |
| reorder | keyed reconciliation + the `edited` collection editor — a playlist keyed by track id, element output row excluding the key (the carrier re-attaches it); Rotate and effectful Shuffle move each row's DOM node with its track, so tick, title and focus follow |
| order-dashboard | **custom components** (MDC3-only): the demo ships its own `DashboardControlsMDC3` module — five label-indexed display controls + a `board` ocular, each taking its read function like the library's own displays (`statTile "Orders placed" ordersCount`, the label stamped as the tile's accessible name), including the packaged-collection-display protocol (`leaderboard`, its `foreach` written off with `# muted`); the model holds only the order stream, every tile a function of it |

**The rest**, grouped by what they exercise: todo-list (`listOf` toggle, `clWhen`,
`segmentedButton` filter), tip-calculator (all-`×→×`, sliders, gated money
readouts), quiz (`provided` panes over one `quizPhase` classifier,
`linearProgress`), tic-tac-toe / calculator (**channel-fed `foreach` grids** — tic-tac-toe also the reset as a restart, `openingPosition` the seed and `snackbar @"New game" newGameLine # fold (const openingPosition)` —
cells built once, key emitted via `clicked` and `# joined`, folded by the fold),
markdown-previewer (`filledTextArea` + injection-proof preview as recursive
`PUI Web`: `(dynamic …) # shown` over element oculars, since structure
genuinely varies per block), stopwatch (`ticks` folded by a `tick` that steps only in the `timing` case of
a stored phase variant — a Boolean nobody edits as a Boolean is a phase — with
`# provided` button panes each `# joined` with the model and a `shownEach` lap list — whose
per-feed release *is* the sequence merge's announcing unit, so an empty
lap list never starves the gate),
shopping-cart (`dataTable` over `foreach`), password-generator (effectful
`action` returning the model), color-mixer (`sliderLive` channels
driving an `attrWith` swatch), signup-form (`debouncedTextField` username check plus two
variant-returning classifiers via `provided`, replacing five `Maybe`
projections — exclusivity by construction), photo-gallery (`imagePane`, the
channel-fed gallery: a retaining `foreach` over the pictures rather than a
wholesale rebuild), inbox (`listOf` + `dialog` + `banner` — the demo whose
MDC3 twin shows the honest catalog gap, MD3 having dropped `banner` for
`snackbar`; also the **determination pilot**, 2026-10-01: `InboxViewModel`,
the model row on its seed line, `messageView`'s cases on its pane, stored
fields read by `_.messages`/`_.deletion`, the next message id derived from
`messages` instead of stored, so every one of the view's 17 typed holes
reports a concrete signature), movie-browser (`tabBar` + `filterChip` filters over one
`visibleMovies` projection, a `foreach` of rows whose in-row `iconToggle`
folds back `# joined @"favored"` through the fold; the MDC3 twin honestly drops the MDC2
selected-row class, the favorite state riding the toggle),
weather (Aff service with a canned per-city delay), helloworld
(`body $ staticText "Hello, World!"` — the 5 kB bundle floor).

**Vocabulary showcases.** restaurant-menu is the plain-HTML one (no design
system: element oculars, `cl`/`:=` decorators, `each` from data, no seed
at all; the fine-dining look is ordinary CSS). espresso-bar is the MDC3 one
(with an MDC2 twin generated in reverse). One per non-Material vocabulary,
suffix naming the vocabulary rather than a twin (so a suffix means
"this vocabulary", not "has a twin" — order-dashboard-mdc3 is single-variant
too, while only helloworld and restaurant-menu, which use no design system at
all, carry no suffix): product-review (Shoelace's
exclusive star `rating`), meeting-booker (Fluent; also the **no-defaults
showcase** — nothing pre-picked, `…Unpicked @"chosen"` selectors
over named two-case fields seeded `.unchosen {}` beside an optional catering
(`dropdownOptional @"Catering" @"ordered" @"none"`, clearable back to none), no `Maybe` in the booking, the
attendees a bounded quantity *in the model*: the slider exists only once a
room is chosen (`# inCase @"chosen" _."Room"`) and the room dropdown
re-scopes its bounds as an invariant (`# settled seatsInRoom`), so an
incomplete meeting is unbookable by construction), loan-calculator (Bootstrap, all
`sliderLive`).

Verify with `npm run smoke` (scripts/smoke/, headless-Chrome CDP: the leaf-law
bench, the every-demo mount check and the carrier-only laws — no per-demo walks).

## Key Dependencies

- `profunctor-lenses` - Profunctor-based optics
- `foreign` - read a plain value's structure for the structural `Eq`/`Ord` (`Data.Profunctor.Row.Structural`), so the algebra keeps no JavaScript of its own
- `type-equality` - the equality behind a merge's shared record input (`SharedRecordInputs`)
- `qualified-do` - Syntax sugar for profunctor composition
- `material-components-web` - MDC (Material Design 2) JavaScript library
- `@material/web` - Material Design 3 web components (custom elements, used by `PUI.Web.MDC3`)
- `@shoelace-style/shoelace` - Shoelace/Web Awesome web components (used by `PUI.Web.Shoelace`; pages link its light theme CSS from the matching CDN release)
- `@fluentui/web-components` - Fluent UI v3 web components (used by `PUI.Web.Fluent`; theme tokens ship in the bundle)
- Bootstrap is CSS-only and **not** an npm dependency — `PUI.Web.Bootstrap` is native elements + classes, pages link the Bootstrap 5 stylesheet from the CDN
