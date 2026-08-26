# Bambik

## Overview

A prototype PureScript library implementing **Profunctor User Interfaces** - a novel approach to declarative UI development. The key insight is that profunctors unify optics (data structure navigation) and arrows (data flow), making them ideal for composable UI development.

**doc/guardrails.md is normative**: the strict MUST/MUST-NOT rules for the library and for applications built on it, plus the admission test every proposed feature passes (derivation → laws → subsumption → honesty → reachability → green stack → sync). Its L15 makes the demo suites the compatibility contract: any library change, however internal, must leave every demo compiling, bundling, and behaving correctly — verified by running `spago build`, `spago test`, `npm run bundle-demos`, and `npm run smoke` in full, with demos edited only when the demo itself is the subject of the change. Consult it before adding, changing, or accepting any combinator, class, component, or demo idiom — a change that violates a guardrail is wrong even if it works. Its **L16 import tower** governs imports: only the algebra layer (`PUI`, `Data.Profunctor.*` in bambik) imports the ecosystem's `Data.Profunctor`; vocabulary modules (design systems, `PUI.Web.HTML`/`PUI.Web.SVG`, packaged controls) build from the carrier + the re-exported vocabulary; application code imports neither the ecosystem algebra nor carrier internals, and `grep "import Data.Profunctor (" demo/` stays empty. Its **Part II is a pointer, not a rulebook**: the code-style contract for application code — demos included — is stated once, in **`.claude/skills/developing-bambik-apps/writing.md`** (its *Code style* section: layout, types and values, business functions, wiring). Read that file before writing or reviewing any demo or app module; nothing here or in guardrails.md restates it, and a change to how applications are written edits writing.md and the demos. The one deliberate restatement is the demo pages' `#code-style` note in demo/index.html — deployed HTML cannot read the skill file, so re-read it against writing.md whenever the contract changes.

## Building

Do `export PATH=$PWD/node_modules/.bin:$PATH` and then `spago build` (tests: `spago test`). 

### PureScript forked compiler

Note the repo builds with the forked PureScript compiler pinned in `package.json` (variant row sugar `[ l :: T | r ]`, `.label` constructors — see doc/variant-sugar.md), so `npm install` first. Nothing in the toolchain depends on a local checkout, so a plain clone builds on any Linux x86_64 machine: the compiler installs from its **GitHub release** (`erykciepiela/purescript` tag `v0.15.16-variant.6`, built from branch `variant-type-sugar` — the release also carries the bare `purs` binary as an asset, and package-lock.json pins the tarball's integrity hash), and the `Prim.Variant`-patched variant library is an ordinary git package in packages.dhall (`with variant.repo`/`.version` → `erykciepiela/purescript-variant` tag `v8.0.0-prim-variant.1`, branch `prim-variant`: `Data.Variant` re-exports the compiler's built-in `Prim.Variant.Variant` instead of declaring its own, so the sugar and `inj`/`on`/`match` share one type). External applications consume bambik as an **ordinary spago git package** pinned to a release tag (`with bambik = { dependencies = [ … ], repo = "https://github.com/restaumatic/bambik.git", version = "v0.1.1" }`) — spago clones the repo and globs `src/**/*.purs`, so no checkout sits beside the app and the demos/docs ride along under `.spago/bambik/<tag>/`. The entry must spell out the library's dependency list (spago does not read a git package's own spago.dhall), but **nothing in the repo duplicates that list**: the bootstrap procedure fetches it from the pinned tag (`raw.githubusercontent.com/restaumatic/bambik/<tag>/spago.dhall`) when it writes the app's packages.dhall, so adding a dependency here needs no companion edit anywhere. A library release is a tag on this repo. The standalone bootstrap flow lives in `.claude/skills/developing-bambik-apps/` (bootstrap.md, which carries the scaffold as inline file contents rather than a stored template), self-contained and copyable out of the repo. Beside bootstrap.md/writing.md/building.md the skill carries **vocabulary.md** (the situation-indexed lookup index into writing.md and the module headers) and **walkthrough.md** (flight-booker read line by line) — pointers, never a second statement of a rule.

### Development loop - watch mode

When introducing code changes, keep `spago build -w` running in the background and read its output after each edit (~0.7s incremental) instead of one-shot `spago build`s per change — plain `spago build -w` covers library, tests, and the demos. Caveats: spago -w reads stdin and dies on EOF, so keep stdin open (never `</dev/null`); run only one watcher over the shared `output/` at a time. Parallel subagents sharing the repo must serialize compiles (`flock <lockfile> spago build`) — and when a serialized build fails, check the per-module `Compiling` lines for *whose* module failed before assuming it's yours. It's recommended to use git workspaces in such cases.

`npm run dev` serves **every** demo from one server at `http://127.0.0.1:1234/`, rooted at `demo/` in the same folder layout the deploy scps to the remote host — local `/7guis/counter-mdc2/` is `/bambik/demo/7guis/counter-mdc2/` there, so every relative link and asset path resolves identically in both places (the server hosts nothing but the demos, so it carries no `/bambik/demo` prefix of its own). The root landing page (demo/index.html) links the two suites. Narrow the bundling to what you're working on by name or set: `npm run dev counter-mdc2 cells-mdc2`, `npm run dev nguis`.

  Auto-rebuild and browser auto-reload throughout (~2s per edit; scripts/dev.mjs — one mtime-polling watcher over src/ and demo/ driving two paths by file kind: a `.purs`/`.js` edit runs an incremental `spago build` and esbuild's own watch over `output/` rebundles the affected demos and reloads once the new bundle lands, while a page's `.html` has nothing to compile and reloads immediately — both kinds are handled independently, so an edit to each in the same tick gets a reload *and* a rebuild; reloads reach the browser over an SSE endpoint at `/esbuild` using esbuild's own protocol, injected via the bundle banner so demo pages need no dev-only markup).

  Polling rather than `fs.watch`: inotify costs an instance per directory and Node hits its per-process ceiling well before the ~40 source dirs here, while `recursive: true` silently delivers no events at all on ext4.

  Runtime emission trace: set `window.__bambikTrace = true` (or `localStorage.setItem("bambik-trace", "true")`) in the browser console to log every propagation decision — stage-to-stage flow, `looped` re-feeds/swallowed echoes, and gate-withheld emissions with the sibling fields they wait for. Independent of the trace flag, every knowledge gate carries a **starvation watchdog**: a gate that withholds and is never fed within 3s prints one `console.warn` naming the gate and (for the record merges) the exact missing fields, with the fix (`seeded`/`announce`, or the seed argument of `feedback`/`folding`/`unfolding`) — so an unprimed gate is a named failure, not a blank screen (and an unprimed *entry* is now a compile error: `body` demands input `{}`) (off until a carrier adopts the host switches — `PUI.Web.adoptHostDiagnostics`, called at `body`/`runComponentInNode`, so a headless `spago test` is silent; opt out with `window.__bambikNoWarn = true`).

Verification stack: `spago test` (value-level law tests over probes — merge units, gating, exactness, the trace quartet); `npm run smoke` (scripts/smoke/ — the committed headless-Chrome CDP harness: serves the repo, launches an isolated throwaway Chrome, runs scripts/smoke/tests/*.mjs; this is where the **carrier-only laws** are tested — the container action's keyed reconciliation/identity-follows-key in the real DOM, the gather gate on a live page (potluck) — plus per-demo smokes (including order-form's Aff load → variant-pane → summary-tap walk); the value-level `Acting` laws (empty announces `[]`, singleton retraction, gather gate, keyed retention) run in `spago test` on `PUI Effect` probes; bundle the demos first, filter with `npm run smoke -- <name>`); `npm run api-docs` generates the browsable API reference into `generated-docs/md/` (gitignored) from the module headers — the single source of truth for combinator contracts.

## Building & Deploying Demos

1. Verify the forked compiler: `node_modules/.bin/purs --version` must report `0.15.16 [development build ...]`; if it shows stock `0.15.15`, run `npm install` (stock purs fails with "Module Prim.Variant was not found").
2. Bundle for deploy: `npm run bundle-demos` (minified, all demos; `node scripts/bundle.mjs <name|set>` for a subset) — use `npm run dev` for watch mode, not for deploys. **Every** demo is a named module (`OrderForm`, `Counter`, `Cells`, `TodoMvc`, ...) entered at its own function (`orderForm`, `counter`, `cells`, `todoMvc`, ...) — no module is `Main`, so they all compile together under one plain `spago build` — and each bundles from the shared registry in **scripts/demos.mjs** (the single source of truth for directory + module + entry, shared with the dev server), which synthesizes the esbuild entry per demo because `spago bundle-app` can only call `Main.main`.
3. Deploy: `npm run deploy-demos` — scps demo/index.html and both suite directories to host `xyz` (root@erykciepiela.xyz, see `~/.ssh/config`) at `/var/www/html/bambik/demo/`.
4. Verify: `http://erykciepiela.xyz/bambik/demo/<d>/` returns 200 (plain HTTP only).

Every demo page is pure structure over **shared chrome**: `demo/page.js` (loaded as `../../page.js`) fetches the listing named by `<body data-source="CounterMDC2.purs">` (space-separated filenames render several listings — the first fills the existing box, each further file gets a filename heading + listing, the header readout sums the sizes; order-dashboard-mdc3 shows its app and its packaged controls module this way), fills the header's source/bundle size readouts, and groups the running demo with its tracing note into one `#demo-column` — the demo mounts into `<body>` at runtime with no marker class of its own, so it cannot be wrapped in static markup and is collected once, then the observer disconnects. The two notes that used to be pasted into all 34 pages now live once in **demo/index.html** as `#code-style` and `#tracing` sections, linked from beneath the source box and beneath the running demo respectively.

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

### Key Source Files

- **src/PUI.purs** — the core profunctor and the carrier-independent algebra's
  hub. Instances: `Profunctor`, `Strong`, `Choice`, `Semigroupoid`, `Category`
  (`identity` is the echo wire), the four row merges, `Joining` (the ungated
  joint merge — broadcast in, interleave out, last writer wins; the class is
  at the profunctor kind, deliberately not `Semigroup (PUI m a b)`), the two mixed strengths
  (`Resolving`, `Retaining`), and the **trace quartet** — `Costrong`/`Cochoice`
  (ecosystem duals of `Strong`/`Choice`: state feedback and iteration;
  knowledge-gated) plus the coined `Coresolving`/`Coretaining` (terminating
  fold, productive unfold). Each co-strength is its strength's retraction:
  `co (strength g) ≅ g` once the state channel is primed. Also `Hosting` and
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

  The **vocabulary** it defines or re-exports (contracts in the headers):

  | Word | What it is |
  | --- | --- |
  | `silence` | the silent UI component, `×→+` shaped `{ \| i } → [ \| o ]`; silence forced by parametricity |
  | `blank` | the faceless *record*-output leaf `{ \| i } → {}` (record outputs must announce); for elements whose whole face is decorators |
  | `announce` | the announcing constant — one registration emission of a row of fields; seeds fields as a merge operand |
  | `with` | discharge the initial-state obligation, `announce a >>> w`; record-shaped on the **input** side only and output-polymorphic, so it closes record pipelines *and* seeds a `×→+` emitter's replay payload — `button @l {…} # with patch`, leaf leading the line |
  | `mvu` | the named app shape, `with seed (looped w)`, closed to `{}` |
  | `seeded` | the seeded echo wire (`Seeding`'s method): pass-through plus one emission of the seed |
  | `looped` | the `×`-diagonal self-trace (`Looping`'s method), re-entrancy-guarded |
  | `joint` | the ungated joint merge (`Joining`'s method): two same-typed whole-row citizens over one channel pair — broadcast in, interleave out, last writer wins; for controls deliberately writing one field (tip-calculator's dual-bound slider + range) |
  | `settled` | `rmap`-only normalization over a stated sub-row footprint |
  | `updated` | the Mealy update stage: fold each event emission of a wrapped `×→+` component into the retained value; **both sides subsume** |
  | `applied` | the occurrence stage — `updated` for an emitter fed the row it acts on (a button replaying its row): `f :: state -> state` steps the retained row per emission, the emitter's input row pinned to `f`'s footprint by the signature, so `button @"Add" {} # applied addTodo` states label and model once each; law `applied f = updated (const f)`. The subsuming stages (`updated`/`applied`/`every`/`settled`) each state their footprint as one constraint, `Union small rest big` — the model is the footprint plus the rest |
  | gated displays | displays are pipeline stages natively, typed `p { o \| rest } { o \| rest }` — a pass-through whose **release is the fulfillment witness**, gate policy baked into the component. The family (PUI.Web.HTML unless noted): `shown content` (ambient structured content — chrome registers at build, renders per feed, releases always), `shownWhen proj content` / `shownCase @l f content` (display panes: attach on relevance, release always), `inCase @l f editor` (the editor pane — `shownCase`'s editor sibling, the pane `joint` the wire), `shownEach @l proj item` (keyed collection), `confirmed cfg $ content` (MDC2/MDC3 — the witness rung: modal, flow withheld until the user confirms). Content slots accept only `{}`-output components, keeping the no-silent-loss law; `observed` unchanged |
  | `muted` | the counit: render, and **deliberately discard** the component's output (`rmap (const {})`) — the visible form of what no stage may do silently; `# muted` writes off a genuinely emitting assembly (a `foreach` forwarding its elements inside a packaged control, scoreboard's summary group) so it can end at `{}` |
  | `observed` | the gated displays' `+`-diagonal sibling: every event forwards once at feed time; the status's own emissions are dropped (events are one-shot) |
  | `required` / `optional` | adopt a type-changing selector as an always-selected / possibly-unselected **whole-row citizen** (label derived from the leaf's closed rows, background carried like `field @l`'s); `optional` keeps the `Maybe` and completes the `Just`-only echo, so an unmade choice is honest knowledge rather than a starved stage |
  | `every` | the heartbeat wire: pass-through plus a periodic step over a sub-row, merged back over the last value |
  | adopter family | `asField`/`projection`/`projected`/`forProperty`/`atField`/`field`/`subStrong`/`subChoice`/`toCase`/`toCases`/`atCase`/`forCase`/`forCases`, plus `acted`/`optioned` |

  Durations are structural `{ ms :: Number }` throughout (`every`, `debounced`,
  `resolveFor`, `debouncedTextField`) — never `Milliseconds`.
  `synced`, `latch` and `constantly` are **deleted**: ensembles are `bracketed`
  record merges, and `constantly` was `()`-subsumption in disguise (positions
  whose mechanism subsumes take `pempty` directly; constant catalogues enter
  through the consuming mechanism's projection argument).
- **src/PUI/Web.purs** - the carrier **and the root of the web layer**: DOM monad (`Web = StateT DOM Effect`), `Node`, DOM building blocks (`element`, `attachable`, `runDomInNode`) and FFI — no UI components. Everything browser-specific is a submodule of it: the element vocabularies `PUI.Web.HTML`/`PUI.Web.SVG` and one module per design system (`PUI.Web.MDC2`, `PUI.Web.MDC3`, `PUI.Web.Shoelace`, `PUI.Web.Fluent`, `PUI.Web.Bootstrap`), all under **src/PUI/Web/** — so the carrier-independent algebra (`PUI`, `Data.Profunctor.*`) stays visibly separate from the web specialization
- **src/PUI/Web/HTML.purs** — the 1-1 HTML vocabulary over the carrier.

  **Entry**: `body :: PUI Web {} o -> Effect Unit` registers the wiring and
  feeds nothing, so it demands a **closed** app — a pipeline's residual input
  row *is* its initial-state obligation, discharged by `with initial`/`mvu seed`
  down to `{}`, the one self-pointed record. A forgotten seed is therefore a
  compile error at the mount point, naming the unsupplied fields.

  **Interaction vocabulary** (the collection combinators `foreach`/`edited`/
  `acted` live at the **PUI level** — see the container-action bullet):

  | Word | What it does |
  | --- | --- |
  | `attrWith` | value-computed attribute — the channel-fed counterpart of static `attr`/`:=`, so a cell's style/coord/colour updates in place through the channel rather than by rebuilding a closure |
  | `clicked` | click emitter for any element (`button`'s replay-last-value protocol); row-shaped, since replay is lawful over records only, and its **content subsumes** — a multi-reader content states its row once in a named closed *face* function |
  | `provided` | the view-model conditional: its argument is a named `Maybe`-valued projection, content attached and fed on `Just`, detached on `Nothing`. The pane consumes the payload, never the whole model, so visibility logic lives in testable business code. It **detaches**, so it is a pipeline stage, not a gated-merge operand |
  | `providedCase` | `provided`'s `+` sibling — case-gated existence over a stored variant field or a variant-returning classifier, so mutually exclusive states are exclusive by construction |
  | `clWhen` | value-dependent class — styling, deliberately last-element-only |
  | gated display rungs | `shown content` (ambient structured content, registered at build), `shownWhen proj content` / `shownCase @l f content` (display panes), `inCase @l f editor` (editor pane), `shownEach @l proj item` (keyed collection) — each `p { o \| rest } { o \| rest }`, releasing the fed row per its policy; `confirmed` (the witness rung) lives in the design systems |
  | `onClickedXY` | container-level pointer-down coordinates (local/viewBox `{ x, y }`) for canvases |

  Announcing statics are `staticText` and the void `hr` (`{} → {}` chrome); the
  raw-HTML `staticHTML` lives one level up in `PUI.Web`, not here, since L10
  keeps an HTML-string surface out of the public vocabulary. `input`/`textArea`
  are focus-guarded. The **element oculars** cover the usual set
  (`div`/`span`/`table`/`tr`/`td`/`ul`/`li`/`p`/`h1`–`h6`/`img`/`a`/`label`/
  `strong`/`em`/`code`/`blockquote`/`header`/`footer`/`section`/…, plus the
  generic `el` for computed tags) with `attr`/`:=` and `cl` decorators; the
  **SVG** oculars (`svg`/`circle`/`path`/`text`) live in **`PUI.Web.SVG`**,
  imported qualified when a component needs both the HTML `text` leaf and the
  SVG `<text>` element. SVG works because `element` is namespace-aware
  (`svg` opens the SVG namespace, children inherit, only SVG-namespaced
  elements use `createElementNS`; HTML stays on `createElement` so MDC init is
  unaffected).

  The native elements with a model interface are **label-indexed components**
  (L3), each stamping its label as the host `name`: the type-changing
  `select @l` (`Cons l (Maybe a)` in, `Cons l a` out — bare
  `<select>`/`<option>`s, no caption chrome of its own), `rangeInput @l`
  (`<input type="range">`, the live bounded-quantity slider over
  `Cons l { current, min, max, step }`), the `progress @l` display
  (`Cons l Number`, fraction 0–1) and the `output` status (the one fixed
  canonical row here, `[ event :: String ] → {}` — HTML's element for the
  result of a user action, shown in place since plain HTML has nothing
  self-dismissing). `radioButton` stays a scalar optic-position
  leaves. Scalar editors take their business label in app code via `field @l`
  (`input "text" # field @"Name"`).

  **Structure computed from data is `PUI Web` all the way down** — no markup
  DSL — in two regimes:

  1. **Fixed structure, changing values** (grids, an SVG canvas): feed the
     structure as data through the retaining `foreach` and compute each
     element's content/attributes from its fed value — label-indexed `text @l`
     for content, `attrWith` for style/coords, `# toCase @l _.key` to emit
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
  exactly one direction (the boundary reading — displays as assurance
  policies, sources as the seed generalized, user input as occurrences —
  is doc/displays-and-sources.md): `×→×` editors (text fields, `checkbox`,
  `toggleSwitch`, `slider`/`sliderLive`) and displays (progress/gauge), `×→+`
  events (`button` and its emphasis siblings, `fab`, `iconButton`, `menuItem`),
  `+→×` statuses (`snackbar`/`toast`/`messageBar`/`banner`), plus the
  type-changing selectors (`select`, `radioButton`/`radioGroup`,
  `segmentedButton`, `dropdown`) shaped `Cons l (Maybe a)` in → `Cons l a` out.
  *Oculars* are shape-preserving decorators with no model of their own
  (`card`/`cardActions`, dialogs, lists, typography, elevations) — and a
  **surface ocular carries no copy config**: MD2 gives a card twelve optional
  structure classes and no heading, MD3's card element is a bare `<slot>`, so
  `card` is a plain `Ocular` in all five vocabularies and a card's heading is
  ordinary typography placed in its content (order-form's sections lead with
  `subtitle1`/`titleMedium`). Config on an ocular would put one child of the
  surface in config position and the rest in content position.
- **Every leaf is label-indexed** (L3) and captions itself from that label
  verbatim; editors also stamp it as the host `name`. Config overrides carry
  real copy the label cannot be — the key is `floatingLabel:` on the MDC text
  fields and `select`, plain `label:` elsewhere.
- **Leaf-echo protocols** are identical across all five: focus-guarded text
  fields (model updates never clobber the field being typed in, and the channel
  stays live), per-feed display echo, `Just`-only echo on type-changing
  selectors, and `clicked`'s replay-last-value protocol on emitters.
- **The `dimap` round-trip contract for editors** (stated in each module
  header): an editor bracketed by `dimap f g` behaves as an iso lens; lossy or
  failing conversions belong in the model (`settled` on the whole-row stage), never
  in a leaf bracket.
- **Same names and signatures wherever both catalogues have the concept**, so a
  screen changes design system by changing one import. A catalogue's honest
  exclusives and honest gaps appear under their own names — the per-module
  deltas below. Bounded quantities ride one row everywhere:
  `{ current, min, max, step }` as **model data from the seed**, re-scopable at
  runtime, never UI config.

Per-catalogue deltas:

| Module | Basis | Deltas worth knowing |
| --- | --- | --- |
| `PUI.Web.MDC2` | `material-components-web`: documented markup + a foundation instance (`newComponent material.x."MDCX"`) wired through its documented properties/events; text fields write through the foundation's `value` so label float stays foundation-managed | the fullest catalogue: `listOf` (a **dynamic collection component**, `{ \\| provided } -> (i -> Array { \\| r }) -> PUI Web { \\| r } o -> PUI Web i { \\| r }` — keyed `foreach` retention, MD2 selected styling via an optional `selected` predicate), `dataTable`/`dataRow`/`dataCell`, `imageList`/`imagePane` (the channel-fed sibling of the static `imageListItem`), `layoutGrid`, `topAppBar`, `drawer` (permanent, with a **live nav slot**: nav and content are sibling stages over the same types), `tooltip`, `banner`, `tabBar` (the same-type selector with unconditional echo — the `looped`-ensemble citizen), `menu`/`menuItem`, `chipSet`/`filterChip`, `iconToggle`, `dialog`/`simpleDialog` (modal protocol: **open on feed, close on emission**) |
| `PUI.Web.MDC3` | Google's `@material/web` custom elements — a leaf is `element "md-…"` plus property/event wiring: no foundation classes, no hand-fused ripple/label chrome | structured to **mirror MDC2** (same helper shapes, same definition order). MD3 renames arrive as the catalogue does: the MD3 typescale (`displayLarge`…`labelSmall`), four emphasis siblings (`elevatedButton`/`tonalButton`/`outlinedButton`/`textButton`), `elevation1/3/5`, and **no `banner`** (MD3 dropped it). Catalogue entries `@material/web` lacks (segmented button, snackbar, card, top app bar, drawer, data table, image list, tooltip) are hand-rolled over the `--md-sys-*` tokens, each injecting its stylesheet once via `ensureStyle`; pages need only the Roboto + Material Symbols fonts |
| `PUI.Web.Shoelace` | `@shoelace-style/shoelace` custom elements, Lit-based so no bind deferral | the MDC3 recipe verbatim. Exclusive: the star `rating` editor. Shoelace's own names where the concept differs — `textField`/`textArea` (no fill/outline split, plain `label`), `toast` (`<sl-alert>`), `progressBar`, `sliderLive` (`<sl-range>`). Page links the light-theme CSS from the CDN; icons from the CDN base path set in the FFI. Typography is deliberately absent — Shoelace styles plain HTML, so the HTML oculars *are* the type scale |
| `PUI.Web.Fluent` | Microsoft's `@fluentui/web-components` v3; tokens set globally from `webLightTheme` at load, so pages need no CSS link; labels associate via `<fluent-field>` wrappers | exclusives `ratingDisplay` (read-only — the catalogue has no star *editor*, and this vocabulary does not invent one) and `messageBar`; type ramp `title3`/`body1`/`caption1` over `<fluent-text>`. **Caveat**: FAST binds a beat after DOM insertion and replays pre-bind property writes at bind, and its update queue is rAF-driven (starving in frameless headless sessions) — so the dropdown/radio-group leaves defer writes on a **timer** poll (`whenBoundDo` in Fluent.js) and finish the two starvable registrations themselves; the dropdown's options must be wrapped in `<fluent-listbox>` (v3's markup contract) |
| `PUI.Web.Bootstrap` | **CSS-only**: native elements dressed in documented classes (`form-control`, `form-select`, `form-range`, `btn btn-primary`, `progress`, `toast`, `card`, `list-group`, `badge`) — no component JS, not an npm dep; the page links the Bootstrap 5 stylesheet from the CDN | the only FFI is the toast's `autoDismiss` timer (what Bootstrap's own JS plugin would do). No commit/live slider split (`sliderLive` only; the label line carries a live numeric readout). `listGroup`/`listGroupItem`, `badge`; typography is plain HTML |

Internals (MDC2/MDC3): the live leaf is `field @l`-lifted — `field` is the
`Strong` field lens, so every editor is a **whole-row citizen**
`p { l | rest } { l | rest }` whose emissions re-attach the background the
lens retains (runtime completeness by construction; freshness rests on the
enclosing loop's re-broadcast) — with hand-fused chrome where abstract
labels can't flow through the merges' `Nub`, while all-chrome groups have
concrete rows and stay literal `RecordToRecord.do` merges of announcing chrome
(`staticText`/`staticHTML`/`pempty` at `{} → {}`). Code order = DOM order.
- **No canonical labels: leaves state business labels, adopters derive them** (L3).

  every canonical-row leaf is **label-indexed**: the business label is a visible type argument on the leaf itself — `text @"Total"`, `filledTextField @"First name" {}`, `select @"Milk" cfg opts`, `button @"Submit order" {}` — so a merge operand or emitter states its row once, at the leaf, and nothing in application code ever says `value`/`clicked`/`event`.

  Adopters that need the leaf's label **derive it** from the closed singleton row via `RowToList`'s fundep instead of taking it as an argument: `# projection f` (retype the field through a formatter), `# projected f` (whole-value read), `# forProperty` (context-pinned wider row), `# required`/`# optional` (selector completion), `# toCases f` (emitter case into business outcomes), `# forCase @l line`/`# forCases classifier` (statuses derive their own case; only the business case is written). `forField` and `asCase` are DELETED — their rename job moved onto the leaf.

  The label also names **and captions** the component: editors stamp it as the host element's `name` attribute, and every captioned leaf — editors' `floatingLabel`/`label` and the whole `×→+` emitter family (`button`/`outlinedButton`/`textButton`/`elevatedButton`/`tonalButton`/`fab`/`iconButton`/`menuItem`, in all six vocabularies) — defaults its caption to the label **verbatim** (`OptCaption` in `PUI.Web`, shared by all six vocabularies; the MDC modules add their own `OptLabelIcon`/`OptLabel`/`OptIcon`/`OptSelected` for their richer faces). **Nothing derives a caption from an identifier** — `humanizeLabel` is DELETED: a label *is* the copy it draws, so it is written as such and is usually a quoted string, since human copy is no identifier (`filledTextField @"First name" {}`, `button @"Submit order" {}` drawing those words and emitting `[ "Submit order" :: _ ]`, quoted at every mention — `atCase @"Submit order"`, `match { "Submit order": … }`). Because a leaf's label is the model field it edits, **the business rows carry the same quoted labels** (`{ "First name" :: String }`), whose one syntactic cost is that a quoted label cannot appear in a **record pun** — the logic modules bind explicitly instead (`createPerson { "Name": name, "Surname": surname, people }`), while field access, accessor sections and update syntax are unaffected. **No demo passes an emitter `label:`**: where a trace form's loop case would force two buttons to share one case under different words, the buttons are two business actions — each takes its own self-describing case and `# toCases` adopts it into the loop case, so the fold still sees one case while each button reads as what it does (checkout's `button @"Next" {} # toCases goneOn`, `button @"Back" {} # toCases goneBack` over `folding @"next"`). For emitters `label:` is left only for a glyph-only face (`fab { label: Nothing }`).

  For **editors** the same rule holds, and **no demo passes a caption config at all**: the label carries the full copy, punctuation and units included — `filledTextField @"Start date (DD.MM.YYYY)" {}`, `sliderLive @"Amount (€)" {}`, `filledTextField @"Formula (e.g. =SUM(A0:A5)*2)" {}`, `filledTextField @"What needs to be done?" {}`. The type argument *is* the caption, so a leaf never states its copy twice. Selector **options** follow the same rule via `choice` (in `PUI.Web`): a choice states its copy once, at its case, and the `{ value, label }` echo disappears — `dropdown @"Room" {} [ choice @"Focus pod (4 seats)", choice @"Boardroom (12 seats)" ]`. `choice @l` is a plain value, so the options stay an ordinary array and their order is the order written — deliberately **not** the variant row's, which the compiler sorts alphabetically, while option order is a design decision (rooms by size, durations by length). The vocabulary still keeps `floatingLabel:`/`label:` in its signatures for real copy a label genuinely cannot be — localized wording above all.

  An editor whose text is *derived* from sibling fields keeps the derived texts as model fields and normalizes them into each other with `settled` (temperature-converter holds both `@"°C"` and `@"°F"` texts — a label is any string, so where a symbol *is* the conventional caption it is written as one — each field's stage running `# settled fromCelsius` / `# settled fromFahrenheit`, a failed parse leaving the sibling untouched)
- **extras/** - the layer the shape modules stand on, extracted so `Data.Profunctor.Row.*` holds only what mentions a row, laid out **exactly as the ecosystem lays out its own**, and kept outside `src/` under separate source roots because claiming an ecosystem module name is a claim about what the module *is*. Everything here but `extras/variant` and `extras/qualified-do` is **unreached by the library, the demos and the tests** (L14 would otherwise prune it) but stays in the build glob so it cannot rot; all of it but `Cont` is also non-row and non-carrier, mentioning neither `PUI`, a row nor a carrier.
  - **extras/profunctor/Data/Profunctor/{Resolving,Coresolving,Retaining,Coretaining,Joining}.purs** — one *class* per module beside `Data.Profunctor.Strong`/`.Costrong`, so the coined strengths, their co-strengths and the juxtaposition (`Joining`/`joint`, the ungated joint merge — `ArrowPlus`'s `<+>` at the profunctor kind) are five separate files, stated positionally with `Tuple`/`Either` or at bare `a b`, no row in sight. Pure **complements of the ecosystem's own**: liftable into `purescript-profunctor` unchanged.
  - **extras/profunctor/Data/Profunctor/Cont.purs** — the root's one **carrier**, and so its one member that is *not* liftable: the CPS profunctor `Cont r a b = (b -> r) -> (a -> r)`, the repo's only *pure* carrier of the row algebra — a timeless model where the merge gate is continuation nesting rather than a pair of `Ref`s. Its header inventories, **exhaustively** over every profunctor subclass in the repo and the ecosystem, which classes it validly inhabits (`Strong`, `Choice`, `Category`, `Wander`, `Acting`, `Cochoice`, `RecordToRecord`, `VariantToVariant`, `Monoid r => RecordToVariant`, plus the degenerate `Resolving`/`Coretaining`) and which it provably cannot (`Costrong`, `Coresolving`, `Retaining`, `VariantToRecord`, `Seeding`, `Closed`), each with its reason — so an absent instance is a stated impossibility, never an unwritten one, and those impossibilities are exactly why the trace forms take seeds and `looped` is a primitive. It builds (it was parked until 2026-08-11, which is what let the inventory drift), so a move in the `Data.Profunctor.*` layout that invalidates it is a compile error rather than silent rot.
  - **extras/lenses/Data/Lens/{Colens,Coprism,Shutter,Coshutter,Reel,Coreel}.purs** — one *optic* per module beside `Data.Lens.Lens`/`.Prism`, each carrying its type, its collapsed constructor and its `*E` existential encoding at arbitrary `s t a b`. `Colens`/`Coprism` are the optics of the *ecosystem* classes `Costrong`/`Cochoice`, which `profunctor-lenses` never built, so they too are liftable as they stand; `Shutter`/`Coshutter`/`Reel`/`Coreel` are coined class and optic alike, so each would travel with its class. Plus **extras/lenses/Data/Lens/Prism/Existential.purs** (`prismE`, the existential constructor of the ecosystem's own `Prism`, which `Data.Lens.Prism` does not export — it *extends* that family rather than shadowing it, which is why it is not named `Data.Lens.Prism`): the purest complement in the tree, since both the optic and its `Choice` are already the ecosystem's.
  - **extras/variant/Data/Variant/Case.purs** — the **value-level label read** `caseText` (the case label of a variant value, verbatim — `unvariant` + `reflectSymbol`, a composition `purescript-variant` never exported, so liftable unchanged; law `caseText (inj @l a) = reflectSymbol (Proxy @l)` in the header). The one extras module that is demo-reached and law-tested rather than parked: under L3 a case label *is* the copy it draws, so application code reads labels back with `caseText` instead of `match`-restating them (espresso-bar's summary, order-form's "Paying by cash", potluck's menu, meeting-booker's booked line, product-review's preview) — options are therefore labeled as the exact copy the line needs (`choice @"with oat milk"`, `choice @"cash"`), while a map that does real work (meeting-booker's `roomText` shortening, tic-tac-toe's glyphs, signup-form's sentences) stays a named copy function. The rule as applications read it is writing.md's *A label is read back, never restated*; `Data.Variant.Case` counts as a domain module, importable from logic and view alike.
  - **extras/row-profunctor/Data/Profunctor/{Row,Row/*,Acting,Seeding}.purs** — the **row-profunctor algebra itself**, and a different claim from the two roots above: not anyone's complement but bambik's own invention, yet still carrier-agnostic (the algebra of merging labelled rows; `PUI` is one carrier that satisfies it, `(->)` another for the value-level laws). With this root out of `src/`, **`src/` holds exactly the carrier and its vocabularies** — `PUI`, `PUI.Web` and `PUI.Web.*` — so the split reads: `src/` is the UI library, `extras/` is the algebra it stands on.
  - **extras/qualified-do/QualifiedDo/Category.purs** — the pipeline sugar: `QualifiedDo.Semigroupoid` at `Category`, `bind`/`discard` verbatim, so `Category.do` names the structure a pipeline composes in (unit `identity`, the wire); qualified-do stops at `Semigroupoid`, so this is a liftable complement like the lens and profunctor roots. Every demo imports it `as Category`.
  - All five roots are covered by the single glob `extras/**/*.purs` in spago.dhall's `sources` beside `src/**/*.purs`, and watched by scripts/dev.mjs. **Downstream caveat**: spago globs a git dependency as `.spago/<pkg>/<ver>/src/**/*.purs` — hardcoded, ignoring the package's own `sources` (the same reason the bootstrap must spell out the dependency list) — so modules outside `src/` are invisible to a consuming app. A tag carrying this layout therefore needs the app's own `sources` to add `.spago/bambik/<tag>/extras/**/*.purs`, which is why bootstrap.md's spago.dhall step carries that second glob; one glob covers all four roots, and it is tag-pinned, so it moves with `bambik.version`. The row layer's combinators are these optics at row granularity (`feedback` a `Colens`, `iterate` a `Coprism`, `folding` a `Coshutter`, `unfolding` a `Coreel`, `subResolving`/`subRetaining` a `Shutter`/`Reel`)
- **extras/row-profunctor/Data/Profunctor/** - `Seeding` (**pointedness as carrier structure**: `class Category p <= Seeding p` with `seeded :: a -> p a a`, the pointed wire — identity plus one registration emission of the seed; the primitive behind `with`/`announce` and the trace forms' seed arguments; deliberately no `(->)` instance — a timeless carrier has no registration moment), `Looping` (**self-reference as carrier structure**, `Seeding`'s sibling: `class Profunctor p <= Looping p` with the row-shaped `looped :: p { | r } { | r } -> p { | r } { | r }`, the `×`-diagonal self-trace no ecosystem class reaches — gated `unfirst` deadlocks on self-feed; laws are the trace axioms at the diagonal (yanking, dinaturality, idempotence); no `(->)` instance — feedback on a timeless carrier is `fix`, a computation; carries `mvu` and `bracketed` as its carrier-agnostic derivations) + the `Row/` layer; everything else was dissolved or deleted
- **extras/row-profunctor/Data/Profunctor/Row/** - Row profunctors over `Record`/`Variant`: four direction modules, each carrying its **direction class** — the binary merge plus its nullary unit `pempty`, the genuine per-carrier primitives — with qualified-do sugar (`bind`/`discard`). Everything kept is reached by a demo or a law test (L14); laws are stated in the module headers. Type variables follow the photographic schema: focus `f`, background `b`, shot `s` (`Cons l f b s`), reality `r`.
  - **`RecordToRecord.purs`** (×→×) — merge `recordToRecord` (`SharedRecordInputs` + `OwnedRecordOutputs`; gated on `PUI`, zero-field sides pre-satisfied — `{}` is always known);

    over ecosystem `Strong`: `subStrong` (sub-record focus, background carried), `field` (the type-changing field lens — the leaf lift: an editor lifted with it is a whole-row citizen, background retained and re-attached per emission, which is what dissolved `completed`), `required` (adopt a type-changing selector as an always-selected whole-row citizen, label derived from its closed rows: `select @l config options # required`; its dual `optional` is carrier-level and lives in `PUI`);

    over the **unit**: `announce` (its `rmap`-closure — the announcing constant) and `with` (`announce a >>> w` over `Semigroupoid` — discharge the initial-state obligation) with `mvu` (`with seed (looped w)` over `Looping` — the app shape, closed to `{}`), plus the subsuming `settled` (`rmap`-only normalization over a stated sub-row footprint);

    over bare `Profunctor`: `muted` (the counit — render and deliberately discard, `rmap (const {})`; the explicit word the gated content slots' `{}` demand points to); `forProperty` (read a field of a **wider** row into the display, the label derived from the leaf's row — the display-side `field @l` for context-pinned rows (collection items, pane payloads): `text @"label" # forProperty`; `projection`'s label-preserving retype is the narrow-row form);

    over ecosystem `Costrong`: the row form `feedback` of the **`Colens`** optic (`Data.Lens.Colens`) (the ×-trace at row granularity — a state sub-record loops from output to input; coerce-split like `subStrong`, so pipeline stages only; takes the traced chain's **initial state** `{ | iw }` as its first argument — the chain is an entity, fed its t=0 value at registration via a composed `seeded` wire, so the loop is primed before any input and never starves).
  - **`VariantToVariant.purs`** (+→+) — merge `variantToVariant` (`OwnedVariantInputs` — one handler per case — + `SharedVariantOutputs`); over ecosystem `Choice`: `focusCase` (the value-level case prism, via `prismE`), `subChoice` (**sub-variant focus**, `subStrong`'s transpose completing the wrap family's `+→+` corner: the wrapped profunctor handles the focus cases, background cases pass untouched — cashbox's money events detour through confirmation dialogs while its audit event flows straight to the fold) and `splitVariant` (the dispatch helper `VariantToRecord.subRetaining`, `iterate` and `subChoice` share); over `Looping`: `bracketed` (the variant-editor bracket, `dimap f g (looped w)` — an adopter with a `+→+` *result*, living at its result direction like `recordToCase` at `×→+`); over bare `Profunctor`: `atCase` (adopt a bare-input UI component as the owner of input case `l` — the closed-singleton unwrap at `+`, `atField`'s exact transpose); over ecosystem `Cochoice`: the row form `iterate` of the **`Coprism`** optic (the `+`-diagonal trace at row granularity — `again` cases loop back into the input, `done` cases exit; retry/wizard flows).
  - **`RecordToVariant.purs`** (×→+) — strength `Resolving`/`resolve :: p a b -> p (Tuple a c) (Either b c)` (a loop step: `Left` = Done, `Right` = Loop; `PUI`-only instances — the branch is derived **from time**: emissions loop while the UI component is still moving, the last resolves at quiescence, so `coresolve (resolve g) = debounced g` — literally: `debounced`'s body IS this retraction, the loop channel primed by a `seeded` wire as in `folding`; window parameterized via `resolveFor`) and its co-strength `Coresolving`/`coresolve` (ties the loop: a terminating fold) with its row form `folding @w` (case `w` continues the fold silently, `done` cases exit — the accumulating-wizard shape; takes the fold state's **initial value** `{ | fb }` as its first argument, emitted once as case `w` at registration via a composed `seeded` wire, so the fold never starves); merge `recordToVariant` (ungated broadcast); over `Resolving`: `subResolving`, `backgroundProperty`, and over `Coresolving` the row form `folding` of the **`Coshutter`** optic; plus `silence` (the unit's `dimap`-closure at any rows — the silent UI component, forced by parametricity), `armed` (the **emit stage** — the `×→+` member of the stage-subsumption family: feed an event ensemble the sub-row its emitters replay; dissolves the last call-site `widenRecordInput`), `recordToCase` (introduce an output case; plain `Profunctor`, the unit-pinned merge by law), `toCase` (`recordToCase` freed of the record-input constraint at the closed singleton row — introduce a **bare** output as case `l`, the output-side dual of `atCase`; dissolves the `rmap`-style payload lambda at collection sites: `listOf {} entries item # toCase @"picked" _.key`), `toCases` (adopt an emitter's case — derived from its singleton variant row — into the **variant of business outcomes** `f` computes: `button @"Sign up" {} # toCases register` emits `register`'s cases directly; row-typed on the outcome side, since a non-variant result is out of shape at `×→+`; the output dual of `VariantToRecord`'s `forCases`).
  - **`VariantToRecord.purs`** (+→×) — strength `Retaining`/`retain :: p a b -> p (Either a c) (Tuple b c)` (a Mealy/coroutine step; `PUI`-only instances — a stateless function can't retain state) and its co-strength `Coretaining`/`coretain` (ties the state channel: a productive unfold/generator) with its row form `unfolding @w` (value fields pass, state fields resume as case `w`; takes the unfold state's **initial value** `{ | fb }` as its first argument, fed once as case `w` at registration, so a gated `retain` inside the chain is primed before the first fresh input); merge `variantToRecord` (gated like ×→×, retaining the other side's last contribution); `forCases` (adopt a status for a **whole classified variant** — one copy classifier renders every case into the status's own payload case, derived from its row, so a single status instance serves mutually exclusive outcomes: `snackbar # forCases (match { booked: …, rejected: … })`; `toCases`' input dual and `forCase @l`'s plural); over `Retaining`: `subRetaining`, `focusCase`, `backgroundCase`, and over `Coretaining` the row form `unfolding` of the **`Coreel`** optic.
  - the collection lives one level up, in **`extras/row-profunctor/Data/Profunctor/Acting.purs`** (module `Data.Profunctor.Acting` — beside `Row/`, not under it: rows are the finitary μ-free fragment of the container grammar, `Array = μx. 1 + a×x` is one `μ` later). The class is the minimal carrier primitive `class Profunctor p <= Acting p where actedBy :: Ord k => (a -> k) -> p a b -> p (Array a) (Array b)`, keyed by the element row's **materialized identity field** (rows carry their identity; `Ord k` is the reconciler's Map-indexing requirement — identity semantics remain equality; keys must be unique and are never rendered). Laws in the header: **empty** (fed `[]` emits `[]`; nothing at registration), **singleton retraction** (yanking at the container), **gather gate** (`Array b` withheld until every element spoke, retain-last thereafter), **identity-follows-key** (stateful carriers). The module holds only the **pure algebra** — the class, `instance Acting (->)` (`actedBy _ = map`, so the laws are value-testable), and `optioned` (the `Maybe = 1 + a` action via the Array embedding). The carrier machinery lives with the carriers, exactly like the merge instances: **`PUI.purs`** carries `class Hosting m node | m -> node` (what a stateful carrier contributes — instantiate one element component at runtime, plus placement: detach a leaver, restack survivors), the placement-free `Hosting Effect` instance (the probe carrier the `spago test` laws run on), the shared keyed reconciler, the one **generic** `instance Hosting m node => Acting (PUI m)`, and the five vocabulary forms; **`PUI.Web`** carries `instance Hosting Web Node` (DOM placement — `appendChild` moves nodes, so identity follows the key). Design note: doc/collections-profunctor-algebra.md.

    The five divide the ground by **input** (everything at once → `×`; one entity at a time → `+`) and **output** (individual event; aggregate as joint decision; aggregate as running state). Key forms encode the key's ontology: a **label `@l`** on the `×`-members (identity is a materialized model field) and the **`{ key, value }` envelope** on the `+`-members (identity is the structural tag, arriving in the input, so no key function). Each `×`-member takes the **projection** that feeds it; the `+`-members take the projection producing the envelope.

    | Form | Shape | Behaviour |
    | --- | --- | --- |
    | `foreach @l` | `(i -> Array { \| a }) -> PUI m { \| a } o -> PUI m i o` | the collapsed/sum-flavored form: **keyed and retaining** reconciliation, matched elements re-fed in place, nodes moved with their keys; forwards each element emission onto the shared channel, ungated, silent on empty. Written trailing in a container ocular, `ul $ item # foreach @"id" rowsOf` |
    | `acted @l` | `PUI p { \| narrow } { \| rb } -> p (Array { \| a }) (Array { \| b })` | the **container action** (Tambara for `Array`): the element's output row **excludes the key**, re-attached from the input row via the `Strong` state channel, so identity is unforgeable. Output is the **gather gate** — withheld until every element has spoken, then whole on any re-choice |
    | `edited @l` | `PUI m { \| a } { \| r } -> PUI m (Array { \| a }) (Array { \| a })` | the **collection editor**: element emissions folded back in by key, whole array emitted **immediately**, input-primed (the retained fed array supplies unedited slots). The element's output row excludes the key — an element structurally cannot change its own identity |
    | `dispatched` | `(i -> { key :: k, value :: a }) -> PUI m a b -> PUI m i { key :: k, value :: b }` | `+→+`: an unknown key instantiates a new case, a known key re-feeds exactly its instance, emissions leave tagged — the targeted-update/stream direction, no whole-array re-feed |
    | `accumulated` | `(i -> { key :: k, value :: a }) -> PUI m a a -> PUI m i (Array a)` | `+→×`, the keyed Mealy: grows per new key in first-appearance order, updates known slots, emits the whole array immediately, input-primed — the board/ledger shape |

    The `+`-members never detach or restack: absence of a key is no signal, so removal and ordering stay array-level concerns upstream.
  - **`Row.purs`** (module `Data.Profunctor.Row`) — the shared floor: the row-constraint vocabulary (`InclusiveRows`/`ExclusiveRows` + the runtime-evidence duals `DispatchableVariants`/`MergeableRecords` with `exactRow`, bundled into the per-side classes `SharedRecordInputs`/`SharedVariantOutputs`/`OwnedVariantInputs`/`OwnedRecordOutputs` — sharing is inclusive, responsibility is exclusive, evidence only on owned sides — so each merge signature is two words, one per side; the owned sides also carry the `DisjointLabels` detector, which turns a duplicated label into a custom compile error naming the label) + the two `dimap`-only widening reshapings (`widenRecordInput`/`widenVariantOutput`) the `PUI` merge instances build on — `widenRecordInput` is **library plumbing, not vocabulary**: it is deliberately not re-exported from `PUI`, because record **subsumption is baked into the stages that read a row** (the gated displays/`updated`/`every`/`settled`/`armed`/`edited`/`acted`), so a demo states each business function's exact footprint as a closed row and never coerces at the call site. It is *not* baked into `foreach` (the element row is inferred from the array), `provided` (the pane is fed the payload) or `atCase` (a case payload is pinned by its consumer as often as by its emitter) — **rows are read narrow, payloads are exact**. The no-nominal-types-in-UI rule (every view-model type anonymous and structural) is stated for application code in `.claude/skills/developing-bambik-apps/writing.md`.
  - **`test/Main.purs`** — value-level `(->)` tests (`subStrong`/`field`/`recordToCase`) plus merge unit laws, gating, and the trace quartet and its row forms (`unfirst`/`unleft`/`coresolve`/`coretain`/`looped`/`iterate`/`feedback`/`folding`/`unfolding`) on the `PUI` carrier via a probe harness; **`test/HelloShutterReel.purs`**/**`BusinessOptics.purs`**/**`RestaurantReel.purs`**/**`EntityEventExample.purs`** — `Shutter`/`Reel` as business optics.

### Composition Patterns

- `Category.do` (`import QualifiedDo.Category as Category` in application code — bambik's complement of qualified-do, under `extras/qualified-do/`) - data-flow pipelines; the block's unit is the wire, `identity`
- `RecordToRecord.do` / `RecordToVariant.do` / `VariantToVariant.do` / `VariantToRecord.do` (qualified-do) - the four row merges
- label-indexed components - every MDC component is a citizen of one direction (`filledTextField @l` ×→×, `button @l` ×→+, `snackbar @l` +→×), so pipeline stages are written directly from components; `field`/`subStrong` nest sub-composites into larger aggregates
- variant editing - **record-shaped editor state**: the model keeps the variant, the editor keeps every payload; `bracketed <stateOf> <caseOf>` wraps `Category.do { selection component; payload panes }` (the variant in via a state function seeding absent payloads, out via a projection on the selection, self-traced in between) — each pane a whole-row editor stage `# inCase @l <selectionOf>` (the editor pane: existence gated on the selection's case, the rest of the row carried by the leaf's own `field @l` lift — no fold, no setter); consistency via the self-trace re-broadcast; unit-payload variants need only the bracket around one selection component
- conditional visibility - `provided <maybeOf>` with a named `Maybe`-valued projection (never an in-UI predicate), or `providedCase @l <variantOf>` for case-gated existence — a stored variant field (`# providedCase @"serving" identity # atField @"display"`) or, for **mutually exclusive derived states**, one variant-returning classifier per rule family (`# providedCase @"taken" usernameStatus`, signup-form: two classifiers replaced five `Maybe` projections, exclusivity by construction): conditional *data* (a pane whose content only exists sometimes) → `pane # provided <maybeOf>`; conditional *mode of a live editor* inside a `looped` ensemble → the editor pane `# inCase @l <classifier>` (order-form's fulfillment panes, flight-booker's return date, meeting-booker's attendees slider), never a payload pane folded back with an identity setter. `clWhen` stays predicate-driven — it toggles a class (styling), not visibility
- gated displays - live views as pipeline stages (slider readouts, summary lines, data tables): each rung renders per its policy and releases the fed row; read functions are closed at their own footprints (the rungs subsume)

### Separation of Concerns

- **Business Logic** - Row-shaped models, structural throughout, per the application code-style contract in **`.claude/skills/developing-bambik-apps/writing.md`** (see the note at the top of this file — it is the single normative statement, and demos are its executable form): no nominal types in UI, exact footprints, fold handlers in the Mealy step's own shape `payload -> state -> state`, no business literals in UI code, no pass-through fields, emissions carrying bare data. Below the UI sit plain functions and Aff actions (demo/nguis/order-form-mdc2/OrderFormMDC2.purs), nominal types only where recursion (cells' `Expr` AST — rows can't express μ) or an ecosystem API (`Aff`, `Either`, `Milliseconds`) demands them, and business optics (Shutter/Reel) where state/loop semantics are needed (test/BusinessOptics.purs)
- **Design System** - Oculars in the vocabulary modules (`PUI.Web.HTML`/`SVG`, MDC2, MDC3, Shoelace, Fluent, Bootstrap); the type `Ocular p = forall a b. Optic p a b a b` is declared in **src/PUI.purs** beside its sibling `Action` — the optic transpose (fix the carrier, quantify the data), with its admission law in the header
- **Composition** - UI elements compose orthogonally to the row combinators

## Demo Structure

102 pages over **40 app families**, registered in **scripts/demos.mjs** (the
single source of truth: directory + module + entry, shared by the bundler and
the dev server). Two suites: **demo/7guis/** (the
[7GUIs](https://eugenkiss.github.io/7guis/) benchmark) and **demo/nguis/**
(popular showcase apps, mostly one combinator each).

**Conventions every demo follows** — stated here once, not per demo:

- **View/logic module separation** (writing.md): the demo module is the view
  (design-system vocabulary + the logic module); a `<Demo>Logic` module holds
  the pure business functions and depends only on the domain. Twins share that
  logic module *verbatim* from the unsuffixed sibling directory
  (`demo/7guis/counter/CounterLogic.purs`, fetched by pages as
  `../counter/CounterLogic.purs`), so **a twin diff is view-only by
  construction**. Single-variant demos keep logic beside the app; helloworld
  (all view) has none.
- **Named module + entry, never `Main`** (`CounterMDC2`/`counterMDC2`), so
  every demo compiles under one `spago build`.
- **Vocabulary suffix = the design-system switch**: `-mdc2`/`-mdc3`/
  `-shoelace`/`-fluent`/`-bootstrap`/`-html` sibling directories over the same
  logic. demo/page.js probes the siblings and injects a switcher listing the
  ones that exist; unsuffixed pages get none. The per-variant diff is the
  honest catalog mapping (typography renames per the Material migration guide;
  vocabularies lacking `listOf` build selectable lists as a keyed `foreach` of
  `clicked` rows; those lacking an indeterminate progress run Aff stages as
  `pempty # action …`).
- **The app shape** is `pipeline # mvu seed` (or `# with seed`), closed to
  `PUI Web {} model`.
- **Naming**: MDC2/MDC3 name the component vocabularies, modules, directories
  and UI labels; plain MD2/MD3 is reserved for the design-system specs
  (m2/m3.material.io) in prose.

### 7GUIs — all seven in all six vocabularies

| Demo | What it shows beyond the benchmark task |
| --- | --- |
| counter | the floor: one editor, one emitter, one fold |
| temperature-converter | both fields in the model; non-numeric input leaves the other untouched |
| flight-booker | type-changing `select` over an anonymous variant row `# required`; both outcomes carry bare payloads into **one** `snackbar # forCases bookingLine` |
| timer | `every` heartbeat; `sliderLive` duration re-scoped at runtime |
| crud | `MDC2.listOf` (keyed `foreach` of `clicked` rows elsewhere); Aff catalogue actions |
| circle-drawer | **channel-fed SVG canvas** — built once, updated via `attrWith`; container-level `onClickedXY`; the diameter a bounded quantity in the model, its slider `# inCase @"chosen" selection # settled resizeSelected` — live-preview resize as a state invariant, an `adjusting` flag coalescing a drag into one undo transaction |
| cells | **channel-fed 31×27 grid** — ~800 cells built once, `attrWith` + `text` in place, clicked key via `# toCase @l _.key`; hand-rolled formula evaluator over an `Expr` AST (nominal, since rows can't express μ) |

The `-html` variants are the **plain-HTML floor**: one container `div` (so
`provided` panes re-attach inside the demo's own DOM), scalar leaves labelled
in place (`input "text" # field @"Name"`), captions as `label`+`staticText`
merges, native `select` and `output`.

### nGUIs — one combinator each

**Flagship.** order-form is the **four-direction showcase**: load action →
`×→×` `looped` form (whole-row editor stages, sub-records nested via `field @l`;
variant editors as `bracketed` pipelines of `tabBar`/`segmentedButton` +
`inCase` editor panes; an **in-form Aff action** — the delivery distance is
estimated on a button, `button @"Estimate distance" {}` →
`action estimateDistance # atCase` → `updated`; the estimate records the
address it was made for and `settled staleDistanceForgotten` keeps that
invariant, so an address edit drops it — the effect runs on an occurrence,
never on the loop's broadcast, and `settled` normalizes, never reacts) → gated live summary
→ `×→+` event buttons `# armed` → `+→+` backend dispatch → `+→×` status
snackbars.

**One combinator each** (the trace quartet's row forms and the focus pair get a
focused demo apiece):

| Demo | Combinator / point |
| --- | --- |
| auction | `feedback` (`Costrong`) — a `top`-bid field loops output→input, seeded, invisible in the stage's outer type |
| checkout | `folding @"next"` (`Coresolving`) — 3-step wizard, step state loops silently; Next/Back are **two business actions** each carrying its own caption-case into the loop case via `# toCases`, so no button needs a `label:` |
| payment | `iterate` (`Cochoice`) — flaky charge retries with attempt+1; no seed (events occur, they don't pre-exist). Also the **`observed`** showcase: a retry toast narrates the loop inline while the event passes on |
| ticket-dispenser | `unfolding @"resume"` + the `Reel` optic (`Coretaining`) — "take a number", counter seeded and resumed. Also the **`providedCase`** showcase: state is a payload-carrying variant field, so panes are pure case adoption |
| parcel | `subStrong` — a reusable address sub-form as a citizen over its own closed row, background field threaded |
| cashbox | `subChoice` — selective interception as UX: outgoing money detours through confirmation dialogs, incoming posts straight to the fold; every branch a two-record Mealy handler `{ amount } -> { balance } -> { balance }`; payloads via `button @l {…} # with patch` |
| potluck | `acted` (the container action) — per-guest dish editors under one model; the menu summary is **withheld by the gather gate until every guest has chosen**, and shows `foreach` beside `acted` |
| departures | `dispatched` (+→+ keyed input) — rows appear on first mention, re-feed in place, tagged output drives a last-update line |
| scoreboard | `accumulated` (+→× keyed input) — board grows to its key set, points update in place, whole array drives the standings |
| reorder | keyed reconciliation + the `edited` collection editor — a playlist keyed by track id, element output row excluding the key (the carrier re-attaches it); Rotate and effectful Shuffle move each row's DOM node with its track, so tick, title and focus follow |
| order-dashboard | **custom components** (MDC3-only): the demo ships its own `DashboardControlsMDC3` module — five controls + a `board` ocular, each a citizen with canonical rows, including the packaged-collection-display protocol (`leaderboard`) |

**The rest**, grouped by what they exercise: todomvc (`listOf` toggle, `clWhen`,
`segmentedButton` filter), tip-calculator (all-`×→×`, sliders, gated money
readouts), quiz (`provided` panes over `Maybe`-projected stages,
`linearProgress`), tic-tac-toe / calculator (**channel-fed `foreach` grids** —
cells built once, key emitted via `clicked` + `toCase`, folded by `updated`),
markdown-previewer (`filledTextArea` + injection-proof preview as recursive
`PUI Web`: `(dynamic …) # shown` over element oculars, since structure
genuinely varies per block), stopwatch (`every` tick pausing via `Nothing` over
a stored phase variant — a Boolean nobody edits as a Boolean is a phase — with
`# providedCase` button panes and a `shownEach` lap list — whose
per-feed release *is* the sequence merge's announcing unit, so an empty
lap list never starves the gate),
shopping-cart (`dataTable` over `foreach`), password-generator (effectful
`action` + `Effect.Random` → `updated`), color-mixer (`sliderLive` channels
driving an `attrWith` swatch), signup-form (`debouncedTextField` username check plus two
variant-returning classifiers via `providedCase`, replacing five `Maybe`
projections — exclusivity by construction), photo-gallery (`imagePane`, the
channel-fed gallery: a retaining `foreach` over the pictures rather than a
wholesale rebuild), inbox (`listOf` + `dialog` + `banner` — the demo whose
MDC3 twin shows the honest catalog gap, MD3 having dropped `banner` for
`snackbar`), movie-browser (Aff search `action` + `providedCase` result panes),
weather (Aff service with a canned per-city delay), helloworld
(`body $ staticText` — the 5 kB bundle floor).

**Vocabulary showcases.** restaurant-menu is the plain-HTML one (no design
system: element oculars, `cl`/`:=` decorators, `each` from data, seeded
`with {}`; the fine-dining look is ordinary CSS). espresso-bar is the MDC3 one
(with an MDC2 twin generated in reverse). One per non-Material vocabulary,
suffix naming the vocabulary rather than a twin (so a suffix means
"this vocabulary", not "has a twin" — order-dashboard-mdc3 is single-variant
too, while only helloworld and restaurant-menu, which use no design system at
all, carry no suffix): product-review (Shoelace's
exclusive star `rating`), meeting-booker (Fluent; also the **no-defaults
showcase** — nothing pre-picked, `# optional` selectors over `Maybe` fields, the
attendees a bounded quantity *in the model*: the slider exists only once a
room is chosen (`# inCase @"chosen" roomChoice`) and the room dropdown
re-scopes its bounds as an invariant (`# settled seatsInRoom`), so an
incomplete meeting is unbookable by construction), loan-calculator (Bootstrap, all
`sliderLive`).

Verify with `npm run smoke` (scripts/smoke/, headless-Chrome CDP; per-demo
tests under scripts/smoke/tests/).

## Key Dependencies

- `profunctor-lenses` - Profunctor-based optics
- `qualified-do` - Syntax sugar for profunctor composition
- `material-components-web` - MDC (Material Design 2) JavaScript library
- `@material/web` - Material Design 3 web components (custom elements, used by `PUI.Web.MDC3`)
- `@shoelace-style/shoelace` - Shoelace/Web Awesome web components (used by `PUI.Web.Shoelace`; pages link its light theme CSS from the matching CDN release)
- `@fluentui/web-components` - Fluent UI v3 web components (used by `PUI.Web.Fluent`; theme tokens ship in the bundle)
- Bootstrap is CSS-only and **not** an npm dependency — `PUI.Web.Bootstrap` is native elements + classes, pages link the Bootstrap 5 stylesheet from the CDN
