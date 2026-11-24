# Bambik

## Overview

A prototype PureScript library implementing **Profunctor User Interfaces**: profunctors unify optics (data structure navigation) and arrows (data flow), which makes them the right algebra for composable UI.

**Why.** A web application is mostly a form over a data model: values flow from the model to the screen, the user edits and clicks, values flow back. The Elm/React family cuts that round trip in half — a view function, a message stream, an update function — so *this text field edits the customer's name* is smeared across four places and a virtual DOM reconstructs at runtime a correspondence the programmer could not write down. Functional programming already has the remedy for each half, optics and profunctors, and they are one remedy: *a lens is exactly what turns a `p field field` into a `p record record`, for any profunctor `p`*. So if a UI component is a profunctor, the lens that focuses the customer's name is the value that mounts its text field into the customer form. That is the bet; the rest of the library is what follows from refusing anything that does not come from the algebra: no virtual DOM, no global store, no message plumbing, an application's architecture readable off its types. *The algebra in brief* below summarises it; doc/presentation-20min.md is the talk.

**Where things are stated**, each once:

- **doc/guardrails.md** is normative: the MUST/MUST-NOT rules for the library and the admission gates every proposed combinator, class, component or demo idiom passes (derivation → laws → subsumption → honesty → reachability → green stack → sync). A change that violates one is wrong even if it works. Two rules govern every change: **L15**, the demos are the compatibility contract (`spago build`, `spago test`, `npm run bundle-demos`, `npm run smoke`, in full, a demo edited only when it is the subject), and **L16**, the import tower (only `PUI` and `Data.Profunctor.*` import the ecosystem's `Data.Profunctor`; `grep "import Data.Profunctor (" demo/` stays empty).
- **`.claude/skills/developing-bambik-apps/writing.md`** is the code-style contract for application code, demos included; read it before writing or reviewing any demo or app module. Its deployed restatements are the `#code-style` and `#tracing` notes in demo/index.html and demo/workflow.html (*Writing order* replayed as a session with verbatim compiler output); re-read them when the contract changes.
- **The module headers** are the contract of every word: what it does, its signature, its laws. `npm run api-docs` renders them into `generated-docs/md/` (gitignored).
- **The git log** holds the history — what was added, tried, withdrawn and when — in the commit messages, and up to 2026-11-19 in the deleted CHANGELOG.md (`git show 14be75ce:CHANGELOG.md`). A dated obituary belongs in a commit message, never in this file, a header or the guardrails, which cite a date only where a precedent matters.

## Building

`npm install`, then `export PATH=$PWD/node_modules/.bin:$PATH` and `spago build` (tests: `spago test`).

**The forked compiler.** The repo builds with the forked PureScript compiler pinned in `package.json` (variant row sugar `[ l :: T | r ]`, `.label` constructors — doc/variant-sugar.md), installed from a GitHub release (`erykciepiela/purescript` tag `v0.15.16-variant.9`, branch `variant-type-sugar`; package-lock.json pins the tarball's hash), with the `Prim.Variant`-patched variant library as a git package in packages.dhall (`erykciepiela/purescript-variant` tag `v8.0.0-prim-variant.1`). A plain clone builds on any Linux x86_64 machine. `node_modules/.bin/purs --version` must report `0.15.16 [development build ...]`; stock `0.15.15` fails with "Module Prim.Variant was not found".

**How applications consume it.** As a spago git package pinned to a release tag, cloned under `.spago/bambik/<tag>/` with demos and docs. Spago does not read a git package's own spago.dhall, so the app spells out the library's dependency list (fetched from the tag by the bootstrap procedure), and it globs only `src/**/*.purs`, so the app's `sources` adds `.spago/bambik/<tag>/extras/**/*.purs`. The procedure is the skill's bootstrap.md, beside SKILL.md, writing.md and building.md.

### Development loop

Keep `spago build -w` running in the background and read its output after each edit (~0.7 s incremental); it covers the library, the tests and the demos. spago -w dies on stdin EOF, so keep stdin open (never `</dev/null`), and run one watcher over the shared `output/` at a time; parallel agents serialize compiles (`flock <lockfile> spago build`) or use git worktrees.

`npm run dev` serves every demo at `http://127.0.0.1:1234/`, rooted at `demo/` in the deploy's own layout (local `/nguis/inbox/?mdc3` is `/bambik/demo/nguis/inbox/?mdc3` on the host). Narrow by name, app or set: `npm run dev counter-mdc2 cells-mdc2`, `npm run dev inbox`, `npm run dev nguis`. scripts/dev.mjs rebuilds and reloads on every edit (~2 s), polling mtimes rather than `fs.watch` (inotify runs out of instances; `recursive: true` delivers nothing on ext4).

**Part tests.** A view split into parts (`Diner.View.Hall` in `View/Hall.purs`) has its tests in the app's `test/` at the part's path (`test/View/Hall.test.mjs`): a list of `{ name, seed, run }`, `seed` naming a value of the app's `<App>.Fixtures` or view model, `run` driving the part with real clicks, typing and picks. `npm run part -- diner` (or `-- diner Hall`) mounts each part alone, `body $ part # looped # with seed`, compiled into .part/ against a copy of output/, and runs every test on a fresh headless-Chrome page, failing on an assertion, a missing control, an exception or a console warning, with a screenshot per failure (about 100 s for all of diner's). A test may set the time its page opens at (`time: '12:20'`, for a part whose lines read the time against a seed's moments) and its viewport (`width:`, `resize(width)`): diner's `test/fits.mjs` holds every station to 360, 768 and 1280 px with no sideways scroll. The view's own tests (`test/View.test.mjs` beside `View.purs`) name no seed: they run the application as it starts, load included, for what crosses stations. The header of scripts/part-test.mjs is its manual.

In the browser, `window.__bambikTrace = true` (or `localStorage.setItem("bambik-trace", "true")`) logs every propagation decision, and every knowledge gate carries a starvation watchdog: a gate still withholding after 3 s prints one `console.warn` naming the missing fields and the fix. The diagnostics are off until a carrier adopts them (`PUI.Web.adoptHostDiagnostics`, at `body`), so `spago test` is silent; `window.__bambikNoWarn = true` opts out.

### Verification

| Command | What it checks |
| --- | --- |
| `spago test` | test/Main.purs: the value-level laws of every combinator on `PUI Effect` probes; test/Exhaustive.purs: the merge and container-action laws over every event script to length 6 or 8, complete by data independence (about 150 s); the business optics (`Shutter`/`Reel`) in their own modules |
| `npm run smoke` | scripts/smoke/: a headless-Chrome CDP harness for only what the laws cannot reach — the leaf-law bench over demo/laws/, the mount check (every registered demo mounts with no exception or warning past the watchdog), the container action's identity in the real DOM, the axe-core stamp audit, and one file per carrier-only behaviour in scripts/smoke/tests/. Bundle first; `npm run smoke -- <name>` filters |
| `npm run check-view-model` | no `Maybe` and no `Boolean` off the Boolean-editor allow-list in any demo's view model, no accessor section `_.l` in any demo view |
| `npm run check-determined` | guardrail L18 made mechanical: every demo's view compiled with a typed hole for every imported value, each exported signature checked to be its hint with the tails filled; `node scripts/check-determined.mjs <demo> …` names demos, `--write` writes the signatures (about a minute for all) |
| `npm run bench` | scripts/bench/: the library measured on Diner (doc/benchmark-plan.md) — the view model's reads in node, real input per station at full and quarter speed with a CPU profile attributed to layers, days of 1× to 64× the standard load, a seeded random walk checked step by step against the view model run in node, races, a soak sampled for leaks, synthetic applications of 6 to 48 stations. Not part of the L15 gate: about half an hour, and its numbers are this machine's. `-- quick`, `-- <scenario> …`, `-- compare`; results in bench/results/, the committed run in bench/baseline.json |

## Building & Deploying Demos

1. Check the compiler (`purs --version`, as above).
2. Bundle: `npm run bundle-demos` (minified, all demos; `node scripts/bundle.mjs <name|set>` for a subset). Every demo is a named module entered at its own function (`Counter.MDC2`/`counter`; no module is `Main`), and **scripts/demos.mjs** is the single registry of directory, module and entry shared by the bundler, the dev server, check-determined and the smoke tests; it synthesizes each esbuild entry because `spago bundle-app` can only call `Main.main`. Bundling stamps every page's `page.js?v=` with a hash of demo/page.js, since the host sends no cache headers.
3. Deploy: `npm run deploy-demos` scps demo/index.html, demo/workflow.html, demo/page.js, both suite directories and demo/diner/ to host `xyz` (root@erykciepiela.xyz, see `~/.ssh/config`) at `/var/www/html/bambik/demo/`.
4. Verify: `http://erykciepiela.xyz/bambik/demo/<d>/` returns 200 (plain HTTP). The deploy deletes nothing, so a directory removed or renamed here lingers there until removed by hand.

**Diner** is an application beside the suites, **demo/diner/** (registry set `apps`, named `diner`), and the benchmark application (doc/benchmark-plan.md): it opens on a day loaded from its stand-in server, `Diner.Service` — 304 dishes in five courses, 42 tables, about 250 orders over the last 24 hours, generated from a seed as of the moment it is asked for — the lines that show time passed follow the wall clock themselves (`timestamped`), and every list is paged. Its index.html is a gitignored build product written from the registry (`appPage` in scripts/demos.mjs); `npm run deploy-demos` publishes it at `/bambik/demo/diner/`, and `npm run deploy-diner` copies index.html and bundle.js to `/var/www/html/diner/` on `xyz` (`http://erykciepiela.xyz/diner/`); bundle first.

**Demo pages** are pure structure over the shared chrome in **demo/page.js**: one `<template data-variant="mdc3" data-source="MDC3.purs Inbox.purs">` per design system, holding its stylesheets; page.js installs the one the query names (the first when none), loads `bundle-<variant>.js` once the stylesheets have loaded, shows each listing verbatim, and groups the running demo with its tracing note into `#demo-column` — on a page marked `<body data-surface>`, inside the vocabulary's card surface.

## Cutting a release

A release is a tag plus a release page, in four steps:

1. Bump the tag in the dependency table of `.claude/skills/developing-bambik-apps/bootstrap.md` **before** tagging, so the tagged tree documents its own tag. Nothing else in the scaffold carries a version.
2. Verify green in full — `spago build`, `spago test`, `npm run bundle-demos`, `npm run smoke` (L15) — then tag the verified commit (`git tag -a v0.2.0`) and push tag and branch.
3. Create the release page from the tag with the **GitHub REST API** (`gh` is not installed; `GITHUB_TOKEN` is in the environment). The body is minimal by decision: the skill-usage prompt naming this tag's asset URL, nothing else — the pins and the packages.dhall entry live in bootstrap.md, which ships attached. Write the JSON with a script so the body's newlines survive quoting:

```sh
node -e 'require("fs").writeFileSync("body.json", JSON.stringify({
  tag_name: "v0.2.0", name: "bambik v0.2.0", draft: false, prerelease: false,
  body: "…the skill-usage prompt, naming the asset URL for this tag…" }))'
curl -s -X POST -H "Authorization: Bearer $GITHUB_TOKEN" \
  -H "Accept: application/vnd.github+json" \
  https://api.github.com/repos/restaumatic/bambik/releases -d @body.json
```

   The response's `upload_url` (strip its `{?name,label}` suffix) is where step 4 posts. A `message` field in the response means it failed; curl exits 0 on API errors.
4. Attach the authoring skill as an asset, built **from the tagged tree** so it cannot drift:

```sh
mkdir -p /tmp/skillpack && cd /tmp/skillpack
git -C <repo> archive v0.2.0 .claude/skills/developing-bambik-apps | tar x --strip-components=2
tar czf developing-bambik-apps-v0.2.0.tar.gz developing-bambik-apps
curl -s -X POST -H "Authorization: Bearer $GITHUB_TOKEN" \
  -H "Content-Type: application/gzip" \
  --data-binary @developing-bambik-apps-v0.2.0.tar.gz \
  "<upload_url>?name=developing-bambik-apps-v0.2.0.tar.gz"
```

   Then verify: re-fetch `releases/tags/v0.2.0` (asset `state` must be `uploaded`, `draft` false) and run README's install one-liner into a throwaway directory. README's install command and release link need the new tag too. The asset is a snapshot, re-attached per tag, bought for a portable one-command install (`tar xz -C .claude/skills`, no GNU-only flags).

## Architecture

### Core Type

```purescript
newtype PUI m i o = PUI (m (Registration i o))
type Registration i o = (o -> Effect Unit) -> Effect (i -> Effect Unit)
```

A `PUI m i o` is a **wire with a face**: `i` the values pushed at it (the model to show), `o` the values it emits (what the user changed or did). Registering it hands over the callback for its `o` and returns the leg that pushes `i` at it, so nothing is pushed before it is listened to, and whatever the component remembers is allocated there, once per use. `m` builds its face (for the DOM, `Web = StateT DOM Effect`). Everything is structure *on* this type, not machinery beside it.

**The four shapes are the four arrows of an app's cycle** (guardrails L3), and the rows between them are named by it — use these names for these positions and nothing else:

| Shape | From → to | The arrow | Citizens |
| --- | --- | --- | --- |
| `×→×` | **model** → **view model** | presentation: the state as the screen shows it, edits carried back | displays, editors, `group @l`, `actedOf` |
| `×→+` | **view model** → **view event** | interaction: the state shown, an occurrence emitted | emitters, emitter panes, emitting lists, `ticks` |
| `+→+` | **view event** → **model event** | interpretation: what happened on screen turned into what it means | `confirmed`, `modal` under `acknowledged`/`continued`, `action`, `joined` |
| `+→×` | **model event** → **model** | transition: the event folded into the next state | `fold`/`foldInto`/`reset`/`capture`, statuses |

The names are positions, not types: the model and the view model are one row wherever nothing narrows or derives. The **view model module** holds the functions of all four arrows.

### The algebra in brief

The vocabulary falls out of the core type taken seriously, operator by operator; the full statement is `Data.Profunctor.Row`'s header and PUI's.

- **Shapes — the compass.** Models come as a **record** (`×`, all of these at once) or a **variant** (`+`, one of these at a time), so a component has four possible shapes, one module each under `Data.Profunctor.Row`, and every component of a vocabulary is a citizen of exactly one. An application is one walk around the compass: model → form (`×→×`) → buttons (`×→+`) → backend, dialogs (`+→+`) → folds, statuses (`+→×`) → model.
- **Binary operators — the merges.** Each shape has one merge putting two components side by side, and its pattern is a law: **sharing is open, responsibility is exclusive**, the input deciding which side is which. Under a record input every operand reads the whole row and owns its part of the output, like the entries of a record literal (fields at `×→×` — the knowledge-gated merge, a multi-field feed released once, never torn; cases at `×→+` — a block listing exactly the events that can occur). Under a variant input every operand owns its cases and all return one row, like the arms of a case match (`+→+`; `+→×`, whose shared output for an application's folds is the step to the next model). These are the product's pairing and the sum's copairing, polarity swapped across input and output — why there are four shapes and not two with a flip. Owned sides check disjointness at compile time (`DisjointLabels`) and carry runtime evidence only where the action is label-driven. Editors and displays are not merge operands: each reads the row and passes it on, a **whole-row citizen** written as one more line of `Semigroupoid.do`, so code order is DOM order.
- **Nullary operators — the units.** The merges carry no unit of their own; they borrow `Category`'s `identity`: the wire into `{}` at `×→×`, `Variant () → Variant ()` at `+→+`, that wire entered from the empty variant at `+→×`. Chrome is the wire too, which licenses decoration anywhere. A unit must not be able to say anything, so the record gates treat a contribution of zero fields as no contribution. What does speak at registration is the **point**, `announce a`, which `with` uses to seed an app. The one merge no wire reaches is `×→+` — nothing maps `{}` into `Variant ()` — so its unit is the carrier's silent component.
- **Unary operators — strengths.** `Strong` gives `focusField @l`, the field lens behind every label-indexed editor and `group @l`; `Choice` gives `focusCase @l`, the dialog's lift. The mixed shapes need strengths coined here: **`Resolving`**, `p a b -> p (Tuple a c) (Either b c)`, one step of a loop answering with a decision, derived on `PUI` **from time** (a burst loops while the component moves and resolves at quiescence, with no "transient" flag on the wire); and **`Retaining`**, `p a b -> p (Either a c) (Tuple b c)`, a Mealy step receiving one case at a time and remembering the rest, which `(->)` cannot have. They underlie the `Shutter` and `Reel` optics.
- **Co-strengths — tying the knot.** Every co-strength is its strength's retraction once the channel is primed, the **trace quartet**:

  | strength | co-strength | ties into | co-optic | in the vocabulary |
  | --- | --- | --- | --- | --- |
  | `Strong` | `Costrong` / `unfirst` | state feedback | `Colens` | `looped`, the carrier's own word (a gated `unfirst` cannot self-feed) |
  | `Choice` | `Cochoice` / `unleft` | iteration | `Coprism` | none: holds raw; an event re-enters through its fold |
  | `Resolving` | `Coresolving` / `coresolve` | terminating fold | `Coshutter` | `debounced`, its one seeded use |
  | `Retaining` | `Coretaining` / `coretain` | productive unfold | `Coreel` | none: a running count is a model field |

  Each co-optic is its optic run backwards and hides its residual in the loop, which is why the UI vocabulary uses none: state an application needs is a model field, and the co-optics live on as business optics below the UI. The retraction pays a dividend — `coresolve (resolve g >>> seeded (Right c0)) ≈ debounced g`: **debouncing is a theorem**, and literally how `debounced` is written. The `×`-side loops are dead unprimed, so `PUI` is genuinely traced over `+` and only pointed-traced over `×`: the knot, `looped`, is a carrier primitive, and an application has exactly one, around every line that changes the model, so the model re-enters whole at one place.
- **Closure — collections.** A list is not a fifth shape: `Array = μx. 1 + a×x` is the same two row forms closed under one `μ`, so collections are the **keyed square**, one class per shape, each the keyed analogue of a strength — `Acting` (`×→×`), `Gathering` (`×→+`), `Dispatching` (`+→+`), `Accumulating` (`+→×`). Identity is a field of the row, never a render index, so matched elements are re-fed in place and their nodes move with their keys: reconciliation without a virtual DOM.
- **The spine — `Category`.** `ui1 >>> ui2` pipes one component's output into the next's input; a `Semigroupoid.do` pipeline reads top to bottom like the user's journey, and `looped` closes it into a circle. `identity` is the **echo wire**: given a seed it is `seeded`, given a face a display, given nothing chrome.
- **What the laws buy.** Six laws, stated once in `Data.Profunctor.Row` and read at each shape — two on components, four on merges (symmetry and associativity, projection, preservation, monotonicity). On `PUI` the record gate is a pure Mealy step and the merge laws are checked over every script to a bound that makes the check complete (test/Exhaustive.purs); what they buy an application is writing.md's *What the laws guarantee*.

### Source map

- **src/PUI.purs** — the carrier `PUI` and its instances, the knot `looped`, the point `announce`, every host-neutral word of the application vocabulary (`with`, `stepped`, `landed`, `settled`, the folds, `action`/`load`, `joined`, the panes, the trailing words `introduced`/`supplied`/`continued`/`acknowledged`, `ticks`, `source`, `editsOf`), the keyed square's `PUI` instances, and the **plumbing** the vocabularies build on (the scope, `Hosting`/`Interacting`, the four leaf wirings, `eventLeaf`, `statusOver`, `confirmedOver`/`modalOver`, the diagnostics switches). Its export list keeps the two apart; no demo writes a plumbing word. The algebra carries no JavaScript.
- **src/PUI/Gate.purs** — the knowledge gate as one pure Mealy machine (`gateStep`), shared by the `×→×` merge and the container action's gather; data-independent with finite control, which makes test/Exhaustive.purs complete.
- **src/PUI/Web.purs** (+ Web.js) — the DOM carrier (`Web = StateT DOM Effect`, `Hosting Web`) and the element-neutral web vocabulary: decorators (`attr`/`:=`, `attrWith`/`attrAt`, `cl`, `clWhen`), text leaves, the canvas sources, `separating`, the facts of the host beside the model (lines that read one — `timestamped`, `timed`, `geolocated`, `online`, `colorScheme`, `viewport`, … — and emitters whose event carries one — `timestamping`, `timing`, `geolocating` — over `PUI`'s `reading`/`stamping`), `dynamic`/`each`/`el`, and the helpers every vocabulary builds its leaves from (`fieldLeaf`, `chrome`, `grouping`/`member`, `ensureStyle`). No element lives here.
- **src/PUI/Web/HTML.purs**, **SVG.purs** — the plain-HTML floor (element oculars, native controls, `body`, `filePicked`) and the SVG oculars.
- **src/PUI/Web/{MDC2,MDC3,Shoelace,Fluent,Bootstrap}.purs** (+ `.js`) — the design-system vocabularies, below.
- **extras/** — the algebra the carrier stands on, outside `src/` because claiming an ecosystem module name is a claim about what the module is; one glob `extras/**/*.purs`. **row-profunctor** (`Data.Profunctor.Row` and the four shape modules: merges with their qualified-do sugar, the field lens and case prism, the laws), **keyed-profunctor** (`Data.Profunctor.Keyed`: the keyed square), **structural** (structural `Eq`/`Ord` of plain values), **variant** (`Data.Variant.Case.caseText`), **profunctor**/**lenses** (the coined strengths and co-optics, law-tested, reached by no demo).
- **test/** — Main.purs (value-level laws), Exhaustive.purs (the bounded exhaustive check), the business-optics examples.

### The design-system vocabularies

Six vocabularies — the HTML floor and five catalogues — interchangeable by guardrail L20: the same concept has the same name and signature in each, a catalogue's renames, exclusives and gaps appear under its own names, and every vocabulary is entered through its own `body`, which does that catalogue's page setup at mount. Every component is label-indexed: its label is the model field or case, its caption verbatim, and the accessible name stamped on its host element. Selectors are editors over option leaves in three words: plain, `…Unpicked` and `…Optional`.

| Module | Basis | Worth knowing |
| --- | --- | --- |
| `PUI.Web.MDC2` | `material-components-web`: documented markup plus a foundation instance per component | the fullest catalogue (`listOf`, data table, image list, drawer, `banner`, menus, chips); text fields write through the foundation's `value` so the label float stays foundation-managed |
| `PUI.Web.MDC3` | `@material/web` custom elements | mirrors MDC2; MD3's renames and no `banner`; entries the catalogue lacks (segmented button, snackbar, card, top app bar, drawer, data table, image list, tooltip) hand-rolled over `--md-sys-*` tokens, each injecting its stylesheet once (`ensureStyle`); `tile`/`doubleClickTile` for grids |
| `PUI.Web.Shoelace` | `@shoelace-style/shoelace` custom elements (Lit) | exclusive star `rating`; its own names where the concept differs (`textField`, `toast`, `progressBar`); no typography; `body` sets the icon base path |
| `PUI.Web.Fluent` | `@fluentui/web-components` v3, theme set by `body` | exclusives `ratingDisplay` and `messageBar`. FAST binds a beat after insertion and its update queue is rAF-driven (starving in frameless headless sessions), so the dropdown and radio group defer writes on a timer poll (`whenBoundDo` in Fluent.js); the dropdown's options must sit in a `<fluent-listbox>` |
| `PUI.Web.Bootstrap` | CSS only: native elements in documented classes, the stylesheet linked from the CDN | the toast's `autoDismiss` timer is its only FFI; `sliderLive` only; no dialog |

### Composition

- `Semigroupoid.do` (`import QualifiedDo.Semigroupoid as Semigroupoid`) composes stages; every line passes the row it is fed, the event path included (`( emitters; dialogs; folds ) # stepped`).
- `RecordToRecord.do`, `RecordToVariant.do`, `VariantToVariant.do`, `VariantToRecord.do` are the four merges. **A line passes, an operand owns**: sources in `RecordToRecord.do`, emitters in `RecordToVariant.do`, options in `VariantToVariant.do`, folds and statuses in `VariantToRecord.do`. A label written twice in a block, a fold for a case nothing emits and an emitted case with no fold are compile errors.
- An application is one knot: `body $ Semigroupoid.do { lines } # looped # with seed`. The rest of the app shape is writing.md's *App shape*.

## Demo Structure

105 demos on 46 pages and one application, registered in **scripts/demos.mjs**. Two suites: **demo/7guis/** (the [7GUIs](https://eugenkiss.github.io/7guis/) benchmark, compared in doc/7guis-comparison.md) and **demo/nguis/** (showcase apps, mostly one idea each). README's reading order — counter, temperature-converter, flight-booker, todo-list, checkout, order-form — is the path through them. demo/laws/ holds the leaf-law benches, one page per vocabulary. **demo/diner/** is an application: a plain page, its view `Diner.View` (the load, then the knot: the station tabs, the stations) split into station parts under `View/`, its own components in `Diner.Design`, its stand-in server in `Diner.Service`, its part tests under `test/`.

Conventions:

- **One app, one directory, every design system it has the leaves for.** The view module per design system is named by it (`MDC2.purs` is `Counter.MDC2`), entered at the app's name (`counter`), never `Main`; the shared view model module is the app's (`Counter.purs`, module `Counter`), so a twin diff is view-only by construction. An app with one design system for good may name its view `View` (diner, registered `'diner': { mdc3: 'View' }`). One page switches between twins (`counter/?mdc3`). A twin exists only where its vocabulary has the leaves the app needs — no twin builds a leaf from a bare element. A further module is named the same way (`OrderDashboard.ControlsMDC3`); which module is the view model is the registry's to say.
- **The view determines its view model** (guardrails L18), checked by `npm run check-determined`.
- **Naming**: MDC2/MDC3 name the vocabularies, modules, directories and UI labels; plain MD2/MD3 is reserved for the specs.

## Key Dependencies

- `profunctor-lenses` — profunctor optics
- `foreign` — reads a plain value's structure for the structural `Eq`/`Ord` (`Data.Structural`)
- `qualified-do` — the `do` sugar for composition and the merges
- `material-components-web` (MDC2), `@material/web` (MDC3), `@shoelace-style/shoelace` (Shoelace; pages link its light theme from the matching CDN release), `@fluentui/web-components` (Fluent; theme tokens ship in the bundle)
- Bootstrap is CSS-only and not an npm dependency: pages link the Bootstrap 5 stylesheet from the CDN
