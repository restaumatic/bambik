# Bambik

## Overview

A prototype PureScript library implementing **Profunctor User Interfaces** - a novel approach to declarative UI development. The key insight is that profunctors unify optics (data structure navigation) and arrows (data flow), making them ideal for composable UI development.

**Why at all.** A web application is mostly a form over a data model: values flow from the model to the screen, the user edits, clicks and chooses, and values flow back. The Elm/React family cuts that round trip in half — rendering is a function from state to view, interaction a separate stream of messages, an update function reconciles them — so *this text field edits the customer's name* is smeared across a view, a message constructor, an update branch and often a lens, and a virtual DOM diff reconstructs at runtime the correspondence the programmer had in mind but no way to write down. Functional programming already has the remedy for each half: **optics** (a lens *is* the round trip, get and set in one value) and **profunctors** (a stage with an input side and an output side). They are one remedy: *a lens is exactly a thing that turns a `p field field` into a `p record record`, for any profunctor `p`*. So if a UI component is a profunctor, optics stop being a way to talk *about* the model and become a way to *assemble the interface* — the lens that focuses the customer's name is the value that mounts its text field into the customer form. That is the bet; the rest of the library is what follows from refusing anything that does not come from that algebra: no virtual DOM, no global store, no message plumbing, and an application's architecture readable off its types. The algebra is summarised under *Architecture → The algebra in brief*; doc/presentation-20min.md is the talk version.

**Where things are stated.** Each kind of statement has one home, and this file points at the others rather than restating them:

- **doc/guardrails.md** is normative: the strict MUST/MUST-NOT rules for the library, and the admission gates every proposed combinator, class, component or demo idiom passes (derivation → laws → subsumption → honesty → reachability → green stack → sync). Consult it before adding, changing or accepting any of those; a change that violates a guardrail is wrong even if it works. Two rules govern every change: **L15**, the demos are the compatibility contract (any library change leaves every demo compiling, bundling and behaving: `spago build`, `spago test`, `npm run bundle-demos`, `npm run smoke`, in full, with a demo edited only when it is the subject of the change), and **L16**, the import tower (only `PUI` and `Data.Profunctor.*` import the ecosystem's `Data.Profunctor`; vocabulary modules build from the carrier; application code imports neither, and `grep "import Data.Profunctor (" demo/` stays empty).
- **`.claude/skills/developing-bambik-apps/writing.md`** is the code-style contract for application code, demos included. Read it before writing or reviewing any demo or app module; a change to how applications are written edits writing.md and the demos. Its three deployed restatements are deliberate (deployed HTML cannot read the skill): the `#code-style` and `#tracing` notes in demo/index.html (the tracing note linked from every demo page), and **demo/workflow.html**, writing.md's *Writing order* replayed as a real session with verbatim compiler output. Re-read all three whenever the contract changes.
- **The module headers** are the contract of every word: what it does, its signature, its laws. `npm run api-docs` renders them into `generated-docs/md/` (gitignored).
- **CHANGELOG.md** holds the history: what was added, tried, withdrawn and when. A dated obituary belongs there, never in this file, a header or the guardrails, which point at a CHANGELOG date only where a precedent matters.

## Building

`npm install` first, then `export PATH=$PWD/node_modules/.bin:$PATH` and `spago build` (tests: `spago test`).

**The forked compiler.** The repo builds with the forked PureScript compiler pinned in `package.json` (variant row sugar `[ l :: T | r ]`, `.label` constructors — doc/variant-sugar.md). It installs from a **GitHub release** (`erykciepiela/purescript` tag `v0.15.16-variant.9`, branch `variant-type-sugar`; package-lock.json pins the tarball's hash), and the `Prim.Variant`-patched variant library is a git package in packages.dhall (`erykciepiela/purescript-variant` tag `v8.0.0-prim-variant.1`: `Data.Variant` re-exports the compiler's `Prim.Variant.Variant`, so the sugar and `inj`/`on`/`match` share one type). Nothing depends on a local checkout, so a plain clone builds on any Linux x86_64 machine. `node_modules/.bin/purs --version` must report `0.15.16 [development build ...]`; stock `0.15.15` fails with "Module Prim.Variant was not found".

**How applications consume it.** An application depends on bambik as an ordinary spago git package pinned to a release tag; spago clones it under `.spago/bambik/<tag>/`, demos and docs included. Two spago facts shape the scaffold: spago does not read a git package's own spago.dhall, so the app spells out the library's dependency list — fetched from the pinned tag by the bootstrap procedure, so nothing in this repo duplicates it and a new dependency needs no companion edit; and spago globs a git dependency's `src/**/*.purs` only, so the app's own `sources` adds `.spago/bambik/<tag>/extras/**/*.purs`. The procedure is the skill's bootstrap.md, which carries the scaffold inline and is copyable out of the repo. The skill is SKILL.md beside bootstrap.md, writing.md and building.md, each rule stated once.

### Development loop

Keep `spago build -w` running in the background and read its output after each edit (~0.7 s incremental) instead of one-shot builds; it covers the library, the tests and the demos. spago -w reads stdin and dies on EOF, so keep stdin open (never `</dev/null`), and run one watcher over the shared `output/` at a time. Parallel agents sharing the repo serialize compiles (`flock <lockfile> spago build`) or use git worktrees; when a serialized build fails, check the `Compiling` lines for whose module failed.

`npm run dev` serves **every** demo at `http://127.0.0.1:1234/`, rooted at `demo/` in the deploy's own layout (local `/nguis/inbox/?mdc3` is `/bambik/demo/nguis/inbox/?mdc3` on the host), so relative links resolve identically in both. Narrow the bundling by name, app or set: `npm run dev counter-mdc2 cells-mdc2`, `npm run dev inbox`, `npm run dev nguis`. scripts/dev.mjs rebuilds and reloads on every edit (~2 s): a `.purs`/`.js` edit runs an incremental `spago build` and esbuild rebundles the affected demos, a page's `.html` reloads at once, the reload reaching the browser over esbuild's SSE endpoint injected through the bundle banner. It polls mtimes rather than using `fs.watch`: inotify runs out of instances well before the ~40 source directories, and `recursive: true` delivers no events on ext4.

**Part tests: a part of a view in isolation.** A view split into parts (`Diner.View.Hall` in `View/Hall.purs`) has its tests beside each part, `View/Hall.test.mjs`: a list of `{ name, seed, run }`, `seed` naming prepared initial data — a value of the app's `<App>.Fixtures` (diner's `rushHour`, `soldOut`, `emptyDiner`) or of its view model — and `run` driving the part with real clicks, typing and picks and asserting on what it shows. `npm run part -- diner` (or `-- diner Hall`) mounts each part on its own, `body $ part # looped # with seed`, compiled into .part/ against a copy of output/, runs every test on a fresh headless-Chrome page, and fails a test on an assertion, a missing control, an exception or a console warning — a screenshot saved for each failure. The way to develop one part: write its test with the data the case needs, then iterate (about 25 s for all of diner's, the first run longer while it copies output/). The header of scripts/part-test.mjs is its manual.

In the browser, `window.__bambikTrace = true` (or `localStorage.setItem("bambik-trace", "true")`) logs every propagation decision: stage-to-stage flow, `looped` re-feeds and swallowed echoes, gate-withheld emissions with the fields they wait for. Every knowledge gate also carries a **starvation watchdog**: a gate still withholding after 3 s prints one `console.warn` naming the missing fields and the fix, and logs their stamped host elements beside it. The diagnostics are off until a carrier adopts them (`PUI.Web.adoptHostDiagnostics`, at `body`), so `spago test` is silent; `window.__bambikNoWarn = true` opts out.

### Verification

| Command | What it checks |
| --- | --- |
| `spago test` | test/Main.purs: the value-level laws of every combinator on `PUI Effect` probes (each word's laws are in its header); test/Exhaustive.purs: the merge and container-action laws over **every** event script to length 6 or 8, complete by data independence (about 150 s); the business optics (`Shutter`/`Reel`) in their own modules |
| `npm run smoke` | scripts/smoke/: a headless-Chrome CDP harness (isolated throwaway Chrome) for **only what the laws cannot reach** — no per-demo walks. The leaf-law bench over demo/laws/ (every published component alone, real input, per vocabulary), the mount check (every registered demo mounts with no exception or warning past the watchdog), the container action's identity in the real DOM, the axe-core stamp audit, and one file per carrier-only behaviour in scripts/smoke/tests/, each headed by what it walks. Bundle first; `npm run smoke -- <name>` filters |
| `npm run check-view-model` | the view-model rule made mechanical: no `Maybe` and no `Boolean` off the Boolean-editor allow-list in any demo's view model module, and no accessor section `_.l` in any demo view |
| `npm run check-determined` | guardrail L18 made mechanical: every demo's view compiled with a typed hole for every imported value, each exported signature checked to be its hint with the tails filled; `node scripts/check-determined.mjs <demo> …` names demos, `--write` writes the signatures (about a minute for all) |

## Building & Deploying Demos

1. Check the compiler (`purs --version`, as above).
2. Bundle: `npm run bundle-demos` (minified, all demos; `node scripts/bundle.mjs <name|set>` for a subset). `npm run dev` is for watching, not deploys. Every demo is a named module entered at its own function (`Counter.MDC2`/`counter`; no module is `Main`, so all compile under one `spago build`), and **scripts/demos.mjs** is the single registry of directory, module and entry, shared by the bundler, the dev server, check-determined and the smoke tests; it synthesizes each esbuild entry because `spago bundle-app` can only call `Main.main`. Bundling also stamps every page's `page.js?v=` with a hash of demo/page.js, since the host sends no cache headers and an unchanged URL would let browsers keep a stale page.js.
3. Deploy: `npm run deploy-demos` scps demo/index.html, demo/workflow.html, demo/page.js and both suite directories to host `xyz` (root@erykciepiela.xyz, see `~/.ssh/config`) at `/var/www/html/bambik/demo/`.
4. Verify: `http://erykciepiela.xyz/bambik/demo/<d>/` returns 200 (plain HTTP). The deploy deletes nothing, so a directory removed or renamed here lingers there until removed by hand.

**A standalone app.** `npm run deploy-diner` publishes diner bare, with no demo chrome, at `http://erykciepiela.xyz/diner/`: scripts/standalone.mjs writes `dist/<app>/` (gitignored) from the demo's bundle and its design system's stylesheets (bundle first), and the script copies it to `/var/www/html/diner/` on `xyz`.

**Demo pages** are pure structure over the shared chrome in **demo/page.js**. A page carries one `<template data-variant="mdc3" data-source="MDC3.purs Inbox.purs">` per design system, holding that system's stylesheets; page.js installs the one the query names (the first when none), drops the page's elements marked for other variants, and loads `bundle-<variant>.js` once the stylesheets have loaded. Space-separated sources render several listings. page.js fills the size readouts, shows each listing verbatim, and groups the running demo with its tracing note into `#demo-column` — on a page marked `<body data-surface>`, inside the vocabulary's card surface, since an app never wraps itself in one. The tracing note every page links to lives once in demo/index.html (`#tracing`).

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
newtype PUI m i o = PUI (m (Registration i o))
type Registration i o = (o -> Effect Unit) -> Effect (i -> Effect Unit)
```

A `PUI m i o` is a **wire with a face**: `i` the values pushed at it (the model to show), `o` the values it emits (what the user changed or did). Registering it hands over the callback for its `o` and returns the leg that pushes `i` at it, so nothing is pushed before it is listened to, and whatever the component remembers is allocated right there, once per use. `m` builds its face (for the DOM, `Web = StateT DOM Effect`). Everything is structure *on* this type, not machinery beside it.

**The four shapes are the four arrows of an app's cycle** (guardrails L3), and the rows between them are named by it — use these names for these positions and nothing else:

| Shape | From → to | The arrow | Citizens |
| --- | --- | --- | --- |
| `×→×` | **model** → **view model** | presentation: the state as the screen shows it, edits carried back | displays, editors, `group @l`, `actedOf` |
| `×→+` | **view model** → **view event** | interaction: the state shown, an occurrence emitted | emitters, emitter panes, emitting lists, `ticks` |
| `+→+` | **view event** → **model event** | interpretation: what happened on screen turned into what it means | `confirmed`, `modal` under `acknowledged`/`continued`, `action`, `joined` |
| `+→×` | **model event** → **model** | transition: the event folded into the next state | `fold`/`foldInto`/`reset`/`capture`, statuses |

The names are positions, not types: the model and the view model are one row wherever nothing narrows or derives. The **view model module** keeps its name: it holds the functions of all four arrows.

### The algebra in brief

The vocabulary falls out of the core type taken seriously, operator by operator.

- **Shapes — the compass.** Models worth having come in two shapes, a **record** (`×`, all of these at once: a form, a settings page) and a **variant** (`+`, one of these at a time: an event, a status, a step), so a component has four possible shapes, one module each under `Data.Profunctor.Row`, and every component of a vocabulary is a citizen of exactly one (`filledTextField @l` `×→×`, `button @l` `×→+`, `confirmed @l` `+→+`, `snackbar @l` `+→×`). An application is one walk around the compass: model → form, summary (`×→×`) → buttons (`×→+`) → backend, dialogs (`+→+`) → folds, statuses (`+→×`) → model.
- **Binary operators — the merges.** Each shape has one merge putting two components side by side. Its pattern is a law, not a coincidence: **sharing is open, responsibility is exclusive**, and the input decides which side is which. Under a record input every operand reads the whole row and owns its part of the output, like the entries of a record literal (fields at `×→×` — the knowledge-gated merge, partial emissions completed from the other operands' last contributions and a multi-field feed released once, never torn; cases at `×→+` — an emitter closed to the case its label names, so a block lists exactly the events that can occur). Under a variant input every operand owns its cases and all return one row, like the arms of a case match (`+→+`; `+→×`, whose shared output for an application's folds is the step to the next model). These are the product's and the sum's universal properties, the **pairing** and the **copairing**, swapping polarity across input and output — which is why there are four shapes and not two with a flip. The constraints are an honest price sheet: a shared side is one row variable and no class; every owned side checks disjointness at compile time (`DisjointLabels`) and carries runtime evidence exactly where its action is label-driven — `OwnedVariantInputs` reified case tags for dispatch, `OwnedRecordOutputs` reified field names for the gate's trimming, `OwnedVariantOutputs` none, since an emitted case is only forwarded. Editors and displays are not merge operands: each reads the row and passes it on, a **whole-row citizen** written as one more line of `Semigroupoid.do`, so code order is DOM order.
- **Nullary operators — the units.** The merges carry no unit of their own; they borrow `Category`'s `identity`: at `×→×` the wire from the shared row into `{}`, at `+→+` the wire `Variant () → Variant ()`, at `+→×` that wire entered from the empty variant. Chrome is the wire too — a divider or a heading reads and changes nothing, a stage at whatever row it stands in — which licenses decoration anywhere. A unit carries no information, so it must not be able to say anything: the record gates treat a contribution of zero fields as no contribution, so an operand owning no field cannot be told from the wire. What does speak at registration is the **point**, `announce a` (one emission out of the terminal record), which `with` uses to supply an app its initial state and from which the seeded echo wire `seeded a` is derived. The one merge no wire reaches is `×→+` — nothing maps `{}` into `Variant ()` — so its unit is the carrier's silent component.
- **Unary operators — strengths: the small component in the big model.** `Strong` gives `focusField @l`, the field lens behind every label-indexed editor and `group @l`; `Choice` gives `focusCase @l`, the case prism, the dialog's lift as `focusField` is the editor's. The mixed shapes need strengths of their own, coined here: **`Resolving`**, `p a b -> p (Tuple a c) (Either b c)`, one step of a loop answering with a decision (`Left` done, `Right` keep going), derived on `PUI` **from time** — a burst loops while the component moves and resolves at quiescence, with no "transient" flag on the wire; and **`Retaining`**, `p a b -> p (Either a c) (Tuple b c)`, a Mealy step receiving one case at a time and remembering the rest, which `(->)` cannot have, a stateless function having nowhere to keep memory. They underlie the `Shutter` and `Reel` optics.
- **Co-strengths — tying the knot.** Every co-strength is its strength's retraction, `co (strength g) ≅ g` once the channel is primed — the **trace quartet**:

  | strength | co-strength | ties into | co-optic | in the vocabulary |
  | --- | --- | --- | --- | --- |
  | `Strong` | `Costrong` / `unfirst` | state feedback | `Colens` | `looped`, the carrier's own word (a gated `unfirst` cannot self-feed) |
  | `Choice` | `Cochoice` / `unleft` | iteration | `Coprism` | none: holds raw; an event re-enters through its fold, a retry is written inside the action |
  | `Resolving` | `Coresolving` / `coresolve` | terminating fold | `Coshutter` | `debounced`, its one seeded use |
  | `Retaining` | `Coretaining` / `coretain` | productive unfold | `Coreel` | none: a running count is a model field |

  Each co-optic is its optic run backwards (`Colens s t a b ≅ Lens b a t s`, `Coprism` the reversed `Prism`, `Coshutter` the reversed `Reel`, `Coreel` the reversed `Shutter`). Where a lens carries its residual visibly in the type, a co-optic hides it and threads it through the loop — `Colens` reads each input against its own last output, `Coprism` is `tailRec` at the optic level, `Coshutter`'s fold state is a reader with no initial value (hence gating until primed), `Coreel` a generator. Strength optics give compositional access, co-optics compositional hiding — which is why the UI vocabulary uses none: state an application needs is a model field, shown in the rows, and the co-optics live on as business optics below the UI. The retraction pays a dividend: `coresolve (resolve g >>> seeded (Right c0)) ≈ debounced g` — **debouncing is a theorem**, and literally how `debounced` is written. The three `×`-side loops are provably dead unprimed (each gate waits on the other), while `Cochoice` holds raw, `unleft (left g) = g`: `PUI` is genuinely traced over `+` and only pointed-traced over `×`, which is why the knot is a carrier word and the `+` side needs none. That knot, `looped`, the `×`-diagonal self-trace, is primitive — no state before the first emission, no emission before the first input — and an application has exactly one, around every line that changes the model, every line of its body passing the row it is fed (the event path under `stepped`), so the model re-enters whole at one place and every whole-row stage's retained background is refreshed on every emission.
- **Closure — collections.** A list is not a fifth shape: `Array = μx. 1 + a×x` is the same two row forms closed under one `μ`, so collections are the **keyed square**, one class per shape, each the keyed analogue of a strength — `Acting` (`×→×`, a table of editors), `Gathering` (`×→+`, a list whose rows emit), `Dispatching` (`+→+`, one entity to the instance its key names), `Accumulating` (`+→×`, a board growing per new key). Identity is a field of the row, never a render index, so matched elements are re-fed in place and their nodes move with their keys: reconciliation without a virtual DOM, because the program already wrote the correspondence down.
- **The spine — `Category`.** `ui1 >>> ui2` pipes one component's output into the next's input, and a `Semigroupoid.do` pipeline reads top to bottom like the user's journey; the compass walk is one composite wire, and `looped` closes it into a circle. `identity` is no no-op but the **echo wire**: given a seed it is `seeded`, given a face a display, given nothing chrome — there is no boundary where the algebra ends and the UI begins.
- **What the laws buy.** Six laws, stated once in `Data.Profunctor.Row` and read at each shape — two on components (a record input is idempotent; every feed is answered once at `×→×` and never at `×→+`) and four on merges (symmetry and associativity, projection, preservation, monotonicity). What they buy an application is writing.md's *What the laws guarantee*; on `PUI` the record gate is a pure Mealy step and the merge laws are checked over every script to a bound that makes the check complete (test/Exhaustive.purs).

In one line: four **shapes** give the map; the **merges** lay components side by side; the **units** decorate and the **point** primes; the **strengths** let small components inhabit big models; the **co-strengths** say which loops can close and **`looped`** closes the one an application needs; the **keyed square** closes it all under lists; and **`Category`** strings the journey into one wire from page load to snackbar and back to the model.

### Source map

What lives where; each module's header is the contract of its words.

- **src/PUI.purs** — the carrier `PUI` and its instances (`Profunctor`, `Strong`, `Choice`, `Semigroupoid`, `Category`, the four row merges, `Resolving`/`Retaining`, the trace quartet), the knot `looped`, the point `announce`, and every host-neutral word: the application vocabulary (`with`, `stepped`, `landed`, `settled`, `fold`/`foldInto`/`reset`/`capture`, `action`/`load`, `joined`, the panes `inCase`/`provided`, the trailing words `introduced`/`supplied`/`continued`/`acknowledged`, `ticks`, `source`, `editsOf`) and the keyed square's `PUI` instances over `Hosting` beside the **plumbing** the vocabularies and law tests build on — the scope (`Scope`/`Scoped`), `Hosting`/`Interacting`, the leaf protocol as four wirings (`recordToRecordWiring`, `variantToRecordWiring`, `variantToVariantWiring`, `optionWiring`) and the leaves every vocabulary would otherwise copy (`eventLeaf`, `statusOver`, `confirmedOver`/`modalOver`), and the diagnostics switches. Its export list keeps the two apart; no demo writes a plumbing word. The algebra carries no JavaScript.
- **src/PUI/Gate.purs** — the knowledge gate as one pure Mealy machine over runtime participants (`gateStep`), shared by the `×→×` merge (participants: owned field labels) and the container action's gather (participants: fed keys, rekeyed per feed); data-independent with finite control, which is what makes test/Exhaustive.purs complete.
- **src/PUI/Web.purs** (+ Web.js) — the DOM carrier (`Web = StateT DOM Effect`, `Hosting Web`) and the element-neutral web vocabulary: decorators (`attr`/`:=`, `attrWith`/`attrAt`, `cl`, `clWhen`), text leaves (`text`/`textAt`, `staticText`, `staticHTML`), the canvas sources (`onClickedXY` and siblings), the platform sources (`geolocated`, `online`, `clock`, `colorScheme`), `separating`, `dynamic`/`each`/`el`, and the helpers every vocabulary builds its leaves from (`fieldLeaf`, `chrome`, `grouping`/`member`, `ensureStyle`, …). No element lives here.
- **src/PUI/Web/HTML.purs**, **SVG.purs** — the plain-HTML floor (element oculars, native controls, `body`, `filePicked`) and the SVG oculars.
- **src/PUI/Web/{MDC2,MDC3,Shoelace,Fluent,Bootstrap}.purs** (+ `.js`) — the design-system vocabularies, below.
- **extras/** — the algebra the carrier stands on, outside `src/` because claiming an ecosystem module name is a claim about what the module is; all roots compile under the one glob `extras/**/*.purs`. **row-profunctor** (`Data.Profunctor.Row` and its four shape modules: the merges with their qualified-do sugar, the field lens and case prism forms, the constraint vocabulary, the laws stated once in `Data.Profunctor.Row`), **keyed-profunctor** (`Data.Profunctor.Keyed`: the keyed square `Acting`/`Gathering`/`Dispatching`/`Accumulating` and their label forms), **structural** (structural `Eq`/`Ord` of plain values, so no instance is asked of a type the view does not fix), **variant** (`Data.Variant.Case.caseText`, a label read back as copy), and **profunctor**/**lenses** (the coined strengths and the co-optics, liftable into the ecosystem unchanged; law-tested, reached by no demo).
- **test/** — Main.purs (value-level laws), Exhaustive.purs (the bounded exhaustive check), and the business-optics examples.

### The design-system vocabularies

Six vocabularies — the HTML floor and five catalogues — make bambik a design-system umbrella, interchangeable by guardrail L20: the same concept has the same name and signature in each, a catalogue's renames, exclusives and gaps appear under its own names, and every vocabulary is entered through its own `body`, which does that catalogue's page setup at mount (none acts at import time). Every component is label-indexed: its label is the model field or case, its caption verbatim, and the accessible name stamped on its host element. Selectors are editors over option leaves (`select @"Flight type" $ VariantToVariant.do option @…`), in three words: plain, `…Unpicked` (a choice owed) and `…Optional` (a choice that may be left unmade).

| Module | Basis | Worth knowing |
| --- | --- | --- |
| `PUI.Web.MDC2` | `material-components-web`: documented markup plus a foundation instance per component | the fullest catalogue (`listOf`, data table, image list, drawer with a live nav slot, `banner`, menus, chips); text fields write through the foundation's `value` so the label float stays foundation-managed |
| `PUI.Web.MDC3` | `@material/web` custom elements, property and event wiring | mirrors MDC2's structure; MD3's renames (typescale, emphasis siblings, elevations) and no `banner`; entries the catalogue lacks (segmented button, snackbar, card, top app bar, drawer, data table, image list, tooltip) hand-rolled over `--md-sys-*` tokens, each injecting its stylesheet once (`ensureStyle`); `tile`/`doubleClickTile` for grids |
| `PUI.Web.Shoelace` | `@shoelace-style/shoelace` custom elements (Lit) | the MDC3 recipe; exclusive star `rating`; its own names where the concept differs (`textField`, `toast`, `progressBar`); no typography (HTML's oculars are its type scale); `body` sets the icon base path |
| `PUI.Web.Fluent` | `@fluentui/web-components` v3, theme set by `body` | exclusives `ratingDisplay` and `messageBar`. **Caveat**: FAST binds a beat after insertion and its update queue is rAF-driven (starving in frameless headless sessions), so the dropdown and radio group defer writes on a timer poll (`whenBoundDo` in Fluent.js); the dropdown's options must sit in a `<fluent-listbox>` |
| `PUI.Web.Bootstrap` | CSS only: native elements in documented classes, the stylesheet linked from the CDN, not an npm dependency | the toast's `autoDismiss` timer is its only FFI; `sliderLive` only; no dialog |

### Composition

- `Semigroupoid.do` (`import QualifiedDo.Semigroupoid as Semigroupoid`) composes stages; every line is a stage that passes the row it is fed — editors, displays, statics, panes, lists, and the event path `( emitters; dialogs; folds ) # stepped`.
- `RecordToRecord.do`, `RecordToVariant.do`, `VariantToVariant.do`, `VariantToRecord.do` are the four merges. **A line passes, an operand owns**: a word either passes the row and is a line, or owns what it names and is an operand of its shape's merge (sources in `RecordToRecord.do`, emitters in `RecordToVariant.do`, options in `VariantToVariant.do`, folds and statuses in `VariantToRecord.do`). A label written twice in a block, a fold for a case nothing emits and an emitted case with no fold are compile errors.
- An application is one knot: `body $ Semigroupoid.do { lines } # looped # with seed`. The rest of the app shape — event lines, dialogs, actions, loads, panes, collections — is writing.md's *App shape*, with the demo that shows each.

## Demo Structure

105 demos on 46 pages, registered in **scripts/demos.mjs**. Two suites: **demo/7guis/** (the [7GUIs](https://eugenkiss.github.io/7guis/) benchmark, compared with other frameworks in doc/7guis-comparison.md) and **demo/nguis/** (showcase apps, mostly one idea each, listed on demo/nguis/index.html). README's reading order — counter, temperature-converter, flight-booker, todo-list, checkout, order-form — is the path through them; order-form is the four-shape flagship. demo/laws/ holds the leaf-law benches, one page per vocabulary.

Conventions every demo follows:

- **One app, one directory, every design system it has the leaves for.** The view module per design system is named by it (`MDC2.purs` is `Counter.MDC2`, `HTML.purs` is `Counter.HTML`), entered at the app's name (`counter`), never `Main`; the shared view model module is the app's (`Counter.purs`, module `Counter`), so a twin diff is view-only by construction. An app with one design system for good may name its view `View` instead (diner's `Diner.View` in `View.purs`, its parts `Diner.View.Hall` in `View/Hall.purs`; registered `'diner': { mdc3: 'View' }`). One page switches between the twins (`counter/?mdc3`); a page with one variant shows no switcher. A twin exists only where its vocabulary has the leaves the app needs — no twin builds a leaf from a bare element. A further module is named the same way (order-dashboard's `ControlsMDC3.purs` is `OrderDashboard.ControlsMDC3`); which module is the view model is the registry's to say.
- **The view determines its view model** (guardrails L18), checked by `npm run check-determined`.
- **Naming**: MDC2/MDC3 name the vocabularies, modules, directories and UI labels; plain MD2/MD3 is reserved for the specs (m2/m3.material.io).

## Key Dependencies

- `profunctor-lenses` — profunctor optics
- `foreign` — reads a plain value's structure for the structural `Eq`/`Ord` (`Data.Structural`)
- `qualified-do` — the `do` sugar for composition and the merges
- `material-components-web` (MDC2), `@material/web` (MDC3), `@shoelace-style/shoelace` (Shoelace; pages link its light theme from the matching CDN release), `@fluentui/web-components` (Fluent; theme tokens ship in the bundle)
- Bootstrap is CSS-only and not an npm dependency: pages link the Bootstrap 5 stylesheet from the CDN
