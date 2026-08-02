# Bootstrapping a bambik application outside the repo

This procedure creates, from nothing but node + git + network, a single
application directory that builds, bundles and runs locally. bambik is an
ordinary dependency — there is **no repo to clone** and nothing to vendor:
the library is a spago git package pinned to a tag, the compiler is an npm
package from a GitHub release, and the patched variant library is another
git package. All three resolve on the first `npm install` / `spago build`.

**Installing this skill elsewhere**: the whole `developing-bambik-apps/`
directory (SKILL.md, this file, templates/) is self-contained — copy it
into any project's `.claude/skills/` and it works there. It also travels
inside the bambik package, so once an app has built, later sessions can
install it from `.spago/bambik/v0.1.0/.claude/skills/`.

## Prerequisites

- **Linux x86_64** — the forked PureScript compiler installs from its
  GitHub release, as an npm package that drop-in replaces `purescript` and
  bundles a prebuilt binary (nothing is downloaded at install time beyond
  the tarball itself):

  ```json
  "purescript": "https://github.com/erykciepiela/purescript/releases/download/v0.15.16-variant.6/purescript-0.15.16-variant.6.tgz"
  ```

  The same release carries the bare binary as asset `purs`, for use without
  npm. On another platform, build the `variant-type-sugar` branch (PR #1)
  of https://github.com/erykciepiela/purescript with `stack install` and
  either use that binary directly or repack the tarball with it as
  `purs.bin`.
- node ≥ 18 with npm, git, network access.
- Stock purs **cannot** build bambik code — it fails with
  `Module Prim.Variant was not found`. The forked compiler adds variant
  sugar (`[ ok :: Int ]` types, `.ok 42` injectors/patterns; see
  `doc/variant-sugar.md` in the fetched package), and the matching fork of
  `purescript-variant`
  (`erykciepiela/purescript-variant`, tag `v8.0.0-prim-variant.1`, which
  the scaffold's packages.dhall names) re-exports the compiler's built-in
  `Prim.Variant.Variant` so the sugar and `Data.Variant`'s
  `inj`/`on`/`match` meet on one type. Spago fetches it like any git
  package — nothing to vendor or check out.

## Where the dependencies come from

The app directory stands alone; nothing lives beside it. Its three
non-registry dependencies are named by URL and pinned by tag:

| Dependency        | Named in        | Pinned by                                    |
|-------------------|-----------------|----------------------------------------------|
| bambik library    | `packages.dhall`| tag `v0.1.0` of `restaumatic/bambik`          |
| variant fork      | `packages.dhall`| tag `v8.0.0-prim-variant.1`                   |
| forked compiler   | `package.json`  | release URL + integrity hash in the lock      |

Spago clones each git package whole, so after the first build the library's
**demos, docs and CLAUDE.md** sit in `.spago/bambik/v0.1.0/` as worked
examples — `demo/7guis/`, `demo/nguis/`, `doc/type-errors.md`, the module
headers under `src/`.

## Steps

1. **Scaffold the app** from this skill's `templates/` directory. Copy it
   verbatim first — the starter is a complete working counter named
   `myapp`/`MyApp`/`myApp` — then rename the three tokens to the
   application's name (kebab-case dir/package, PascalCase module,
   camelCase entry function; the entry function is named after the
   application, never `main`):

   ```sh
   cp -r <this-skill-dir>/templates <app>
   cd <app>
   # rename: myapp -> <app>, MyApp -> <Module>, myApp -> <entryFn>
   sed -i 's/MyApp/<Module>/g; s/myApp/<entryFn>/g; s/myapp/<app>/g' \
     package.json spago.dhall entry.mjs public/index.html src/MyApp.purs
   mv src/MyApp.purs src/<Module>.purs
   ```

   What the scaffold is:
   - `package.json` — `purescript` from the compiler release URL, `spago`
     0.21 (legacy, dhall-based), `esbuild`, and the design-system npm
     package (see the table below).
   - `packages.dhall` — the upstream package set, plus three entries:
     `bambik` (repo + tag + its dependency list), `variant` repointed at
     the `Prim.Variant`-patched fork, and `convertable-options`. The
     `bambik` entry spells out the library's dependencies because spago
     does not read a git package's own `spago.dhall`; if the library gains
     one, this list needs the same addition or the build fails with a
     missing module.
   - `spago.dhall` — an ordinary config: `dependencies = [ "bambik",
     "effect", "prelude", "qualified-do", "variant" ]` and
     `sources = [ "src/**/*.purs" ]`. Add dependencies as the app's
     imports grow — imports are 100% explicit, so the list follows them.
   - `entry.mjs` — the esbuild entry: imports the app's entry function
     from spago's `output/` and calls it (`spago bundle-app` can only call
     `Main.main`, and no bambik module is `Main`).
   - `public/index.html` — minimal page: design-system CSS from CDN and
     `<script type="module" src="bundle.js">`; the app mounts into
     `<body>` at runtime via `body $ …`.
   - `src/<Module>.purs` — the application. Replace the starter's content
     with the application the developer specified, written to SKILL.md's
     rules (this file is the deliverable; everything else is scaffolding).

2. **Install and check the compiler**:

   ```sh
   npm install
   node_modules/.bin/purs --version   # must say 0.15.16 [development build ...]
   export PATH=$PWD/node_modules/.bin:$PATH
   ```

3. **Build** — the first run fetches the package set, clones bambik and the
   variant fork, and compiles the lot (a few minutes); after that an
   app-module edit rebuilds in well under a second:

   ```sh
   spago build
   ```

4. **Bundle and run**:

   ```sh
   npm run bundle          # spago build + minified public/bundle.js (~500 kB for the starter)
   npm run dev             # serves public/ at http://127.0.0.1:8000
   ```

   The dev loop is two terminals: `npm run watch` (incremental spago) and
   `npm run dev` (esbuild rebundles from `output/` on request — refresh the
   browser after an edit). **Both die when stdin closes**, so an agent
   backgrounding them must keep stdin open (`tail -f /dev/null | npm run
   dev`, never `</dev/null`), and only one watcher may own `output/` at a
   time. Verify the page: HTTP 200 on `http://127.0.0.1:8000/` and on
   `/bundle.js`, the app rendered inside `<body>`, no console errors — the
   app mounts asynchronously, so poll for a rendered element rather than
   sampling once after a fixed delay. For emission-level debugging set
   `window.__bambikTrace = true` in the console (log level Verbose); a
   knowledge gate left unfed warns after 3s naming the missing fields.

## Design systems

The starter uses MDC2. To use another vocabulary, switch the import in the
app module, the npm dependency, and the page's CSS:

| Vocabulary module    | npm dependency               | index.html needs                          |
|----------------------|------------------------------|-------------------------------------------|
| `PUI.Web.MDC2`       | `material-components-web`    | MDC CSS + Material Icons links (starter)   |
| `PUI.Web.MDC3`       | `@material/web`              | Roboto + Material Symbols Outlined fonts   |
| `PUI.Web.Shoelace`   | `@shoelace-style/shoelace`   | Shoelace light theme CSS from CDN          |
| `PUI.Web.Fluent`     | `@fluentui/web-components`   | nothing (tokens ship in the bundle)        |
| `PUI.Web.Bootstrap`  | — (CSS-only)                 | Bootstrap 5 CSS from CDN                   |

Match the versions bambik pins in its own `package.json`; copy the exact
CDN links from a demo page of that vocabulary, under
`.spago/bambik/v0.1.0/demo/nguis/`: `espresso-bar-mdc3/` for MDC3,
`product-review-shoelace/`, `meeting-booker-fluent/`,
`loan-calculator-bootstrap/`.

## Updating and pinning

To move to a newer library, bump `bambik.version` in `packages.dhall` to a
newer tag and re-run `spago build`; spago fetches that tag into its own
`.spago/bambik/<tag>/`, so nothing is upgraded behind your back. The other
two are pinned the same way: the compiler by the release URL in
`package.json` (npm records the tarball's integrity hash in
`package-lock.json`, so a re-published asset of the same name is rejected
rather than silently swapped) and the variant fork by `variant.version`.
The three move together in practice — the fork's patch only compiles under
that compiler, and the library needs both — so change them as a set, and
check the library's `dependencies` list in the `bambik` entry when you do.

## Troubleshooting

- `Module Prim.Variant was not found` — stock purs got installed; check
  `package.json`'s `purescript` entry is the release URL, re-run
  `npm install`, confirm with `purs --version`.
- `Module PUI was not found` — `bambik` is missing from `spago.dhall`'s
  `dependencies`, or its `packages.dhall` entry failed to fetch (check
  `.spago/bambik/<tag>/src/PUI.purs` exists).
- A missing module from some *other* library while compiling bambik itself
  — the `bambik` entry's `dependencies` list is behind the library's;
  compare it with `spago.dhall` in `.spago/bambik/<tag>/`.
- `Module Prim.Variant was not found` *while compiling `Data.Variant`* —
  the `variant` override is absent, so the stock library is being built;
  check both `with variant.repo` and `with variant.version` are present.
- dhall errors mentioning an absolute `/home/...` path, or a `variant`
  Location pointing into a `vendor/` directory — the scaffold predates the
  move to GitHub-hosted dependencies; re-copy `templates/packages.dhall`.
- Custom type errors from the row layer — read
  `.spago/bambik/<tag>/doc/type-errors.md` before fighting a merge error.
- Port busy — change `--serve=127.0.0.1:8000` in `package.json`.

## Maintainer note (cutting a new compiler release)

The compiler is distributed as a release on the fork, tagged at the commit
`purs --version` reports, with two assets: the npm tarball (bundling
`purs.bin`) and the bare `purs` binary. To publish a rebuild:

```sh
gh release create v0.15.16-variant.7 \
  --repo erykciepiela/purescript --target <build-commit-sha> \
  --title "purs 0.15.16-variant.7 — variant sugar (Linux x86_64)" \
  purescript-0.15.16-variant.7.tgz purs
```

Then bump the URL in bambik's `package.json` and in this skill's
`templates/package.json`, and re-run `npm install` in both so the lock
records the new integrity hash. Never replace an asset in place — the
pinned hash would reject it, correctly.

The variant library is the same story one repo over: the patch lives on
branch `prim-variant` of `erykciepiela/purescript-variant`, tagged
`v8.0.0-prim-variant.1`, and `master` is left at upstream so the fork stays
easy to rebase. To revise it, push a new tag and bump `variant.version` in
both `packages.dhall` files. Should the sugar ever land in upstream
purescript, both forks dissolve: `variant` goes back to the package set and
`purescript` to the official npm package.
