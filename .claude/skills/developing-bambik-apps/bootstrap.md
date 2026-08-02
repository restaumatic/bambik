# Bootstrapping a bambik application outside the repo

This procedure creates, from nothing but git + node + network, a workspace
holding a bambik clone (the library, plus the demos for reference) and a
buildable, bundlable, locally runnable application. Every dependency comes
from GitHub — the library from its repo, the compiler from a release, the
patched variant library from a forked repo — so nothing depends on the
maintainer's machine.

**Installing this skill elsewhere**: the whole
`developing-bambik-apps/` directory (SKILL.md, this file, templates/) is
self-contained — copy it into any project's `.claude/skills/` and it works
there. It also ships inside the bambik clone, so after step 1 a
bootstrapped workspace can install it for later sessions with
`cp -r bambik/.claude/skills/developing-bambik-apps <app>/.claude/skills/`.

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
  sugar (`[ ok :: Int ]` types, `.ok 42` injectors/patterns; see the clone's
  `doc/variant-sugar.md`), and the matching fork of `purescript-variant`
  (`erykciepiela/purescript-variant`, tag `v8.0.0-prim-variant.1`, which
  the scaffold's packages.dhall names) re-exports the compiler's built-in
  `Prim.Variant.Variant` so the sugar and `Data.Variant`'s
  `inj`/`on`/`match` meet on one type. Spago fetches it like any git
  package — nothing to vendor or check out.

## Workspace layout

```
<workspace>/
  bambik/       # git clone — the library's src/ and the demos
  <app>/        # the application (scaffolded from this skill's templates/)
```

Only the app's `spago.dhall` reaches into the clone — by the relative path
`../bambik`, for the library's sources and its dependency list — so the two
directories must stay siblings. Nothing else needs it: `package.json` names
the compiler release, and `packages.dhall` (byte-identical to bambik's own)
names the variant fork.

## Steps

1. **Clone bambik** (the library, and the demos as worked examples):

   ```sh
   mkdir <workspace> && cd <workspace>
   git clone --depth 1 https://github.com/restaumatic/bambik.git
   ```

2. **Scaffold the app** from this skill's `templates/` directory. Copy it
   verbatim first — the starter is a complete working counter named
   `myapp`/`MyApp`/`myApp` — then rename the three tokens to the
   application's name (kebab-case dir/package, PascalCase module,
   camelCase entry function; the entry function is named after the
   application, never `main`):

   ```sh
   cp -r <this-skill-dir>/templates <workspace>/<app>
   cd <workspace>/<app>
   # rename: myapp -> <app>, MyApp -> <Module>, myApp -> <entryFn>
   sed -i 's/MyApp/<Module>/g; s/myApp/<entryFn>/g; s/myapp/<app>/g' \
     package.json spago.dhall entry.mjs public/index.html src/MyApp.purs
   mv src/MyApp.purs src/<Module>.purs
   ```

   What the scaffold is:
   - `package.json` — `purescript` from the compiler release URL, `spago`
     0.21 (legacy, dhall-based), `esbuild`, and the design-system npm
     package (see the table below).
   - `packages.dhall` — the upstream package set bambik pins, plus two
     overrides it also uses: `variant` repointed at the
     `Prim.Variant`-patched fork (`with variant.repo`/`.version`) and
     `convertable-options`.
   - `spago.dhall` — the library is **not** a package: bambik's `src/` is
     compiled by the app's own `sources` glob
     (`[ "src/**/*.purs", "../bambik/src/**/*.purs" ]`), and the app's
     `dependencies` are inherited with
     `(../bambik/spago.dhall).dependencies` so the list never drifts from
     the library's — app-only additions append (`# [ "argonaut" ]`). Why
     not a Location package: legacy spago resolves a Location package's
     `sources` glob *inside* that package's directory, stripping leading
     `../` segments, so no config file placed beside the library can point
     out at `src/` — the glob has to belong to the app. (A consequence:
     `spago build` warns that a few inherited deps — `console`, `lists`,
     `random` — are unused until the app imports them. Expected; trimming
     the list means abandoning the inheritance.)
   - `entry.mjs` — the esbuild entry: imports the app's entry function
     from spago's `output/` and calls it (`spago bundle-app` can only call
     `Main.main`, and no bambik module is `Main`).
   - `public/index.html` — minimal page: design-system CSS from CDN and
     `<script type="module" src="bundle.js">`; the app mounts into
     `<body>` at runtime via `body $ …`.
   - `src/<Module>.purs` — the application. Replace the starter's content
     with the application the developer specified, written to SKILL.md's
     rules (this file is the deliverable; everything else is scaffolding).

3. **Install and check the compiler**:

   ```sh
   npm install
   node_modules/.bin/purs --version   # must say 0.15.16 [development build ...]
   export PATH=$PWD/node_modules/.bin:$PATH
   ```

4. **Build** — the first run fetches the package set and compiles it plus
   the bambik library (a few minutes); after that an app-module edit
   rebuilds in well under a second:

   ```sh
   spago build
   ```

5. **Bundle and run**:

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
CDN links from a demo page of that vocabulary in the clone — under
`demo/nguis/`: `espresso-bar-mdc3/` for MDC3, `product-review-shoelace/`,
`meeting-booker-fluent/`, `loan-calculator-bootstrap/`.

## Updating and pinning

`git -C ../bambik pull && spago build` picks up a newer library. To pin the
library, keep the clone at a commit (`git -C ../bambik checkout <sha>`).
The other two pin independently and are already pinned by tag: the compiler
by the release URL in `package.json` (npm records the tarball's integrity
hash in `package-lock.json`, so a re-published asset of the same name is
rejected rather than silently swapped) and the variant fork by
`variant.version` in `packages.dhall`. The three move together in practice
— the fork's patch only compiles under that compiler — so change them as a
set.

## Troubleshooting

- `Module Prim.Variant was not found` — stock purs got installed; check
  `package.json`'s `purescript` entry is the release URL, re-run
  `npm install`, confirm with `purs --version`.
- `Module PUI was not found` while the build otherwise succeeds — the
  sources glob lost `../bambik/src/**/*.purs`, or the clone is not the
  app's sibling (`ls ../bambik/src/PUI.purs`).
- `when importing local packages you should point to their spago.dhall
  file` — a Location entry names something other than `spago.dhall`.
- dhall errors mentioning an absolute `/home/...` path, or a `variant`
  Location pointing into `../bambik/vendor/` — the clone or the scaffold
  predates the move to GitHub-hosted dependencies; `git -C ../bambik pull`
  and re-copy `templates/packages.dhall`.
- Custom type errors from the row layer — read the clone's
  `doc/type-errors.md` before fighting a merge error.
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
