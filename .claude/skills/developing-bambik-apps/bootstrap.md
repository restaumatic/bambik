# Bootstrapping a bambik application

This procedure creates, from nothing but node + git + network, a single
application directory that builds, bundles and runs locally. bambik is an
ordinary dependency — there is **no repo to clone** and nothing to vendor:
the library is a spago git package pinned to a tag, the compiler is an npm
package from a GitHub release, and the patched variant library is another
git package. All three resolve on the first `npm install` / `spago build`.

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
| bambik library    | `packages.dhall`| tag `v0.1.1` of `restaumatic/bambik`          |
| variant fork      | `packages.dhall`| tag `v8.0.0-prim-variant.1`                   |
| forked compiler   | `package.json`  | release URL + integrity hash in the lock      |

Spago clones each git package whole, so after the first build the library's
**demos, docs and CLAUDE.md** sit in `.spago/bambik/<tag>/` as worked
examples — `demo/7guis/`, `demo/nguis/`, `doc/type-errors.md`, the module
headers under `src/`.

## Steps

1. **Settle the design system** — it decides the npm dependency, the
   page's CSS links and the vocabulary the app module imports, so it has
   to be known before anything is copied. If the developer did not name
   one, **ask**; do not default silently to the starter's MDC2. The
   choices are the five rows of the [table below](#design-systems), plus
   plain HTML (`PUI.Web.HTML` + `PUI.Web.SVG`, no design system at all —
   the app supplies its own CSS, as `demo/nguis/restaurant-menu/` does).
   Worth stating when asking: the vocabularies are interchangeable — same
   two-sorted structure, same citizenship, same names where the concept
   exists in both catalogs — so the choice is a look, not an
   architecture, and switching later is switching the import plus the
   page's CSS.

2. **Write the seven scaffold files** into a fresh directory, from the
   [Scaffold](#scaffold) section below. Three names are chosen once and
   used throughout: `<app>` (kebab-case, the directory and package
   name), `<Module>` (PascalCase, the view module — the logic module
   beside it is `<Module>Logic`) and `<entryFn>` (camelCase, the
   exported entry function — named after the application, never
   `main`).

   ```
   <app>/package.json
   <app>/packages.dhall
   <app>/spago.dhall
   <app>/entry.mjs
   <app>/public/index.html
   <app>/src/<Module>.purs        ← the view module; with its logic module,
   <app>/src/<Module>Logic.purs   ← the deliverable — the rest is scaffolding
   ```

3. **Install and check the compiler**:

   ```sh
   npm install
   node_modules/.bin/purs --version   # must say 0.15.16 [development build ...]
   export PATH=$PWD/node_modules/.bin:$PATH
   ```

4. **Build** — the first run fetches the package set, clones bambik and the
   variant fork, and compiles the lot (a few minutes); after that an
   app-module edit rebuilds in well under a second:

   ```sh
   spago build
   ```

5. **Bundle and run** — `npm run bundle` writes the minified
   `public/bundle.js` (~500 kB for the starter) and `npm run dev` serves
   `public/` at `http://127.0.0.1:8000`. The dev loop, its stdin caveat,
   and how to verify a running page are [building.md](building.md).

## Scaffold

Seven files, written fresh each time rather than copied from a stored
template — the one part that could drift, the library's dependency list,
is fetched from the tag in the process.

### package.json

`<design-system-package>` is the npm dependency from the
[table below](#design-systems); drop the whole `dependencies` block for
Bootstrap or plain HTML, which need none. Keep the version ranges the
library itself uses — `.spago/bambik/<tag>/package.json` after the first
build, or
`https://raw.githubusercontent.com/restaumatic/bambik/<tag>/package.json`
before it.

```json
{
  "name": "<app>",
  "private": true,
  "scripts": {
    "build": "spago build",
    "watch": "spago build -w",
    "bundle": "spago build && esbuild entry.mjs --bundle --minify --format=esm --outfile=public/bundle.js",
    "dev": "esbuild entry.mjs --bundle --format=esm --outfile=public/bundle.js --servedir=public --serve=127.0.0.1:8000"
  },
  "dependencies": {
    "<design-system-package>": "^14.0.0"
  },
  "devDependencies": {
    "esbuild": "0.25.1",
    "purescript": "https://github.com/erykciepiela/purescript/releases/download/v0.15.16-variant.6/purescript-0.15.16-variant.6.tgz",
    "spago": "^0.21.0"
  }
}
```

spago is pinned to 0.21 — the legacy dhall-based line this scaffold is
written for. Do not upgrade it to the 0.9x rewrite, which uses
`spago.yaml` and a different package-set mechanism.

### packages.dhall

The upstream package set plus three overrides: `variant` repointed at
the `Prim.Variant`-patched fork, `convertable-options` (not in the set),
and `bambik` itself.

**The `bambik` entry must spell out the library's own dependency list**,
because spago does not read a git package's `spago.dhall`. Do not
transcribe the list from memory or from this document — read it from the
tag being pinned, so it is right by construction:

```sh
curl -sfL https://raw.githubusercontent.com/restaumatic/bambik/<tag>/spago.dhall \
  | sed -n '/^, dependencies/,/^  ]/p'
```

That prints the library's own one-per-line dhall formatting. Re-emit the
names as a comma-separated dhall list — every entry quoted, commas
*between* entries, no trailing comma, and no identifier broken across a
line wrap (a mangled list fails as a dhall parse error, not a helpful
one). Then write the file, substituting that list and the tag:

```dhall
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.10-20231023/packages.dhall
        sha256:b9a482e743055ba8f2d65b08a88cd772b59c6e2084d0e5ad854025fa90417fd4

in  upstream
  with variant.repo = "https://github.com/erykciepiela/purescript-variant.git"
  with variant.version = "v8.0.0-prim-variant.1"
  with convertable-options =
    { dependencies = [ "console", "effect", "maybe", "record" ]
    , repo = "https://github.com/natefaubion/purescript-convertable-options.git"
    , version = "v1.0.0"
    }
  with bambik =
    { dependencies =
      [ <the list fetched above> ]
    , repo = "https://github.com/restaumatic/bambik.git"
    , version = "<tag>"
    }
```

If a later library version gains a dependency, this list needs the same
addition or the build fails with a missing module from some *other*
library while compiling bambik.

### spago.dhall

An ordinary config. Add dependencies as the app's imports grow — imports
are 100% explicit, so the list follows them.

```dhall
{ name = "<app>"
, dependencies = [ "bambik", "effect", "prelude", "qualified-do", "variant" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
```

### entry.mjs

The esbuild entry: imports the app's entry function from spago's
`output/` and calls it. Needed because `spago bundle-app` can only call
`Main.main`, and no bambik module is `Main`.

```js
import { <entryFn> } from './output/<Module>/index.js'
<entryFn>()
```

### public/index.html

Minimal page — the app mounts into the document body at runtime, so
there is no markup to write. An empty `<html>` with:

- the usual `charset` and `viewport` meta tags, and a `<title>`;
- the chosen design system's stylesheet `<link>`s, copied from its demo
  page (last column of the table below) — plain HTML links whatever CSS
  the app supplies instead;
- a `<style>` giving the body a margin and the design system's font
  family;
- `<script type="module" src="bundle.js">`, the bundle esbuild writes;
- optionally `<script>window.__bambikTrace = true</script>` to turn on
  the emission trace, as every demo page does.

Do **not** copy a demo's page wholesale: those carry the suite's own
chrome — source panel, `page.js`, highlight.js, the back-link header —
none of which belongs in an application. Take only the `<link>`s.

### src/&lt;Module&gt;.purs and src/&lt;Module&gt;Logic.purs

The application itself, written to the rules in
[writing.md](writing.md): the view module and the logic module it
imports — view depends on the logic module and the design system, logic
only on the domain. If the developer's app is not yet specified, copy
the counter demo as the starter:
`.spago/bambik/<tag>/demo/7guis/counter-mdc2/CounterMDC2.purs` as
`src/<Module>.purs` and the shared
`.spago/bambik/<tag>/demo/7guis/counter/CounterLogic.purs` as
`src/<Module>Logic.purs`, renaming the modules and entry function to
`<Module>`, `<Module>Logic` and `<entryFn>` (the view module's import of
`CounterLogic` follows the rename). It is a complete working app in
twenty-odd lines — the MVU shape, a display, an event button and a
business function — so a green build of it proves the whole toolchain.
Its page, `demo/7guis/counter-mdc2/index.html`, is the source of the CSS
links above.

For a vocabulary other than MDC2, copy the counter's sibling view module
`counter-mdc3/` instead, or the demo named in the last column of the
table below: the logic module is the same whatever the vocabulary, the
oculars the counter wraps its content in exist under each catalog's own
names, and drop them entirely for plain HTML.

## Design systems

The starter uses MDC2. To use another vocabulary, switch the import in the
app module, the npm dependency, and the page's CSS:

| Vocabulary module    | npm dependency               | index.html needs                          | Demo to copy from            |
|----------------------|------------------------------|-------------------------------------------|------------------------------|
| `PUI.Web.MDC2`       | `material-components-web`    | MDC CSS + Material Icons links (starter)   | any `*-mdc2/`                |
| `PUI.Web.MDC3`       | `@material/web`              | Roboto + Material Symbols Outlined fonts   | `espresso-bar-mdc3/`         |
| `PUI.Web.Shoelace`   | `@shoelace-style/shoelace`   | Shoelace light theme CSS from CDN          | `product-review-shoelace/`   |
| `PUI.Web.Fluent`     | `@fluentui/web-components`   | nothing (tokens ship in the bundle)        | `meeting-booker-fluent/`     |
| `PUI.Web.Bootstrap`  | — (CSS-only)                 | Bootstrap 5 CSS from CDN                   | `loan-calculator-bootstrap/` |
| `PUI.Web.HTML` alone | — (none)                     | whatever CSS the app itself supplies       | `restaurant-menu/`           |

Match the versions bambik pins in its own `package.json`, and copy the
exact CDN links from that vocabulary's demo page under
`.spago/bambik/<tag>/demo/nguis/`.

The last row is the no-design-system case: element oculars from
`PUI.Web.HTML` (and `PUI.Web.SVG`) styled by the app's own CSS, no npm
component library at all. Everything else in this procedure is unchanged
by the choice — same scaffold, same build, same rules in
[writing.md](writing.md).

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
- dhall errors mentioning an absolute `/home/...` path — `packages.dhall`
  points at a local checkout instead of the tagged git package; rewrite
  it from [Scaffold](#packagesdhall) above.
- Custom type errors from the row layer — read
  `.spago/bambik/<tag>/doc/type-errors.md` before fighting a merge error.
- Port busy — change `--serve=127.0.0.1:8000` in `package.json`.
