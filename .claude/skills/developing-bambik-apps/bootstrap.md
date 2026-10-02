# Bootstrapping a bambik application

Creates, from node + git + network alone, an app directory that builds,
bundles and runs. bambik is an ordinary spago git package pinned to a
tag; nothing is cloned by hand or vendored.

## Prerequisites

- **Linux x86_64.** The forked PureScript compiler installs from a
  GitHub release as an npm package with a prebuilt binary. Stock `purs`
  cannot build bambik code (`Module Prim.Variant was not found`).
- node ≥ 18 with npm, git, curl, network access.

## Pins

Three pins, moved together — the variant fork compiles only under that
compiler, and the library needs both:

| Dependency      | Named in         | Pin                                                   |
|-----------------|------------------|-------------------------------------------------------|
| bambik          | `packages.dhall` (and the second `sources` glob in `spago.dhall`) | tag `v0.1.6` of `restaumatic/bambik` |
| variant fork    | `packages.dhall` | tag `v8.0.0-prim-variant.1` of `erykciepiela/purescript-variant` |
| forked compiler | `package.json`   | release `v0.15.16-variant.9` of `erykciepiela/purescript` |

Below, `<tag>` is the bambik tag (`v0.1.6`).

## Steps

1. **Choose the design system.** ⚠ **Decision point**: if the developer
   has not named one, **ask** — do not default silently. It decides the
   npm dependency, the page's links and the starter view module. The
   choice is a look, not an architecture:

   | Vocabulary module   | npm dependency (`package.json`)          | Page links (`public/index.html`) | Body font | Counter twin (`<twin>` / `<Suffix>`) |
   |---------------------|------------------------------------------|----------------------------------|-----------|--------------------------------------|
   | `PUI.Web.MDC2`      | `"material-components-web": "^14.0.0"`   | `https://unpkg.com/material-components-web@14.0.0/dist/material-components-web.min.css`, `https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;700&display=swap`, `https://fonts.googleapis.com/icon?family=Material+Icons` | `Roboto, sans-serif` | `counter-mdc2` / `MDC2` |
   | `PUI.Web.MDC3`      | `"@material/web": "^2.5.0"`              | `https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;700&display=swap`, `https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined:opsz,wght,FILL,GRAD@20..48,100..700,0..1,-50..200&display=swap` | `Roboto, sans-serif` | `counter-mdc3` / `MDC3` |
   | `PUI.Web.Shoelace`  | `"@shoelace-style/shoelace": "^2.20.1"`  | `https://cdn.jsdelivr.net/npm/@shoelace-style/shoelace@2.20.1/cdn/themes/light.css` | `var(--sl-font-sans, sans-serif)` | `counter-shoelace` / `Shoelace` |
   | `PUI.Web.Fluent`    | `"@fluentui/web-components": "^3.0.2"`   | none                             | `'Segoe UI', system-ui, sans-serif` | `counter-fluent` / `Fluent` |
   | `PUI.Web.Bootstrap` | none                                     | `https://cdn.jsdelivr.net/npm/bootstrap@5.3.8/dist/css/bootstrap.min.css` | (Bootstrap's own) | `counter-bootstrap` / `Bootstrap` |
   | `PUI.Web.HTML`      | none                                     | the app's own CSS                | `system-ui, sans-serif` | `counter-html` / `HTML` |

2. **Name the app.** Three names, used throughout: `<app>` (kebab-case:
   directory and package name), `<Module>` (PascalCase: the view module;
   the view model module is `<Module>ViewModel`), `<entryFn>` (camelCase: the
   exported entry function, named after the app, never `main`).

3. **Write the scaffold files** from [Scaffold](#scaffold) into a fresh
   `<app>/`:

   ```
   <app>/.gitignore
   <app>/package.json
   <app>/packages.dhall
   <app>/spago.dhall
   <app>/entry.mjs
   <app>/public/index.html
   ```

4. **Write the starter app** — the counter twin from step 1's table,
   fetched from the tag and renamed. Write it even when the developer's
   app is already specified: a running counter proves the toolchain,
   and the real app replaces it afterwards, written to
   [writing.md](writing.md).

   ```sh
   cd <app> && mkdir -p src
   RAW=https://raw.githubusercontent.com/restaumatic/bambik/<tag>
   curl -sfL $RAW/demo/7guis/<twin>/Counter<Suffix>.purs \
     | sed -e 's/Counter<Suffix>/<Module>/g' -e 's/counter<Suffix>/<entryFn>/g' \
           -e 's/CounterLogic/<Module>ViewModel/g' > src/<Module>.purs
   curl -sfL $RAW/demo/7guis/counter/CounterLogic.purs \
     | sed 's/CounterLogic/<Module>ViewModel/g' > src/<Module>ViewModel.purs
   ```

   The result is the counter shown in [SKILL.md](SKILL.md) under your
   names (the other twins differ in their imports and heading word).

5. **Install and check the compiler:**

   ```sh
   npm install
   node_modules/.bin/purs --version   # must print 0.15.16 [development build ...]
   export PATH=$PWD/node_modules/.bin:$PATH
   ```

6. **Build:**

   ```sh
   spago build
   ```

   The first run fetches the package set, bambik and the variant fork
   (a minute or two). It ends with `Build succeeded.`; a warning listing
   unused dependencies is expected and harmless.

7. **Run it in dev mode and verify** — [building.md](building.md),
   *Run* and *Verify*. Bootstrapping is done when the verify check passes
   and the URL is reported.

## Scaffold

### .gitignore

```
node_modules/
output/
.spago/
generated-docs/
public/bundle.js
```

### package.json

Put the design system's `dependencies` entry from step 1's table in
place of `<design-system dependency>`; for Bootstrap and plain HTML
drop the `dependencies` block.

```json
{
  "name": "<app>",
  "private": true,
  "scripts": {
    "build": "spago build",
    "watch": "spago build -w",
    "dev": "esbuild entry.mjs --bundle --format=esm --outfile=public/bundle.js --servedir=public --serve=127.0.0.1:8000",
    "bundle": "spago build && esbuild entry.mjs --bundle --minify --format=esm --outfile=public/bundle.js",
    "docs": "spago docs --open"
  },
  "dependencies": {
    <design-system dependency>
  },
  "devDependencies": {
    "esbuild": "0.25.1",
    "purescript": "https://github.com/erykciepiela/purescript/releases/download/v0.15.16-variant.9/purescript-0.15.16-variant.9.tgz",
    "spago": "^0.21.0"
  }
}
```

Keep spago on 0.21 (the dhall-based line); the 0.9x rewrite uses
`spago.yaml` and does not read this scaffold.

### The library's dependency list

Both dhall files need the library's dependency list (spago does not
read a git package's own `spago.dhall`). Fetch it from the tag; never
type it from memory:

```sh
curl -sfL https://raw.githubusercontent.com/restaumatic/bambik/<tag>/spago.dhall \
  | sed -n '/^, dependencies/,/^  ]/p' | tail -n +2
```

It prints a valid dhall list (`[ "aff"` … `]`). Paste it verbatim in
the two places marked `<library dependency list>` below.

### packages.dhall

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
        <library dependency list>
    , repo = "https://github.com/restaumatic/bambik.git"
    , version = "<tag>"
    }
```

### spago.dhall

```dhall
{ name = "<app>"
, dependencies = [ "bambik" ] # <library dependency list>
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", ".spago/bambik/<tag>/extras/**/*.purs" ]
}
```

The second `sources` glob is required and carries the **same tag** as
`packages.dhall`. Because it compiles part of the library as the app's
own sources, the app lists the library's dependencies too. Add a
package to the `[ "bambik" ]` part when an app import needs one the
list lacks.

### entry.mjs

```js
import { <entryFn> } from './output/<Module>/index.js'
<entryFn>()
```

### public/index.html

The app mounts into `<body>` at runtime, so the body is empty. The page,
not the app, provides any surrounding surface or margin — style it
here, never by wrapping the app. Fill the slots from step 1's table
(one `<link>` per listed URL; none for Fluent):

```html
<!doctype html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title><App title></title>
    <!-- design-system links from step 1's table, one per URL: -->
    <link rel="stylesheet" href="<link URL>">
    <style>
      body { margin: 24px; font-family: <body font>; }
    </style>
    <script type="module" src="bundle.js"></script>
  </head>
  <body></body>
</html>
```

## Updating

To move to a newer bambik tag, change all three together:
`bambik.version` in `packages.dhall`, the tag in `spago.dhall`'s second
`sources` glob, and the pasted dependency lists (re-run the fetch
against the new tag). Check the new tag's own Pins table
(`https://raw.githubusercontent.com/restaumatic/bambik/<new tag>/.claude/skills/developing-bambik-apps/bootstrap.md`)
and move the compiler and variant-fork pins in the same edit if they
changed. Then `npm install` and `spago build`.

## Troubleshooting

- `Module Prim.Variant was not found` — stock purs is installed: check
  `package.json`'s `purescript` entry is the release URL, `npm install`,
  confirm with `purs --version`. While compiling `Data.Variant`, it
  means the `with variant.repo`/`with variant.version` lines are missing
  from `packages.dhall`.
- `Module Data.Profunctor.… was not found` (or `Data.Lens.…`,
  `Data.Variant.Case`) — the second `sources` glob is missing or names a
  different tag than `packages.dhall`. Make them equal; confirm
  `.spago/bambik/<tag>/extras/` exists.
- `Module PUI was not found` — `bambik` is missing from `spago.dhall`'s
  `dependencies`, or its fetch failed (check `.spago/bambik/<tag>/src/PUI.purs`).
- A missing module from some other library while compiling bambik, or
  spago asking to add packages to the dependency list — a pasted list is
  behind the tag; re-run the fetch and paste again.
- A dhall parse error — a list was edited by hand; paste the fetch
  output unchanged.
- Port 8000 busy — change `--serve=127.0.0.1:8000` in `package.json`
  and use the new port in building.md's checks.
