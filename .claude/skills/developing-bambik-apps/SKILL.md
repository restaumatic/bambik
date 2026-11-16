---
name: developing-bambik-apps
description: Use when creating, editing, reviewing, building or running a bambik / PUI (Profunctor User Interfaces) PureScript web app — an app whose view composes components from PUI.Web.HTML, PUI.Web.MDC2, PUI.Web.MDC3, PUI.Web.Shoelace, PUI.Web.Fluent or PUI.Web.Bootstrap. Covers scaffolding a new app with the forked compiler and the bambik spago package, the view/view-model module split and the code style for application code, and the dev-mode run-and-verify loop.
---

# Developing bambik applications

A bambik app is a view module and a view model module. The smallest one with
a model, the counter (MDC2):

```purescript
counter :: Effect Unit
counter =
  body $ Semigroupoid.do
    headline4 $ text countLine
    ( Semigroupoid.do
      button @"Count"
      fold @"Count" increment ) # stepped
  # looped # with freshCount
```

```purescript
type Count = { counted :: Int }

freshCount :: Count
freshCount = { counted: 0 }

countLine :: Count -> String
countLine { counted } = show counted

increment :: Count -> Count
increment m = m { counted = m.counted + 1 }
```

Read top to bottom, the view is the screen: a heading showing
`countLine` of the model, then a `Count` button whose press `increment`
folds silently (the heading already shows the count) — the button and
its fold one line of the body, closed by `# stepped` — the whole over
the model `{ counted :: Int }` started at `freshCount`. The view names design
system words and view model values; the view model module is plain
functions typed as the view reported them, unit-testable, importing no
UI, its signatures read off the view (writing.md *Writing order*). What the screen reads is a
copy function (`countLine`), taken by the display at the leaf.

## Procedures

1. **[bootstrap.md](bootstrap.md)** — scaffold a new app: design
   system choice (ask the developer), the pinned toolchain, the scaffold
   files, the counter as starter.
2. **[writing.md](writing.md)** — write the two modules. Its sections:
   *Terms* (the only vocabulary this skill uses), *The pipeline*,
   *Components*, *Stages*, *App shape*, *Conditional visibility*,
   *Modals*, *Collections*, *View module and view model module*, **Code style** (the
   contract for application code: *Layout*, *Types and values*,
   *Business functions*, *Wiring*), *Writing order* (view first; its holes list the
   view model's signatures), *What the laws guarantee*, *When it does not
   compile*, *When it does not propagate*, *Finish by running it*, *Looking things up*.
3. **[building.md](building.md)** — run dev mode, verify in a headless
   browser, bundle for deploy, open the API reference.

**Every task ends in dev mode**: the app running, the check in
[building.md](building.md#verify) passing, and its URL reported to the
developer. A green build is not a finished task — a pane waiting for a
missing value is invisible to the compiler and obvious on screen.

## Worked examples

Spago fetches the library whole, so after the first build
`.spago/bambik/<tag>/` holds its module headers under `src/` (the API
reference) and its demos under `demo/7guis/` and `demo/nguis/`. A view
module is named by its design system (`MDC2.purs`, `HTML.purs`, …,
the modules `Counter.MDC2`, `Counter.HTML`) and twins share one view
model module, the app itself (`Counter.purs`), all in the app's one
directory (`demo/7guis/counter/`). Switching an app's design system is its vocabulary import,
the words that catalogue names differently (counter's heading is
`headline4` in MDC2, `headlineLarge` in MDC3, `title3` in Fluent) and
the page's links ([bootstrap.md](bootstrap.md), step 1).
