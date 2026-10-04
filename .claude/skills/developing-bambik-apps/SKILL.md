---
name: developing-bambik-apps
description: Use when creating, editing, reviewing, building or running a bambik / PUI (Profunctor User Interfaces) PureScript web app — an app whose view composes components from PUI.Web.HTML, PUI.Web.MDC2, PUI.Web.MDC3, PUI.Web.Shoelace, PUI.Web.Fluent or PUI.Web.Bootstrap. Covers scaffolding a new app with the forked compiler and the bambik spago package, the view/view-model module split and the code style for application code, and the dev-mode run-and-verify loop.
---

# Developing bambik applications

A bambik app is a view module and a view model module. The smallest one with
a model, the counter (MDC2):

```purescript
counterMDC2 :: Effect Unit
counterMDC2 =
  body $
    ( Semigroupoid.do
      headline4 (text countLine) # shown
      button @"Count" {}
      fold @"Count" increment
    ) # mvu @( count :: Int ) freshCount
```

```purescript
freshCount :: { count :: Int }
freshCount = { count: 0 }

countLine :: { count :: Int } -> String
countLine { count } = show count

increment :: { count :: Int } -> { count :: Int }
increment m = m { count = m.count + 1 }
```

Read top to bottom, the view is the screen: a heading showing
`countLine` of the model, then a `Count` button that applies
`increment`, the whole over the model `{ count :: Int }` started at
`freshCount`. The view names design
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
   propagate*, *Finish by running it*, *Looking things up*.
   Companions, stating no rules of their own:
   [walkthrough.md](walkthrough.md) (flight-booker read line by line —
   read it after the counter) and [vocabulary.md](vocabulary.md) (from
   what the screen needs to the word and the demo that uses it).
3. **[building.md](building.md)** — run dev mode, verify in a headless
   browser, bundle for deploy, open the API reference.

**Every task ends in dev mode**: the app running, the check in
[building.md](building.md#verify) passing, and its URL reported to the
developer. A green build is not a finished task — a pane waiting for a
missing value is invisible to the compiler and obvious on screen.

## Worked examples

Spago fetches the library whole, so after the first build
`.spago/bambik/<tag>/` holds its module headers under `src/` (the API
reference) and its demos under `demo/7guis/` and `demo/nguis/`. A demo
directory's suffix names its design system (`counter-mdc2`,
`counter-html`, …); twins share the view model module in the unsuffixed
directory. Switching an app's design system is its vocabulary import
plus the page's links ([bootstrap.md](bootstrap.md), step 1).
