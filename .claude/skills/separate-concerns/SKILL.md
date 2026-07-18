---
name: separate-concerns
description: Reorganize a bambik demo's functions into exactly two classes — UI wiring inlined into main, pure business functions standalone. Use when a demo has glue functions mixing variant dispatch or event constructors with model logic.
---

# Separation of concerns in demos

Reorganize a demo `Main.purs` (by inlining and extracting) until every
function belongs to exactly one of two classes:

1. **UI wiring** — lives inline in `main` (or is unavoidably standalone
   like a `Model -> Array Markup` render function). Anything that mentions
   PUI types, variants-as-events, `Markup`, DOM wiring.
2. **Pure business** — standalone functions over the model and plain data:
   `Model -> Model`, `Model -> String`, parsers, evaluators. No variant
   types, no PUI types, no UI vocabulary in their signatures.

**File order**: one purely UI-related function — `main` — comes first,
followed by the pure business functions over the model. (This structure is
also stated in the 7guis pages' code-style note; keep the two in sync.)

## What to inline (delete the named glue)

- **Update dispatchers** — a `handle :: [ cases ] -> Model -> Model` that
  merely `match`es cases becomes an inline dispatch at the update stage:

  ```purescript
  # updates (match { cellClicked: selectCell, undo: undo, ... })
  ```

  Each case's body is extracted first (see below). `match { ... }` applied
  point-free curries correctly: `updates` wants `e -> Model -> Model` and
  `match { c: f }` gives exactly that when each handler `f` is
  `payload -> Model -> Model`.

- **Event constructors** — a `clickedCell :: String -> [ cellClicked :: String ]`
  wrapper becomes the variant sugar applied inline at the wire:

  ```purescript
  (\node emit -> onKeyClick node \key -> emit (.cellClicked key))
  ```

## What to extract (name the business)

Each case lambda inside the old dispatcher becomes a standalone pure
function named for the business action, payload first, model last:

```purescript
selectCell :: String -> Model -> Model
selectCell key m = m { selected = Just key, formula = fromMaybe "" (lookup key m.cells) }
```

Existing `Model -> Model` functions (`commit`, `applyDiameter`, …) already
belong to the business class — leave them standalone.

## Type-inference gotchas (both hit in practice)

- **Inline variant sugar needs a closed-row annotation.** A named
  constructor wrapper pinned the row via its signature; inlined, the sugar
  is open and the merge's `Nub` fails. Annotate at the use site:

  ```purescript
  emit (.clicked { x, y } :: [ clicked :: { x :: Number, y :: Number } ])
  # rmap (\e -> .picked e.key :: [ picked :: Int ])
  ```

- **Ignored button payloads still pin rows.** A `button # asCase @l`
  emission's payload row is inferred *from the handler*. `const f` leaves
  it free and the whole merge becomes ambiguous (the error surfaces at a
  sibling stage). Dispatch by applying the business function to the payload
  snapshot instead — it is the same model value:

  ```purescript
  # updates (match { create: \m _ -> createPerson m, ... })
  ```

## Boundary cases

- `Model -> Array Markup` render functions are UI but too large to inline —
  they stay standalone; that is fine (they are *purely* UI-related).
- `Model -> String` caption/validation formatters are pure business — keep.
- **A `forall click. click -> Model -> Model` handler is a smell**: the
  phantom payload parameter is UI (the event) smuggled into an otherwise
  pure business function. Strip it — the business function is
  `Model -> Model` — and absorb the event in the inline dispatch. Note the
  bare (un-`asCase`d) button emits the canonical variant `[ event :: _ ]`,
  so the dispatch is a one-case match applying the business function to the
  payload snapshot (which also pins the button's row):

  ```purescript
  button { label: "Count" } # updates (match { event: \m _ -> increment m })

  increment :: { count :: Int } -> { count :: Int }
  increment r = { count: r.count + 1 }
  ```

## After reorganizing

1. Clean up imports the deleted glue no longer needs (compile shows them).
2. Follow the demo code style: no comments, 100% explicit imports
   (including Prelude — add/remove names the change touched), UI leads with
   `$`, data plumbing trails with `#`.
3. Verify: `export PATH=$PWD/node_modules/.bin:$PATH`, then
   `spago build --path "demo/<d>/**/*.purs"`, bundle with
   `spago bundle-app --minify --main Main --to demo/<d>/bundle.js --path "demo/<d>/**/*.purs"`,
   run the demo's CDP suite from the scratchpad harness, commit to main,
   `npm run deploy-demo-7guis` (or the matching deploy script).
