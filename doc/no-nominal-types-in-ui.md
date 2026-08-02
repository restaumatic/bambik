# Design rule: no nominal types in UI

Bambik rejects nominal types in UI — in the library's UI-facing vocabulary and
in every application built on it.

A view-model type — the model a widget displays, the payload a pane consumes,
the case an event raises, an entry a collection reconciles, a config a
component takes — is specific to its one UI. It is a one-off, never reused, so
it earns no name and no declaration. UI code declares no `data`, no `newtype`,
no `type` synonyms: every view-model type is an anonymous structural type
written in place —

- **Record rows** for all-at-once (`{ start :: String, return :: String }`)
- **Variant rows** for one-at-a-time, via the fork's sugar
  (`[ oneWay :: {}, return :: {} ]`, constructed `.oneWay {}`)
- **`{}`** for unit payloads (never `Unit`)
- primitives (`String`, `Number`, `Int`, `Boolean`) at the leaves
- the generic containers `Array` and `Maybe` over all of the above

## Rationale

Nominal types bloat UI code, reduce flexibility, and impose rigidity:

1. **A name is indirection.** Reading a widget's type means chasing a
   declaration elsewhere; an anonymous row is read whole at the use site.
   `{ value :: Maybe [ oneWay :: {}, return :: {} ] }` says everything
   `Maybe FlightType` deferred.
2. **A declaration is coupling.** Two widgets sharing a named type are forced
   to evolve together; rows keep every widget free to change alone, and the
   row layer's subsumption (`updates`/`tapped`/`displayed`/`edits`/`acted`/
   `completed` read narrow) means two stages agree only on the fields they
   actually share.
3. **The ceremony buys nothing for one-off types.** A `data` enum needs a
   declaration, imports at every consumer, and `derive instance Eq`; the
   equivalent variant row needs none of it — structural `Eq` is free, the
   labels feed `Data.Variant.match` records directly, and the type-changing
   selectors (`select`, `radioButton`, `segmentedButton`) consume it as-is.

## What the library guarantees

The vocabulary never mentions an application-defined nominal type and never
forces one into existence:

- Components speak **canonical rows** (`{ value :: String }`,
  `[ clicked :: r ]`, `[ event :: String ]`), adopted per business label with
  `asField`/`asCase`/`forCase` — labels, not declarations.
- Selector **options carry variant-row values**, never strings-as-enums:
  `select`/`radioButton`/`segmentedButton`/`tabBar` options read
  `{ value: .oat {}, label: "Oat" }`, so consumers branch by total `match`
  instead of string comparison with a silent catch-all. The `labeled` helper
  (self-labeled string options) was removed — it existed only to feed the
  antipattern. Strings remain right where the values are *data* flowing from
  a catalogue (product names, collection keys) or a token alphabet a parser
  consumes (calculator keys, cells' formula language).
- Configs are **anonymous records** (`{ floatingLabel }`, `{ min, max, step }`,
  `{ caption }`).
- Durations are **`{ ms :: Number }`** — `every`, `debounced'`, `resolveFor`,
  `inputDebounced`, `debouncedTextField` — not `Milliseconds`, so no demo
  imports a duration constructor.
- Unit payloads are **`{}`**: `Default` deliberately has no `Unit` instance
  (the empty record instance serves), and `attrDyn`/`clDyn` signal presence
  with `Maybe {}`.
- `Maybe` stays: it is the presence-of-data container (`provided`, the
  unselected state of type-changing selectors, `every`'s pause), maximally
  generic and declared by nobody. The rule rejects *declaring* domain types,
  not the two ubiquitous parametric containers (`Array`, `Maybe`) the
  vocabulary is generic over — both are containers in the library's own
  algebra (`Acting`'s `Array` action; `optioned` derives the `Maybe = 1 + a`
  action from it).

## The boundary: UI, not business

The rule is about UI. Business logic below the UI may name its types:

- **Recursion demands a name.** Rows are the μ-free fragment of the container
  grammar; a directly recursive type (cells' formula AST `data Expr`) cannot
  be a row. This is a hard structural boundary, not a style choice.
- **Ecosystem APIs keep theirs.** `Aff`, `Either` in parsers/validators,
  `Milliseconds` in `Aff.delay` inside business actions.

A nominal type still never appears in a widget's type: it enters the UI only
through business functions that project it into rows (`parse :: {...} ->
Either String [ oneWayOn :: ..., returnBetween :: ... ]` — the `Either`
consumed by business dispatch, the rows fed to widgets).

## Costs, and what answers them

doc/experiment-ad-hoc-rows.md measured the conversion of all demos: lines
−4.6%, characters +20%, p99 line width +76%; the losses were role-naming,
single-point-of-change, and signature width. Its verdict at the time
recommended a split convention (named alias for the top aggregate). This rule
supersedes that verdict — the rejection is adopted wholesale — because the
follow-up audit showed the answers were already in the design:

1. **Read-narrow subsumption.** Most of the measured width was over-demand:
   helpers typed against the whole model out of habit. The stages that read a
   row subsume, so each business function states its **exact footprint as a
   closed narrow row** — `selectCell :: String -> { selected :: Maybe String,
   formula :: String } -> ...`, not the 31×27 sheet.
2. **The pipeline pins the model once.** Only the seed literal
   (`mvu`/`with`/`announce`/`seeded`) and merge operands need the concrete
   full row; everything else unifies with it.
3. **Role names move to values.** The seed's *value name* carries what the
   type alias used to say, in business language: `mvu plannedTrip`,
   `mvu tenSecondFreshTimer`, `with emptyCanvas` — and business function
   names (`bookingProblem`, `returnLeg`) carry the rest.

## Enforcement

- The demos are the executable form of the rule: exactly one type declaration
  survives in ~3,000 demo lines (cells' recursive `Expr`, business).
- The library refuses the easy outs: no `Default Unit`, no `Milliseconds` in
  any widget signature.
- The rule is *stated* once, as part of the application code-style contract in
  [writing.md](../.claude/skills/developing-bambik-apps/writing.md) (*Code
  style → Types and values*); this note is the argument behind it, not a second
  statement of it. The demo pages' code-style footer restates it for readers who
  never open the skill — the one deliberate copy.
