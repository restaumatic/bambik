# Experiment: demos without nominal types — ad-hoc rows only

Every `type` synonym and (almost) every `data` declaration was removed from all
demos (demo/1, demo/7guis, demo/nguis). Synonyms were inlined at every use
site, transitively; sum types became anonymous variant rows via the fork's
sugar (`[ oneWay :: Unit, return :: Unit ]`, `.oneWay unit`,
`Data.Variant.match`). Behavior is unchanged: full build, `spago test`, all
bundles, and the whole smoke suite pass.

## What survived

Exactly one declaration in ~3,000 demo lines:

```purescript
data Expr                    -- cells' formula AST
  = Lit Number | Ref String | Range String String | App String (Array Expr) | Bin Char Expr Expr
```

`Expr` is directly recursive, and rows cannot express μ — a hard structural
boundary, not a style choice. Everything else went, including
markdown-previewer's `Block`/`Inline` (they looked recursive but aren't:
`Block` → `Inline` → nothing, no cycle).

## Source size

| metric | before | after |
|---|---|---|
| total demo LOC | 3,053 | 2,913 (−4.6%) |
| total chars | 118,984 | 142,860 (+20.1%) |
| non-whitespace chars | 92,108 | 110,769 (+20.3%) |
| diff | — | 30 files, +444/−583 |
| longest line | 302 ch | 735 ch |
| p99 line width | 160 ch | 282 ch |
| p95 line width | 103 ch | 133 ch |
| lines > 120 ch | 89 | 196 |

Line count *drops* (every declaration block vanished) while the code *grows a
fifth in characters*: the type text didn't disappear, it moved from one
declaration into every signature that used the name, multiplied by the
use-site count. Chars, not lines, are the honest size metric here. The distribution per file is bimodal:

- shrinks when the type had few consumers: movie-browser −28, weather −19,
  quiz/checkout −15, todomvc −14
- grows when a big row has many consumers: **demo/1 +57** — the 22-line
  `Order` row is now pasted verbatim into 4 signatures, ~88 lines of repeated
  type; stopwatch +8, markdown-previewer +8, reorder +4

Rule of thumb that fell out of the data: cost ≈ row size × use-site count.
Below roughly 3 uses × 3 fields the synonym was pure ceremony; above it, it
was earning its keep.

## Type inference

**Zero annotations had to be added in the entire conversion, and zero
inference failures occurred.** Every file compiled on the first attempt after
mechanical substitution. PureScript synonyms are transparent, so removal is
semantically free; and the demos' business functions were already fully
annotated at top level, so inference never had to reconstruct a row from
scratch. Variant construction (`.all unit` open injections) closed against
`mvu` seeds' concrete signatures without help; `select`/`segmentedButton`'s
`Eq` constraints resolved through Variant's structural `Eq` over `Unit`
payloads, so every `derive instance Eq` was simply deleted.

The two *pre-existing* annotations that inference genuinely requires are where
the style hurts most, because they live **inside expressions**, where a long
row can't be formatted away:

- circle-drawer's `foreach` lambda: `\(m :: Canvas) ->` became a ~280-char
  in-expression annotation on the densest line of the file
- inbox's mid-pipeline echo wire: `(identity :: PUI Web Mailbox Mailbox)`
  became ~330 chars, the row spelled twice

Signatures can go multi-line; in-expression annotations can't, and they are
exactly the positions the row layer sometimes needs pinned.

## Type signatures

Representative growth (checkout):

```purescript
atCart :: Step -> Maybe Step
-- became
atCart :: { item :: String, address :: String, card :: String, confirmation :: String, step :: String }
       -> Maybe { item :: String, address :: String, card :: String, confirmation :: String, step :: String }
```

repeated near-identically for 11 helpers. Nesting compounds transitively:
shopping-cart's `addUnit :: Product -> Cart -> Cart` (28 ch) became 250 ch
with `Product` inlined twice inside `Cart`; markdown-previewer's
`blocks :: Array String -> Array Block` became a 6-line type in which the
`Inline` row appears four times. The update-helper idiom `X -> Model -> Model`
— the backbone of every MVU demo — is the worst-hit shape, since `Model`
appears twice per signature and the helpers come in packs of 5–11.

Two genuine signature *improvements*: single-field models
(`Queue = { serving :: Int }`, `Auction = { bid :: Number }`) read better
inline — ticket-dispenser's `unfolding` signature now shows both trace
channels concretely; and quiz's `{ question :: Int, correct :: Int }` says
more than `QuizRun` did.

## Readability

Better or neutral (small/flat models): auction, ticket-dispenser,
tic-tac-toe, photo-gallery, tip-calculator, color-mixer, payment,
restaurant-menu (its synonyms were pure documentation — the widget was
already structurally typed).

Clearly worse (large or nested or many-consumer models): demo/1, inbox,
checkout, crud, stopwatch, calculator, password-generator, shopping-cart,
weather, markdown-previewer's parser half. Three distinct losses:

1. **Documentation-of-role.** `Canvas`, `PeopleCatalogue`, `Bill` named what a
   row *is for*; checkout's `Order` vs `Step` distinguished model from
   model+loop-state in the `folding @"next"` wizard — now you diff two 4- vs
   5-field literals to find the looped field. Aff actions no longer say what
   they load (`loadOrder :: {22 lines} -> Aff ...`).
2. **Single point of change.** Adding a field to a model now means editing
   every signature in lockstep — ~10 sites per file in crud/stopwatch/inbox,
   7 in password-generator, 4×22 lines in demo/1.
3. **Sameness.** Structurally equal rows with different roles become
   indistinguishable (payment's model vs its retry row).

Genuine wins on the variant side: enum ADTs convert beautifully.
`data Visibility = All | Active | Completed` + `derive instance Eq` became a
row that needs no derive, whose labels feed `match` records
(`display = match { numV: formatNum, textV: \t -> t, errV: \e -> e }` is
tighter than three pattern equations), and whose construction `.all unit`
reads fine (though there's no niladic sugar — the `unit` payload is
mandatory). Multi-argument constructors force named payload records
(`Heading Int (Array Inline)` → `.heading { level, inlines }`) — bulk in the
type, but a readability *gain* at construction sites.

One semantic regression: the fork's variant *pattern* sugar requires a `_`
catch-all, so cells' `numAt` — previously compiler-checked exhaustive over 4
constructors — gained a dead `_ -> Left "#REF!"` branch that would silently
absorb a future case. `Data.Variant.match` stays total and avoids this, but
doesn't do multi-scrutinee or literal-payload patterns.

## Developer experience

- Conversion itself is risk-free: zero compile errors across 30 files, no
  row-layer or variant-sugar friction. The library's structural spine
  (merges, `mvu`, `edits`, `feedback`, `foreach`) never referenced the names
  anyway — **the UI-pipeline code was untouched in every file**; nominal types
  lived exclusively in the business layer.
- Error messages and IDE hovers now print full rows instead of names — fine at
  3 fields, walls at demo/1 scale.
- Refactoring cost inverts: renaming a *type* becomes free (there is none),
  renaming a *field* touches every pasted copy.

## Verdict

Nominal types in bambik demos are pure business-layer documentation — the
profunctor/row machinery is already 100% structural and doesn't miss them.
Removing them costs nothing in inference or compile-time and −4.6% in line
count, but pays for it in signature width (p99 +76%), duplicated
single-point-of-change, and lost role names. The data supports a split
convention rather than either extreme, which is what CLAUDE.md already
prescribes: **structural as far as readable — anonymous rows for small/
single-use shapes, a named alias only for the top aggregate** (and `data`
where there's recursion). The experiment confirms the existing convention is
at the optimum; the all-ad-hoc extreme is viable but strictly worse for the
larger demos.

## Follow-up audit: unnecessary row members after inlining

A second pass checked every inlined signature for row members the function
body never needs. Headline: **inlining created no new slack — but it exposed
a lot of pre-existing slack.** At the parent commit every flagged helper was
typed `Model -> Model` / `Model -> String` behind a synonym naming the full
model; demanding the whole model cost one cheap word, so nobody noticed.
Ad-hoc rows re-price that habit per field per paste site: a large share of
the signature-width explosion measured above is *over-demand*, not necessary
type text.

### Where the exact row is genuinely forced

Consistent across all 30 files: `mvu`/`with` seed literals, `announce`/
`seeded` primers, constructed record literals (loaders like demo/1's
`loadOrder`, state-bracket constructors like `fulfillmentState`), merge
operands and `forCase` positions, and inbox's mid-pipeline
`identity :: PUI Web {model} {model}` type anchor. Everything else —
projections under `lcmap`/`projection`, `updates`/`match` handlers,
`provided` panes, `every` steps — only has to *unify* with the
pipeline-pinned model, and open rows (`forall r. { used :: T | r } -> ...`)
unify fine. The pipeline pins the concrete model at exactly one place (the
merge or the seed); helpers never need to repeat it.

### Magnitude of the slack

Dead weight concentrates in the "pack of small helpers over one model" files:

| file | share of helper-signature row text that is carried, never read |
|---|---|
| checkout | ~70% (9 of 11 helpers read only `step` + at most one field) |
| inbox, weather | ~two-thirds (most helpers read 1–2 of 4–6 leaf fields) |
| calculator | 60–80% in the read helpers (`pressKey` alone earns the full row) |
| stopwatch | ~50% |
| demo/1 | of 4 full-Order pastes only `loadOrder` earns it; `printReceipt` needs 2 of 7 top fields, `submitOrder` 3, `summarize` 5 |
| crud | ~17 of 45 member occurrences carried; plus a dead `surname` in the `Entry` row that flows into `listOf` unread |
| signup-form | ~40% — `plan`/`country` are edited and seeded but never consulted by any business function |
| tip-calculator | 6 of 8 row-typed values over-demand (only `perPersonLine` and the seed need the whole `Bill`) |

Clean (no or trivial slack): temperature-converter, shopping-cart, reorder,
tic-tac-toe, markdown-previewer, photo-gallery, restaurant-menu, and
near-clean auction, ticket-dispenser, payment, color-mixer, cells.
**No unused variant cases exist anywhere** — every case of every converted
variant is both constructed and consumed.

Extreme cases: quiz's `restart` and inbox's `emptiedNote` declare the full
model and ignore their argument entirely (`forall a. a -> ...` would do).
Incidental finds: `demo/7guis/flight-booker/Business.purs` is imported by
nothing (orphaned module); circle-drawer's `undo`/`redo` don't restore
`diameter` — visible in the audit precisely because the field sits unused in
their signatures.

### What this does to the verdict

The width explosion blamed on ad-hoc rows above is really two costs
superimposed: necessary row text (the model spelled where it's pinned) and
over-demand (full model where 1–3 fields suffice). The second dominates in
the worst files. The synonym style *hid* the over-demand; the ad-hoc style
*charges* for it — and, unlike the synonym style, makes the honest narrow
signature expressible per helper. A disciplined ad-hoc variant of the demos
(open rows for helpers, exact rows only at seeds/literals/anchors) would be
substantially less verbose than this branch, at the cost of `forall r.`
noise and weaker "this is the model" documentation. The optimum remains the
existing convention, but with one refinement the audit argues for: helper
signatures need not name the full model even when a synonym exists — the
full-model `Model -> Model` habit is a documentation choice, not a
requirement of the row machinery.
