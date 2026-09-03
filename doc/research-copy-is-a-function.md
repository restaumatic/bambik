# Research: copy is a function, not a field

Status: DECIDED 2026-09-02. Partially reverses the *display* half of
doc/research-presentation-model.md (2026-08-31), keeping its
testability motivation and its `settled` half intact.

## The question that forced it

Read a swept view line by line and ask *where is this value
computed?*

```purescript
( RecordToRecord.do
    progressBar @"fraction"
    p (text @"progressLine") ) # shown
sliderLive @"Duration" {}
every tickPeriod tick
button @"Reset" {} # with nothingElapsed # updated (match { "Reset": const })
) # settled presentTimer # mvu tenSecondFreshTimer
```

Nothing at the read site names a writer. `"Duration"` and `elapsed` are
source fields; `fraction` and `progressLine` are derived by
`presentTimer`, which is typed `model -> model` — so its signature
cannot say which is which, the seed must carry placeholder values for
the derived fields, and a second writer of `progressLine` would compose
silently. Provenance rested on one convention (*one `present<App>` per
app*) and one naming habit (`*Line`).

## What the census says

One variant per family (40 families; twins share their view), **109
display read sites**, classified by what the read field actually is:

| The site reads | Sites |
| --- | --- |
| a String **manufactured for display** — `present<App>`, a classifier case payload, a collection/row projection | ~85 |
| a genuinely-String **domain** field (name, title, subject, prose) | ~12 |
| a **Number** read by a quantity leaf (`progressBar`, `linearProgress`, `gauge`, `ratingDisplay`) | ~12 |

Read-once is essentially universal: across all 40 families only two
fields are read twice.

Two conclusions follow.

1. **`text @l` reading a model field is the exception, not the rule.**
   It is honest only where the domain value is *already* copy — and
   those ~12 sites are all name-like strings. Everywhere else the field
   exists **because a display needed a String**: cells' `text` is
   `colName c`/`show r`, tic-tac-toe's `mark` is `markText` over
   `[ x, o, free ]`, restaurant-menu's `priceLine` rewrites a
   `price :: String`, shopping-cart shows an `Int` quantity through a
   line. The field route forces every non-String value through a
   manufactured String sibling; that is why the 2026-09-01 sweep
   created ~60 `*Line` fields and why `present<App>` reached five
   entries in tip-calculator and weather.
2. **Quantity displays looked like the honest label-indexed ones** —
   `progressBar @"fraction"` reads a `Number`, nothing to format — but
   that reading was wrong, and the demos proved it: after the first
   sweep, *every* surviving `present<App>` existed to write exactly
   such a field (`fraction`, `progress`, `caffeine`, `interestShare`),
   each a function of source fields with one writer and one reader.
   Formatting and **derivation** are the same act: `fraction = elapsed
   / duration.current` is no more state than the sentence beside it,
   and `progressBar` no more renders it verbatim than `text` renders a
   line verbatim — it renders `width: 42%`. So quantity displays take a
   read function too, and their label survives as the accessible name
   only.

## Principle

**A display takes a read function. Full stop.**

- A leaf whose content **is copy** (`text`) takes the read function and
  no label: `text progressLineOf`, typed `p { | reads } {}` with the
  footprint stated by the function's own signature. The function is
  named at the point of use, so the read site answers *where is this
  computed* by itself.
- A leaf that renders a **number** as a bar, gauge or stars takes the
  read function *and* keeps its label as the **accessible name**:
  `progressBar @"Elapsed" elapsedFraction`. The label is copy there,
  like an editor's caption — not a field reference.
- Quantity **editors** are untouched: a slider genuinely edits a field,
  so `sliderLive @"Duration"` keeps label-as-field. The line is
  *display vs. editor*, not *string vs. number*.

Formatting stays in the logic module — as **bare-value functions**
(`{ reads } -> String`) rather than field-writing normalizers. The
testability motivation of the presentation model is therefore kept in
full and sharpened: `progressLineOf { "Duration": …, elapsed: 3.0 } ==
"3.0s / 10.0s"` is narrower than a whole-model record assertion, and
needs no idempotence law.

`settled` keeps its honest job: **invariants among edited fields**,
where two writers are inherent and type preservation is the point
(temperature-converter's `°C`/`°F`, meeting-booker's `seatsInRoom`,
order-form's `staleDistanceForgotten`). It is no longer the
presentation backbone.

## Laws

- **Copy-is-a-function law**: `text f` renders `f` of the fed row; the
  row is read narrow (`Union reads rest big`), the function is a
  **named logic function**, never a lambda and never a business literal
  at the view site (writing.md's existing rule, now the only home for
  formatting).
- **Quantity-verbatim law**: a quantity leaf never brackets a
  formatter — its field arrives ready to render, because a number needs
  no rendering.
- **Stamp invariant**: unchanged for every labelled leaf. `text` takes
  no label and therefore no stamp; under host diagnostics it plants a
  `text` comment marker with no label (the marker keeps its
  smoke-harness role).

## Vocabulary changes

| Change | Why |
| --- | --- |
| `text :: ({ \| reads } -> String) -> PUI Web { \| row } {}` (subsuming, label-free) | copy is a function of the row |
| `text @l` (label-indexed verbatim read) DELETED | ~85 of 109 sites were manufacturing a String for it; the remaining ~12 read `_.title`-style accessors, which say the same thing in the same space |
| `forProperty` on `text` no longer needed | a context-pinned row is read by the function itself (`_.title`) |
| quantity leaves (`progressBar`, `linearProgress`, `determinateLinearProgress`, `gauge`, `ratingDisplay`, `badge`) unchanged | label = field is honest there |
| quantity leaves (`progressBar`/`linearProgress`/`progress`/`gauge`/`ratingDisplay`) take `@l` **plus** a read function | a fraction is derived; the label is the accessible name |
| `present<App>` disappears | every one of them existed to feed a display; `settled`'s remaining job is invariants among edited fields, and in the demos every surviving `# settled` sits on an editor |
| seed placeholders for derived fields disappear | a derived line is not model state |

## Migration & verification

1. Library: `text` retyped; header rewritten; `PUI.Web.HTML` export
   unchanged in name.
2. Demos: per-family sweep — display fields whose only reader is a
   `text` become bare-value functions in the logic module; the fields,
   their `present<App>` entries and their seed placeholders go.
   Classifier payloads and collection item rows keep the *source*
   fields they carry and lose their line fields.
3. Mechanical guard: `check-view-model` keeps its Maybe/Boolean rules
   and gains one — no lambda in a `text` argument.
4. Docs (L-sync): writing.md, vocabulary.md, walkthrough.md, CLAUDE.md,
   demo/index.html `#code-style`.
5. L15 in full.

## Admission test

Derivation: `text f` is `lcmap` at the leaf — the same `lcmap`-only
read the deleted `projection` was, minus the label it could not
justify. Laws: the two above. Subsumption: stated (`Union reads rest
big`), so a call site never coerces. Honesty: the read site names its
writer; a display no longer pretends its copy is model state.
Reachability: every surviving word demo-reached. Green stack: step 5.
Sync: step 4.
