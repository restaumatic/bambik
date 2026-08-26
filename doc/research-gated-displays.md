# Research: fulfillment-gated displays

**Merged into main. Developed on branch `research-gated-displays` (off
the main line at the displays-and-sources doc); landed green in full —
clean build, tests, 102 bundles, the whole smoke suite — with two rungs
implemented and three demos converted.**

## The design under test

One display type across the assurance ladder:

```purescript
p { o | rest } { o | rest }
```

— a pass-through whose **release is the fulfillment witness**, with the
gate policy **baked into the component**. Feeding renders `o`; the whole
fed row is withheld until the component's policy deems the display
fulfilled, then released unchanged. The ladder becomes a family of gate
policies over one type; the dialog protocol ("open on feed, close on
emission") is revealed as the general case and the ambient readout as its
instant-open degenerate form.

Implemented rungs:

  * **instant** — `PUI.Web.HTML.shown @l f`: render field `l` through the
    formatter, release synchronously (render before release — the
    operand-order lesson, now baked into one body). Assurance by
    visibility.
  * **witness** — `PUI.Web.MDC2.confirmed cfg display`: the modal rung.
    Derived entirely from existing machinery — it *is* `simpleDialog`
    restricted to display-shaped content: the instant display inside
    renders and forwards the fed row; the confirm button's
    replay-last-value protocol releases it; `init`'s close-on-emission
    shuts the modal. **The ladder composes: witness rung = instant rung
    inside the modal.** A dismissal without confirming releases nothing —
    a declined reading withholds, honestly.

## What the three demos show

**counter-mdc2** — `headline4 (text @"count") # projection show
# completed` became `headline4 (shown @"count" show)`. One stage, no
adopter, no pass-through wrapper: **`tapped` dissolves at the call site**,
for real — the display carries the flow itself, so terminality (the
`× → 1` obstruction every previous attempt died on) never arises.

**auction-mdc2** — both `# tapped` stages gone, including the one inside
the `feedback` chain; the smoke's t=0 assertions pass unchanged (the
seeded wire feeds the display, which renders then releases — priming the
loop exactly as before).

**cashbox-mdc2** — the decisive one. The two `simpleDialog … # tapped`
detours became `confirmed cfg (body1 (shown @"amount" refundText))`, and
the smoke verifies the gate's business meaning live: *the balance is
untouched while the refund awaits confirmation* (withheld), *the
confirmed refund re-entered the flow as its business case and folded*
(released). L6's knowledge gate now spans **user knowledge**: downstream
of `confirmed`, the flow means "this row, as seen and acknowledged".
`subChoice` survives — it routes *which cases* need a witness — but the
dialog inside it is now a gated identity rather than a replayed tap.

## Costs, measured on real code

1. **Displays leave the merges.** The pass-through owns its whole open
   row, so it cannot sit beside an editor in a `RecordToRecord.do`. All
   three conversions restructured display merges into pipeline stages
   (inside the same ocular, so layout is unchanged) — and the glued
   `staticText`/`text` lines collapsed into **field-formatter functions**
   (`bidText`, `refundText`). This is the third display architecture in a
   row (events, gates) that pressures A11's per-field text nodes into
   line functions; only the `{}`-output display supports A11's merges.
   The pattern is now unmistakable: **A11's idiom and the unit-typed
   display are one design** — change either and the other follows.
2. **A blocking gate in the trunk blocks the loop.** `confirmed` on the
   `mvu` trunk would hold every turn; witnessing displays belong on
   branches (`subChoice` focus cases, `provided` panes) — exactly where
   cashbox already puts them. The existing starvation watchdog names a
   trunk mistake at runtime.
3. One incidental: the leaf name `shown` collided with local binders in
   `PUI.Web.HTML` (renamed to `fed`).

## Round two — the full sweep: `tapped` is deleted

All 157 remaining tap sites converted and the combinator removed from the
vocabulary (definition, export, re-export, laws rewritten against the
inline derivation `recordToRecord w identity`). The final gated family:

  * `shown @l f` — field, instant (HTML)
  * `told line` — narrow-row line, instant (HTML)
  * `shownAs proj content` — ambient structured content: the content
    (chrome merges, nested collections, adopted assemblies) registers at
    **build time**, renders per feed through the projection, releases the
    fed row always (HTML)
  * `shownWhen proj content` / `shownCase @l f content` — the pane rungs:
    content attached on relevance, row released always (HTML)
  * `shownEach @l proj item` — the keyed collection rung (HTML)
  * `confirmed cfg display` — the witness rung (MDC2/MDC3)

Two implementation lessons the sweep taught, both now baked in:

1. **Registration time is part of a rung's policy.** `shownAs` was first
   derived from `provided` (attach-on-first-feed) and three demos broke
   at t=0: potluck's gather-gated menu prefix vanished — the gate
   lawfully withholds the feed, and chrome that used to exist at build
   now waited for it. The ambient rung must register its content at
   build and gate only the *release*; the pane rungs must defer — the
   difference **is** the policy, not plumbing.
2. **A content slot must complete its content's wiring.** Leaves finish
   registration inside `fromUser`; a wrapper that never subscribes feeds
   a half-wired leaf. `shownAs` subscribes with the lawful `{}`-discard.

What was preserved, against expectation: **every A11 line survived.**
The pane and ambient rungs take structured content, so the glued
`staticText`/`text` merges converted as wrapper swaps — contents
untouched. No line functions were needed at all (the `told` collapse
prepared for never happened); the assurance ladder and A11 turned out to
be compatible once the ambient rung carried content rather than a
formatter.

## Standing of the three display architectures

| | unit-typed (`p {|r} {}`, main line) | event-consuming (`p [|e] {}`) | fulfillment-gated (`p {o|r} {o|r}`) |
| --- | --- | --- | --- |
| merges (A11 lines, editor-beside-readout) | native | impossible | impossible |
| pipeline stages | via `tapped` (derived, one word) | via emitter + `tapped` | **native — `tapped` deleted** |
| witnessed reading (dialogs) | separate protocol (`simpleDialog` + replay) | statuses | **the same type, stronger gate** |
| user-knowledge in the flow | no | no | **yes — L6 extended to the user** |
| re-display of a field | free (merges and taps) | one case per view | free in sequence |

The gated form is the strongest *pipeline* display and the unit form the
only *merge* display. They coexisted at this step — `shown`/`confirmed`
beside `text`/`tapped` — and the honest conclusion so far is that this
coexistence may be the design: merges keep the unit-typed display (A11's
per-field lines), pipelines take the gated one (no tap, witnesses typed),
and the ladder's upper rungs exist only in gated form because a witness
*is* a gate.
