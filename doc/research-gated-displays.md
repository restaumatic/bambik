# Research: fulfillment-gated displays

**Branch: `research-gated-displays` (off the main line at the
displays-and-sources doc). Status: green in full — clean build (the 3
pre-existing warnings), tests, 102 bundles, the whole smoke suite — with
two rungs implemented and three demos converted.**

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

## Standing of the three display architectures

| | unit-typed (`p {|r} {}`, main line) | event-consuming (`p [|e] {}`) | fulfillment-gated (`p {o|r} {o|r}`) |
| --- | --- | --- | --- |
| merges (A11 lines, editor-beside-readout) | native | impossible | impossible |
| pipeline stages | via `tapped` (derived, one word) | via emitter + `tapped` | **native — `tapped` dissolves** |
| witnessed reading (dialogs) | separate protocol (`simpleDialog` + replay) | statuses | **the same type, stronger gate** |
| user-knowledge in the flow | no | no | **yes — L6 extended to the user** |
| re-display of a field | free (merges and taps) | one case per view | free in sequence |

The gated form is the strongest *pipeline* display and the unit form the
only *merge* display. They coexist on this branch — `shown`/`confirmed`
beside `text`/`tapped` — and the honest conclusion so far is that this
coexistence may be the design: merges keep the unit-typed display (A11's
per-field lines), pipelines take the gated one (no tap, witnesses typed),
and the ladder's upper rungs exist only in gated form because a witness
*is* a gate.
