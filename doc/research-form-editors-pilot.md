# Pilot: the seedless form editor (experiment/form-editors, 2026-10-17)

Merged to main the same day, the consequences below accepted (CHANGELOG, 2026-10-17). The question (from the symmetry discussion of 2026-10-17): can a
knotless form drop `# looped # with seed` by giving every editor an
**introducing** form, `{ | b } → { l :: a | b }`, its blank stated at the
leaf as `init` of the row above it? Piloted on signup-form (MDC3) and
flight-booker (MDC3), as agreed.

## What is in this worktree

- `PUI.introduced @l init editor` — the lift. **Not** `lcmap (insert (init
  b))`: in a chain, an upstream edit re-feeds every editor below it, and a
  bare insert would re-show each init over the user's value. So the lift
  retains the user's last value and re-shows `init` only when `init`
  itself changed since the last feed (structural equality). That is a
  state the rows do not show plus a comparison — exactly the hidden-state
  shape the library deleted (`bracketed`, `edited`; guardrails L14).
- MDC3 `filledTextFieldFrom`, `debouncedTextFieldFrom`, `radioButtonFrom`,
  `selectFrom`, `checkboxFrom` — one primed sibling per editor used.
- signup-form MDC3 knotless, its seed taken apart into five init
  functions in `SignupForm.purs`; the MDC2 twin keeps the loop, so the
  twins no longer differ by vocabulary alone.
- `scripts/smoke/tests/introducing-editors.mjs` — the behavioural check: a
  Username edit leaves Plan and Email as the user set them, the readouts
  follow. Passes (`npm run smoke -- introducing-editors`), as do `spago build` and
  `check-determined signup-form-mdc3`.

## Findings

1. **Screen order leaks into the view model's types.** An init is typed at
   the row the editors above it have built, so the hints the compiler
   demands are
   `defaultCountry :: { "Plan" :: …, "Username" :: String } -> Country`,
   `termsUndecided :: { "Country" :: …, "Email" :: String, "Plan" :: …,
   "Username" :: String } -> Consent` — reorder two fields on the screen
   and two signatures change. The seed record has no order.
2. **A conditional editor cannot introduce.** Flight-booker's Return date
   sits under `# inCaseAt @"return" @"Flight type"`; the pane is `{ | r }
   → { | r }` and, detached, the wire, so the field must already be in
   the row above the pane — the compiler pushes it up to the Start date
   editor's output and fails there (error saved beside this file in the
   session scratchpad; the demo was reverted). A field whose editor
   exists sometimes is model state, which is today's design.
3. **The lift is stateful.** Finding 1 is a cost, 2 a boundary; this one
   fails the admission test's derivation and honesty steps: the word is
   not derived from the vocabulary, keeps a value the row does not show,
   and needs an equality the holes cannot supply (hence
   `withStructuralEq`).
4. **What it buys.** For a pure form (signup-form, product-review,
   loan-calculator, parcel, markdown-previewer, potluck): no `looped`, no
   `with`, no seed — three tokens less on the view, five more names in
   the view model.

## Verdict proposed (overruled)

Two editor shapes for two kinds of screen was the right reading, but the
second shape does not earn words: its honest implementation is a
hidden-state leaf, and the view model pays in order-dependent signatures
what the view saves in one line. The lens editor in a `looped` row stays
the one editor shape; the seed is where a form's blanks are stated.

Eryk accepted the consequences and the word landed on main with
signup-form MDC3 as its user; flight-booker stays looped (finding 2).
