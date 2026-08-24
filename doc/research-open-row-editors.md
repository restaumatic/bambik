# Research: open-row editors and the joint merge

**Branch: `research-open-row-editors` (off main after the gated-display
merge). Status: green in full — zero-warning build, tests, 102 bundles,
the whole smoke suite including a new tip-calculator test.**

## The proposal under test

Editors as whole-row citizens, `p { a | r } { a | r }` — fed the wide
row, emitting the wide row with the edited field fresh — combined not by
the owned, gated `×→×` merge but by an **ungated joint merge**: broadcast
in, either side's emission forwards whole, **last writer wins**. The
proposal emerged by symmetry with the gated displays and survived the
standard objections under scrutiny:

  * *"Two editors of one field would clobber silently"* — no: they become
    **two synchronized views-with-write of one value**, coherent through
    the loop, and the duplicated-by-accident case is loud in the rendered
    page (two identically-captioned controls). The `DisjointLabels` check
    policed the gated union's internal need for disjointness, not the
    user.
  * *"Stale sibling fields"* — the loop re-feeds every sibling within the
    same task as any emission (single-threaded carrier, synchronous
    loop), so retention is current at every emission. The guarantee is
    scheduling, not types — solid on `PUI Web`, unproven for a
    concurrent carrier.
  * *"Starvation diagnostics lost"* — starvation is a gated-world failure
    mode; with nothing withheld there is nothing to diagnose, and a field
    with no editor is just displayed state.

## What was built

1. **The joint merge is a `Semigroup` instance** — the whole mechanism:

   ```purescript
   instance Apply m => Semigroup (PUI m a b)   -- broadcast in, interleave out
   ```

   Associative, registration order = code order = DOM order, no gate, no
   trim, no ownership — the shared-record sibling of `recordToVariant`'s
   ungated broadcast. Forms combine with `<>`.

2. **The open-row editor is `leaf # completed`** — the shape already
   existed; fusing it into leaves is mechanical (`completed`'s constraint
   set moves into the leaf signature). The research uses the unfused
   form.

3. **The decisive artifact** — tip-calculator-mdc2's tip percentage is
   now edited by **both** the MDC slider and a native `rangeInput`, in
   one merge:

   ```purescript
   ( (filledTextField @"Bill amount" {} # completed)
       <> (slider @"Tip percentage" {} # completed)
       <> (rangeInput @"Tip percentage" # completed) )
   ```

   The owned merge rejects this by `DisjointLabels`. The smoke test
   drives the native range and asserts the model, the readout line, and
   the sibling MDC slider all follow within one loop turn — two views,
   one field, live.

## The honest residuals

  * **L4 is amended** where the joint merge is used: "responsibility is
    exclusive" becomes "responsibility is temporal — last writer wins."
    Both merges now exist side by side; the owned merge keeps its laws
    where it is used.
  * **Exactness is carrier-scheduling-dependent** in the joint world. A
    concurrent carrier would need the owned merge's trim back; the joint
    merge's laws should say so if this ever leaves research.
  * Per-editor retention (one full-row `Ref` per `# completed`) replaces
    the merge's single gate state — a consistency surface, kept honest by
    the same loop invariant.

## Standing

The two merges coexist and divide the ground cleanly: the **owned merge**
where the business wants one producer per field, machine-checked, with
knowledge-gating (unseeded ensembles, `acted`-style completeness UX); the
**joint merge** where citizens are whole-row (gated displays, completed
editors) and multiplicity of writers is a feature (dual-bound controls).
`completed` is the bridge from owned-world editors into joint-world
citizens; fusing it into leaves would make the joint world self-standing
at the cost of committing the vocabulary to it.
