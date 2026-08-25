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

1. **The joint merge** — the whole mechanism (first landed as
   `instance Apply m => Semigroup (PUI m a b)`; revised in the third
   step, see below):

   ```purescript
   joint :: p a b -> p a b -> p a b   -- broadcast in, interleave out
   ```

   Associative, registration order = code order = DOM order, no gate, no
   trim, no ownership — the shared-record sibling of `recordToVariant`'s
   ungated broadcast.

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

## The fusion (second step, this branch)

The deferred fusion is done: the vocabulary is committed to the joint
world, and `completed` is **deleted** (L14 subsumption).

  * **`field @l` became the `Strong` field lens** (absorbing
    `focusProperty`, also deleted — one name for one thing, and the demo
    word won). Every label-indexed editor was already `field @l`-lifted
    internally, so the whole leaf catalogue went open-row by signature
    alone: `Cons l T () r` became `Cons l T rest r`, in all six
    vocabularies. An editor is now natively
    `p { l | rest } { l | rest }` — fed the wide row, edits its field,
    re-attaches the background the lens retains.
  * **Selectors stay closed** (`{ l :: Maybe a } → { l :: a }`): they
    remain owned-merge operands where the `Maybe` is load-bearing
    (potluck's gather gate), and `required`/`optional` now *absorb* the
    completion — each adopts a closed selector as a whole-row citizen,
    deriving the label from the closed rows exactly as before. Label
    derivation is why the closed leaf shape must survive somewhere:
    `RowToList`'s fundep cannot read a label off an open row, which is
    also why `projected`-over-an-editor died (see the demo note below).
  * **The owned merge's remaining ground**: content merges — chrome
    beside displays inside gated rungs — and bare type-changing
    selectors beside the displays that read them. Editors are pipeline
    stages; parallel writers of one field use `joint`.
  * **The loop is now load-bearing for exactness** (the residual above,
    made policy): an editor's background is as fresh as its last feed,
    so editor ensembles live inside `mvu`/`looped`/`bracketed`, and the
    one loop-free flow (order-form) wraps its form window in `# looped`,
    fed by its load action — crud's shape, generalized.
  * **Demo sweep** (all under L15's full stack, green: zero-warning
    build, tests, 102 bundles, whole smoke suite): every
    `# completed` dropped; owned merges of editors became pipeline
    stages; pure chrome in pipelines is `shownAs identity (…)`;
    counter-style `text @l # projection f # completed` display stages
    became `shown @l f`; temperature-converter's two-label
    `# projected`-editor pattern dissolved into a both-text model
    normalized by `settled` (one shared logic edit for all six twins) —
    the honest casualty: adopting an open-row editor under a different
    model reading has no annotation-free encoding, and the `settled`
    form is smaller anyway.
  * **L4 stands amended as the first step recorded**: responsibility is
    exclusive where the owned merge is used, temporal (last writer wins)
    in the joint world — which is now the default world for editors.

## The class (third step)

The joint merge moved from `instance Apply m => Semigroup (PUI m a b)`
to a class at the **profunctor kind** — `Data.Profunctor.Joining`,
beside `Seeding` and `Looping` (juxtaposition as carrier structure),
method `joint`. The instance at the saturated type was structure at the
wrong kind twice over: PureScript has no quantified constraints, so
`forall a b. Semigroup (p a b)` is unstatable and the operation could
never be carrier structure a signature abstracts over; and the
ecosystem's function-like `Semigroup` lifts **pointwise**
(`Semigroup b => Semigroup (a -> b)`), so claiming broadcast/interleave
as *the* semigroup of `PUI m a b` would give `<>` a different algebra on
different carriers. The literature pointer is `ArrowPlus`'s `<+>` (the
arrows' monoid, minus `arr`); no unit member, deliberately — the lawful
unit differs by output shape (L5), so the units stay per-direction. No
`(->)` instance (a function cannot interleave two emission streams);
`Semigroup r => Joining (Cont r)` runs both continuations and combines
the answers, recorded in `Cont`'s exhaustive inventory. Laws (broadcast,
interleave order, observational associativity) run on `PUI Effect`
probes in `spago test`; tip-calculator's trio reads
``filledTextField @"Bill amount" {} `joint` slider @"Tip percentage" {}
`joint` rangeInput @"Tip percentage"``.
