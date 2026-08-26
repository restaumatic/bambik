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
    stages; pure chrome in pipelines is `(…) # shownAs identity`;
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
method `joint`, homed in `extras/profunctor` beside the coined
strengths: an ecosystem complement (no row, no carrier, liftable
unchanged), not bambik's own. The instance at the saturated type was structure at the
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

## The editor pane (fourth step)

`informed` turned out to be two things. At nine sites it was the fusion's
own smell in another coat: `# provided paneOf # updated (informed
setField)` with `setField` the **identity** (`setTable { "Table": t } =
{ "Table": t }`, `setReturn` likewise) — a pane pinning a whole-row
editor down to its payload, then a retention `Ref`, a union and a
do-nothing function rebuilding the completion its `field @l` lens would
have done for free. The dissolution is `shownCase`'s editor sibling,
`inCase @l classifier`: a whole-row citizen whose *existence* is gated
on the case, derived as `joint (provided caseHolds w) identity` — the
pane **`joint`** the wire, where `shownCase` is the pane owned-merged
with it (its content emits `{}`; an editor's emits the row, which the
owned merge's disjointness rejects, so the rung became derivable only
once `Joining` existed). Order-form's three fulfillment panes and
flight-booker's return date read `# inCase @l selection` /
`# inCase @"return" tripType`; six setters and pane projections in
order-form and two in flight-booker died, replaced by one row-stating
classifier each. Two releases per feed while attached (the wire's and
the editor's echo), idempotent under the loop.

Where `informed` **survives** it is doing genuine dispatch — a payload
that is computed (circle-drawer's diameter quantity, assembled from a
scalar and constant bounds; meeting-booker's `seats` role-renamed over
`attendees`) or a fold that does real work (undo-stack transactions,
`markFavorite`'s map over the movies, cashbox's three branches) — and
the 2026-08-13 rejection of fusing it into `updated` stands: per-branch
merging inside `match` needs the row-walking class machinery that was
judged too heavy, and the single-case `toCase @l … # updated (match
{ l: informed f })` shape keeps its case because the label is what the
emission trace prints.

## The Mealy shape (fifth step)

`informed` is gone, and no class replaced it — because it never was
algebra. Its body was `\g pay small -> g (union pay small)` with a
read-narrow coercion: `uncurry` over `Record.union`, written to serve
one style rule (*one record of data per business function*) against the
mechanism's own shape, `updated`'s handler `e -> { | small } -> { | small }`
— the Mealy step, which a dozen scalar-payload handlers (`pick`,
`selectCell`, `toggleTodo`, `refreshPeople`, `removeUnit`, …) already
took as written. The class-free routes were weighed:

- **Handlers take the Mealy shape** — chosen. `applyRefund :: { amount }
  -> { balance } -> { balance }`; `adjustDiameter :: { "Diameter" :: qty }
  -> { circles, selected, "Diameter" :: Number, … } -> …`. Expressivity is
  identical: `informed`'s `Union fed extra (pay ∪ small)` already limited
  reads to payload ∪ writes, exactly what the two arguments give. The
  shadowing convention (a payload label over a model label of another
  type) dissolves with it — `adjustDiameter`'s quantity and the model's
  scalar are two visibly separate arguments.
- **Structural**: meeting-booker's slider is a `×→×` value folded as if
  an occurrence; with the bounded quantity in the model it would read
  `# inCase @l roomChosen` + `# settled` and lose the fold entirely.
  Circle-drawer's diameter is the same shape once `undo`/`redo` are seen
  to clear the selection — the resize invariant survives them. Both done
  as the sixth step below.
- **Bake the record case into `updated`** — declined: variant-outcome
  folds would need the Mealy form under a second name, and it is the
  2026-08-13 fusion's motivation in another coat.
- **Inline `Record.merge`** — types at four of nine sites, fails wherever
  reads ⊊ payload ∪ writes. **Replay-protocol facts** — only emitters fed
  the model get their facts for free; panes and collection items are fed
  projections by design.

The one cost bought: a payload row must be the case's exact payload —
`informed`'s "unused payload fields cost nothing" is gone. Movie-browser's
card row `{ title, year, rating, "Favorite" }` now narrows at the emitter,
`# toCase @"favored" favoriteMark`, a named projection in the logic
module; circle-drawer's canvas already emitted the exact `{ x, y }`. The
rule as writing.md states it: a fold handler is the one-record rule's one
carve-out, its two records being an occurrence and a retained state — two
directions, not one row in disguise — with the degenerate shapes spelled
by `const` (payload-only `const <<< f`, state-only `const f`, replace
`const`, neither `const (const patch)`). Nine functions, 22 view lines,
seven app families; the row layer lost one export and gained nothing.

## The quantity in the model (sixth step)

Meeting-booker's attendees slider was one of the last two `updated` folds
over an editor:
`(slider @"Attendees" {}) # provided seatsFor # updated (const <<< chooseSeats)`
— a `×→×` value folded as an occurrence, with `seatsFor` assembling a
bounded quantity from a scalar `attendees` and the room's capacity on
every feed, and `chooseSeats` projecting it back. The doctrine already
said where the quantity belongs (*bounded quantities ride one row
everywhere, as model data from the seed, re-scopable at runtime*), so the
model now holds `"Attendees" :: { current, min, max, step }`, born as just
the organizer, and the two jobs the fold did become two words that were
already there:

- **existence** — `slider @"Attendees" {} # inCase @"chosen" roomChoice`,
  the editor pane over a two-case classifier of the `Maybe` room;
- **the invariant** — `dropdown @"Room" {} [ … ] # optional # settled
  seatsInRoom`, on the room's own stage: a chosen room sets `max` to its
  capacity and clamps `current`, exactly the temperature-converter shape
  (`textField @"°C" {} # settled fromCelsius`). Re-picking a smaller room
  re-scopes the slider in place through the loop; no handler, no fold.

`seatsFor` and `chooseSeats` are deleted; `completePlan` and `seatsTaken`
read `seats.current`. The demo has no `updated` left.

Circle-drawer's diameter slider was the other one, and the fifth step had
kept it a fold on the claim that resizing the selected circle is a
transaction a `settled` invariant would clobber on undo. The claim was
wrong: `undo` and `redo` clear the selection, so "the selected circle's
radius is the slider's diameter" *is* an invariant of the state, holding
through every history move. The model now holds `"Diameter" :: { current,
min, max, step }` (born at 40 with the constant bounds), the six views read
`sliderLive @"Diameter" {} # inCase @"chosen" selection # settled
resizeSelected`, `selectOrAddCircle` writes `current` when a circle is
picked, and `selectedDiameter`/`adjustDiameter` are gone. The `adjusting`
flag still coalesces a drag into one undo transaction — inside the
normalization, exactly as before. Rule: an editor folded as an event
(`# provided paneOf # updated f`) is the smell; what the edit does to the
rest of the row is `settled` on the editor's own stage.

## The surface (seventh step)

An outsider's read of the demos — a web/FP developer, no algebra — found
the leaf-as-field idea and the small demos the strongest surface, and four
costs: a vocabulary of near-synonyms with no situation-indexed entry; two
`do`s that are not monads, taught nowhere before line one; recurring
ceremony (`identity` arguments that were never anything else, the `const`
handler spellings, long structural rows repeated verbatim); and semantics
without a mainstream analogue (the gate, copy as record key, the forked
compiler) explained from the algebra outward rather than from the screen.

What changed:

- **Two dead slots pruned (L14, applied to parameters).** `shownAs proj
  content` had 135 demo sites, every one `proj = identity`; it is now
  `shownAlways content`, the content reading its closed narrow row by
  `Union` subsumption as `told` did, and the name joins the family's
  grammar (`shownWhen`/`shownCase`/`shownEach`/`shownAlways`).
  `forProperty f` had 22 sites, every one `identity`, and
  `forProperty f = projection f >>> forProperty` — so it is now
  `forProperty`, the widening alone; formatting stays `projection`'s.
- **`Pipeline.do`.** Application code imports `QualifiedDo.Semigroupoid as
  Pipeline`, so the block reads as what it is; the four merges keep their
  direction names and the library still speaks `Semigroupoid`.
- **86 dead logic exports deleted** — `*Line`/`*Text` formatters left over
  from the `told line` era, exported by 29 logic modules and used by no
  view (the sentences are `RecordToRecord.do` merges of
  `staticText`/`text` now).
- **The newcomer path**: vocabulary.md (which word, when — an index into
  writing.md and the headers), walkthrough.md (flight-booker line by
  line), the gate stated up front in writing.md, a localization paragraph
  with its honest gap (`choice @l` has no caption override), and the
  fork's status in doc/variant-sugar.md.

Kept by decision: no `type` synonyms in application code, logic modules
included — the shape is the interface; the cost (rows repeated verbatim)
is now stated in writing.md rather than left to be discovered.
`toCase @l identity` (13 of 52 sites) and `foreach @l identity` (5 of 40)
keep their slots, real projections being the majority.

`shownAlways` lasted a day. The adverb was a policy word on the one rung
whose point is having no policy, and the bare name was free once the
field rung `shown @l f` was seen to be derivable — one text node reading
field `l` through `f`, then releasing the row, is exactly
`text @l # projection f # <ambient rung>`. So the field rung is deleted
(subsumption) and the ambient rung is **`shown`**: the family reads
`shown` / `shownWhen` / `shownCase` / `shownEach`, and the ten former
`shown @l f` sites read `headline4 (text @"count" # projection show) # shown`
(two of them dropping an `identity` on the way).

And then `told` went too. Every surviving `told line` site (cashbox's
`balanceLine`, `refundLine`, `courierLine`; auction's `bidLine`, `topLine`)
was a literal-plus-formatted-field sentence — the shape writing.md's
*Business functions* section names as UI structure in disguise, with
`balanceLine` as its own example — and the MDC3 twins had already written
them the lawful way. The five became `staticText`/`text` merges `# shown`
(`( headline6 $ RecordToRecord.do staticText "Till balance: €"; text
@"balance" # projection euros ) # shown`), the five line functions left
the logic modules, `told` reached zero sites and L14 pruned it; it was in
any case `shown (text @l # projected line)` under a phantom label. The
bottom rung is one word now: `shown`.

## The occurrence stage (eighth step)

The floor itself was carrying the fifth step's cost twice over.
`button @"Add" {} # updated (match { "Add": const <<< addTodo })` states
the label twice — the leaf's closed singleton row already says `"Add"`
— and the model twice: `updated` feeds the emitter the row it retains,
`clicked` replays it, so payload and state arrive as the same value and
`const <<<` throws one away. vocabulary.md filed the shape under "the
button carries no payload" while prescribing the *payload-only*
spelling; the payload position was load-bearing only as the pin of the
emitter's input row — `updated (const f)`, the honest state-only shape,
leaves a button's row ambiguous.

**`applied`** is that pin given a name: `applied f = updated (const f)`
at the signature `({ | small } -> { | small }) -> PUI m { | small } [ | s ]
-> PUI m { | big } { | big }`. The emitter is fed `f`'s footprint; its
emissions are occurrences, their payload unread; `[ | s ]` keeps the
wrapped component an emitter. A first cut derived the case label from
the singleton row (`RowToList`, as `toCases`/`forCase` do) — seven
constraints for a rung whose whole point is not looking at the payload;
it lasted an hour. Law-tested on the probe carrier (gated, pass-through,
replay payload discarded). It touches none of the standing decisions: no
row-walking class (the 2026-08-13 rejection), `updated` keeps its name
and Mealy shape for every real payload and every stage whose emitters
mean different things (the "second name" decline), and the case stays
for the trace to print — only its `match` restatement goes. Twelve sites
in five families (counter ×6, todomvc's Add and Clear completed, inbox's
Compose) read `# applied f`; six view modules lost their
`const`/`(<<<)`/`match` imports. The multi-handler `const <<< f`
branches (circle-drawer's undo/redo, inbox's sort menu and dialog
outcomes, espresso-bar) stand: a `match` over emitters meaning different
things is honest dispatch.

The same pass simplified the **subsumption encoding**. `updated`,
`every` and `settled` each stated "the footprint is a sub-row of the
model" as `Union small big u => Nub u big` — two constraints and a
phantom `u`, there only because `Record.merge` demands `Nub` — while the
read side of `updated` stated the same fact as `Union narrow extra big`.
`Record.merge` *is* `unsafeUnion` at runtime, so the three now write
`unsafeUnion (f small) big :: { | big }` under the one constraint
`Union small rest big`, which reads as the fact it is: the model is the
footprint plus the rest. `Nub` left both modules' imports; `applied`
needs exactly one constraint, and it is that one.
