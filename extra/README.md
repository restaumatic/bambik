# extra/

Parked modules: outside `spago.dhall`'s source globs, so they neither build,
ship to applications, nor appear in the API docs. Nothing here is reached by
the library, the demos or the tests — which under L14 is the criterion for
pruning — but each was kept because it records something the design notes
would otherwise have to restate in prose.

Re-enter the build by adding `"extra/**/*.purs"` to `sources`.

- **Data/Profunctor/Cont.purs** — the CPS profunctor
  `Cont r a b = (b -> r) -> (a -> r)`, the repo's only *pure* carrier of the
  row algebra: a timeless model where the merge gate is continuation nesting
  rather than a pair of `Ref`s. Its module header inventories which classes it
  validly inhabits (`Strong`, `Choice`, `Category`, `Wander`, `Acting`,
  `Cochoice`, `RecordToRecord`, `VariantToVariant`, `Monoid r =>
  RecordToVariant`) and — more usefully — which it provably cannot
  (`Costrong`, `Coresolving`, `Retaining`, `VariantToRecord`, `Seeding`,
  `Closed`), each with the reason. Those impossibilities are exactly why the
  library's trace forms take seeds and why `looped` is a primitive.
- **Data/Default.purs** — `class Default` with a `RecordDefault` derivation,
  once used to prime the checkbox and radio leaves with a value to report
  before the model had supplied one. Removed because that is precisely the
  fabricated business value the no-defaults discipline forbids: the leaves now
  take the value from the caller (`checkbox { ticked: {} }`,
  `radioButton { picked: … }`) and nothing is conjured from a type.
