## Module Data.Profunctor.Row.Sequence

The **sequence direction** of the row-profunctor family: the collection as
the runtime-sized, homogeneous generalization of a row merge. Where a record
merge combines a fixed set of distinct-typed operands over static labels,
the sequence merge combines a runtime-sized set of same-typed operands over
an `Array`, keyed by `key a`.

Class law (up to the `Array` wrapper), for the `PUI Web` instance:

  * **singleton** — `sequenced key g` fed `[a]` behaves as `g` fed `a`
    (one element, built and driven through its channel);
  * **empty (the nullary unit)** — `sequenced key g` fed `[]` builds nothing
    and emits nothing (it collapses to the shared output `o`, which is
    uninhabited with no elements — so a terminal display pairs it with
    `displayed`, whose unconditional carrier echo supplies the announcing
    unit);
  * **retraction / reconciliation** — feeding an array reuses the element
    instance for each surviving `key`, so identity (DOM, focus) follows the
    key, not the position.

Only `PUI Web` has an instance: a dynamic DOM collection has no `(->)` or
general-carrier meaning (there is no canonical `Array a -> o` from `a -> o`),
exactly as `Resolving`/`Retaining` are `PUI`-only. `foreach` (PUI.HTML) is
the friendly re-export of `sequenced`.

#### `Sequencing`

``` purescript
class (Profunctor p) <= Sequencing p  where
  sequenced :: forall a o. (a -> String) -> p a o -> p (Array a) o
```

Lift a profunctor over the homogeneous `Array` shape, keyed by `key`.

##### Instances
``` purescript
Sequencing (PUI Web)
```


