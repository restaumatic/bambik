## Notation

A▫B - forall p. Profunctor p => p A B

S→A▫B→T

S▫T - forall p. Profunctor p => p A B

## Plus class

The class
```
class Plus i where
    plus :: i a -> i a -> i a
    zero :: i a
-- laws:
--  plus a zero == a = plus zero a
--  plus a (plus b c) == plus (plus a b) c
```
is seemingly related to Haskell's `Alternative` or PureScript's `Plus`/`Alt`/`Alternative` but it differs in that it has no `Functor` nor `Applicative` constraint on `i`.
It's rather a relative of `Monoid` for `* -> *` kind types.

Intuitively, `Foo i` denotes `i` having the quality of being able to reason about a number of `i a`'s as a single `i a`, for any `a`.
Moreover, there is `zero :: i a` for every a, that can be discarded when reasoning about a number of `i a`s.

Plus invartiants denote invariants that are not (effectful) functions (endomorphism) of shape `Applicative m => a -> m a` as then `zero` must have been `pure`, so `plus zero a` would yield two `a`s from which one `a` must have been selected and the selection would have always been the oppostite to the selection of `plus a zero` which contradicts the first law.

Plus invariants are then invariants that "fire" output not on input but on external trigger.
This, again, reminds of UI where the trigger is a user action rather than data populating the UI.

Plus invariant enables:

```
combineCartesian :: (Invariant i, Cartesian a, Foo i) => i a -> i b -> i (a, b)
combineCartesian ia ib = first a `plus` second b

combineCoCartesian :: (Invariant i, CoCartesian a, Foo i) => i a -> i b -> i (Either a b)
combineCoCartesian ia ib = left a `plus` right b
```

Notice that Plus profunctor doesn't allow for that:
```
combineCartesian :: (Profunctor i, CartesianProfunctor a, Plus p) => p a b -> p c d -> p (a, c) (b, d)
combineCartesian ia ib = first a `plus` second b -- type mismatch

combineCoCartesian :: (Profunctor p, CoCartesianProfunctor p, Plus p) => p a b -> p c d -> p (Either a c) (Either b d)
combineCoCartesian ia ib = left a `plus` right b -- type mismatch
```

For this reason, we won't analyse profunctor case here anymore, and will focus on invariants only:
  * Polymorphic invariant transformers - adapters, lenses, prims
  * Invariant polymorphic transformers - non-optics
  * Invariant polymorphic combinators - traversals, non-optics

## UI

Notice that `Invariant i => i Void` denotes a widget that is not backed by any data model, in other words, is backed by void data model.
This is a static widget that doesn't get updated nor does it update anything.

But to be able to put such widget in an arbitrary place we need a way to lift it to arbitrary data type.
This capability is descibed as StaticInvariant.

```
class Invariant i <= StaticInvariant i where
    invstatic :: forall a . i Void -> i a
```

`Invariant i => i Unit` denotes a widget...

UI provides:
  - primitive widgets (Widget Bool, Widget String etc.)
  - widget wrappers (Widget a -> Widget a, Widget Void -> Widget a etc.)
  - widget combinators (Widget a -> Widget a -> Widget a)

Model provides:
  - optics (i Customer -> i Order etc.)

what model requites from ui is expressed in invariant constraints.


## Optics and combinators

Optics are transformers: functions from invariant/profunctor to the same invariant/profunctor.
Combinators, in turn, are functions from two same invariants/profunctors to the same invariant/profunctor.

Profunctor/invariant polymorphic transformers have the shape of `forall a b . MyProfunctor a b -> MyProfunctor a b`/`forall a . MyInvariant a -> MyInvariant a`.

Profunctor/invariant polymorphic combinators, in turn, have the two arguments thus the shape is `forall a b . MyProfunctor a b -> MyProfunctor a b -> MyProfunctor a b`/`forall a . MyInvariant a -> MyInvariant a -> MyInvariant a`.
