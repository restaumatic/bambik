# ?

In this work I'll start with profunctor optics, extrapolate the idea to invariant optics and then expand the idea even beyond that: to the duals of optics.

## Profunctor optics

TODO

## Profunctors and invariants

* mapping: `dimap :: p a b -> (a' - a) -> (b -> b') -> p a' b'` turns into `invmap :: i a -> (a' -> a) -> (a -> a') -> i a'`
* cartesianity (aka stregth, StrongProfunctor): `first :: p a b -> p (a, c) (b, c)` turns into `invfirst :: i a -> i (a, c)`
* co-cartesianity (aka ChoiceProfunctor): `left :: p a b -> p (Either a c) (Either b c)` turns into `invleft :: i a -> i (Either a c)`
* product-monoidality: 
    * `p a b -> p c d -> p (a, c) (b, d)` turns into `i a -> i b -> i (a, b)`
    * `p () ()` turns into `i ()`
* sum-monoidality: 
    * `p a b -> p c d -> p (Either a c) (Either b d)` turns into `i a -> i b -> i (Either a b)`
    * `p Void Void` turns into `i Void`
* profunctor optics tells us *polymorphic optics is polymorphic profunctor transformer* (PolyOptics a b s t === forall p. p a b -> p s t)
* invariant optics tells us *monomorphic optics is polymorphic invariant transformer* (MonoOptics a s === forall i. i a -> i s)

## Invariant optics

By analogy to profunctor optics we can distinguish the following invariant optics: 

* adapter
* lens
    * projector (projection?) - specialization of lens, not there in polymorphic optics (s -> a)
* prism
    * action (ray?, constructor?) - specialization of prism, not there in polymorphic optics (a, a -> s)
    * prototype - (s -> i s -> i s), when prism fires, replaces s with given s
* traversal (s -> FunList a s, data FunList a s = Done s | More a (FunList a (a → s)) === s -> ([a], [a] -> s))
    * fold (?) - specialization of traversal, not there in polymorphic optics (s -> [a])

Importantly, there is more invariant optics than profunctor ones.
Projections, actions and folds are only possible in invariant optics.
This makes invariant optics, in a sense, more capable than its profunctor couterparts.
This is at the cost of monomorphicity.

> Invariant optics - monomorphic but more capable

> Profunctor optics - polymorphic but less capable


Invariant less demanding.    

## Profunctors/invariants apart from optics

Polymorphic profunctor/invariant transformers work as monomorphic/polymorhphic optics. 
What else profunctors/invariant transformers can do?

Profunctor/invariant polymorphic transformers (watch the word order) are when we fix an instance of profunctor/invariant and the transformer itself is polymorphic.

```
myInvariantPolymorphicTransformer :: forall a . MyInvariant a -> MyInvariant a

myProfunctorPolymorphicTransformer :: forall a b . MyProfunctor a b -> MyProfunctor a b
```

They are the exact opposite and complementary to polymorphic invariant transformers since they can't deal with data structures processed by invariants/profunctos. At the same time they can manipulate the processing in all other aspects.

For polymorphic profunctor/invariant transformers it's the opposite: they can deal with data structure being processed but cannot influence the processing in any other aspect.

This makes the pair complementary and orthogonal.

As it will turn out, this complementarity and orthogonality is the same complementarity and orthogonality between the model and the presentation we see in user interfaces.

## Optics and combinators

Optics are transformers: functions from invariant/profunctor to the same invariant/profunctor.
Combinators, in turn, are functions from two same invariants/profunctors to the same invariant/profunctor.

Profunctor/invariant polymorphic transformers have the shape of `forall a b . MyProfunctor a b -> MyProfunctor a b`/`forall a . MyInvariant a -> MyInvariant a`.

Profunctor/invariant polymorphic combinators, in turn, have the two arguments thus the shape is `forall a b . MyProfunctor a b -> MyProfunctor a b -> MyProfunctor a b`/`forall a . MyInvariant a -> MyInvariant a -> MyInvariant a`.

## Foo

The typeclass 
```
class Foo i where
    iappend :: i a -> i a -> i a
    iempty :: i a
-- laws: 
--  iappend a iempty == a = iappend iempty a
--  iappend a (iappend b c) == iappend (iappend a b) c
```
is seemingly related to Haskell's `Alternative` or PureScript's `Plus`/`Alt`/`Alternative` but it differs in that it has no `Functor` nor `Applicative` constraint on `i`. 
It's rather a relative of `Monoid` for `* -> *` kind types.

Intuitively, `Foo i` denotes `i` have the quality of being able to reason about a number of `i a`'s as a single `i a`, for any `a`.
Moreover, there is `iempty :: i a` for every a, that can be discarded when reasoning about a number of `i a`s. 

Foo invartiants denote invariants that are not (effectful) functions (endomorphism) of shape `Applicative m => a -> m a` as then `iempty` must have been `pure`, so `iappend iempty a` would yield two `a`s from which one `a` must have been selected and the selection would have always been the oppostite to the selection of `iappend a iempty` which contradicts the first law.

Foo invariants are then invariants that "fire" output not on input but on other external trigger.
This, again, reminds of UI where the trigger is a user action rather than data populating the UI.

Foo invariant enables:

```
combineCartesian :: (Invariant i, CartesianInvariant a, Foo i) => i a -> i b -> i (a, b)
combineCartesian ia ib = first a `iappend` second b

combineCoCartesian :: (Invariant i, CoCartesianInvariant a, Foo i) => i a -> i b -> i (Either a b)
combineCoCartesian ia ib = left a `iappend` right b
```

Notice that foo profunctor doesn't enabled that:
```
combineCartesian :: (Profunctor i, CartesianProfunctor a, PFoo p) => p a b -> p c d -> p (a, c) (b, d)
combineCartesian ia ib = first a `iappend` second b -- type mismatch

combineCoCartesian :: (Profunctor i, CoCartesianProfunctor a, Foo i) => p a b -> p c d -> p (Either a c) (Either b d)
combineCoCartesian ia ib = left a `iappend` right b -- type mismatch
```

For this reason, we won't analyse profunctor case here anymore, and will focus on invariants only:
  * Polymorphic invariant transformers - adapters, lenses, prims
  * Invariant polymorphic transformers - non-optics
  * Invariant polymorphic combinators - traversals, non-optics

---
References

[1] - Matthew Pickering, Jeremy Gibbons, and Nicolas Wu: "Profunctor Optics. Modular Data Accessors" - https://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/poptics.pdf