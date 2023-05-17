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

## Invariant Optics

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

Profunctor/invariant polymorphic transformers (watch the word order!) are when we fix an instance of profunctor/invariant and the transformer itself is polymorphic.

```
myInvariantPolymorphicTransformer :: forall a . MyInvariant a -> MyInvariant a

myProfunctorPolymorphicTransformer :: forall a b . MyProfunctor a b -> MyProfunctor a b
```

They are the exact opposite and complementary to polymorphic invariant transformers since they can't deal with data structures processed by invariants/profunctos. At the same time they can manipulate the processing in all other aspects.

For polymorphic profunctor/invariant transformers it's the opposite: they can deal with data structure being processed but cannot influence the processing in any other aspect.

This makes the pair complementary and orthogonal.

As it will turn out, this complementarity and orthogonality is the same complementarity and orthogonality between the model and the presentation we see in user interfaces.


---
References

[1] - Matthew Pickering, Jeremy Gibbons, and Nicolas Wu: "Profunctor Optics. Modular Data Accessors" - https://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/poptics.pdf