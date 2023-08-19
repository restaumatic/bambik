# Bambik

##  Demo

> npm install && npm run demo1

## Best practices

  * In component body, each line should start with sub component (e.g. `div`). In particular place `^`, `<^` or `^>` at the end of line. This is for highlighting visual aspect.
  * In component body read `#` as *of*, so `text # value # field @"firstName"` can be read as *text of value of field "firstName"*.

## TODO

  1. First-order MDC components
  1. Higher-order components (including MDC)
  1. Inputless components
  1. I18n support
  1. Nice type synonym for fields
  1. Virtual fields
  1. Get rid of Scoped in Data.Profunctor.Optics

## Abstract

In this work we'll start with presenting profunctor optics encoding where an optic is encoded as a polymorphic profunctor transformer. 
Then we'll apply the same idea to invariants, which can be though of as a subtype of profunctors: monomorphic profunctors.
They trade polymophism to the ability of forming arbitrary products and sums while preserving the ability to encode optics.
After that, we'll turn "polymorphic invariant transformers encode optics" thinking into "optics are polymorphic invariant transformers" and see what other kind of optics we can discover.




 and then expand the idea even beyond that: to the duals of optics.

## Profunctor optics

Profunctor optics [PO] provides a way of encoding optics (adapters, lenses, prisms, affine traversals, traversals etc) whose advantage over alternative encodings (concrete encoding, van Laarhoven encoding [LAAR] and existential encoding [EXIST-OPTICS]) is its inherent composability, both in terms of categorical composition as well as the ability to combine different types of optics e.g. lenses with prismes etc.

For fixed types `a`, `b`, `s` and `t`, a function
```haskell
Profunctor p => p a b -> p s t
```
encodes an optic.
Notice that this function being polymorphic in `p` can only rely on profunctor's `dimap` function:

```haskell
dimap :: Profunctor p => (s -> a) -> (b -> t) -> p a b -> p s t
```

That is, `Profunctor p => p a b -> p s t` can be obtained from a pair of functions: `f :: s -> a` and `g :: b -> t` that we know as the concrete encoding of *adapter* optics.
It turns out that there's also an inverse function from profunctor encoding to concrete encoding, hence profunctor encoding is isomophic to concrete encoding [PO].

Similarly, for fixed `a`, `b`, `s` and `t`,
```haskell
Profunctor p, Strong p => p a b -> p s t
```
function is isomorphic to concrete encoding of lenses which is a pair of functions `get :: s -> a` and `set :: s -> b -> t`.
Additional constraint, `Strong p`, allows this function to use

```haskell
first :: Strong p => p a b -> p (a, c) (b, c)
second :: Strong p => p a b -> p (c, a) (c, b)
```
functions, one of which is necesseary to encode a lens.

`Strong p` denotes a profunctor `p`, whose specific output occurence, in a way, refers to specific input occurence that "caused" the output value, establishing an implicit, logical link from output occurence to input occurence.

Think about it as follows: for fixed `p . Profunctor p, Strong p`, and for all `a` and `b`, `p a b` can be lifted to `p (a, c) (b, c)` for an arbitrary `c`.
This means that the structure of `p` coveys a context `c` along the manufacturing line from input to output.
It is like saying: whetever `c` can be attached to the input `a` occurence, and if there is an output `b` occurence, it has attached `c` too.
How to get the attached `c` value of an arbitrary, unconstrained type if not from the input?

In profunctor encoding of a lens we use `second :: Profunctor p, Strong p => p Part Part' -> p (Whole, Part) (Whole, Part')`.
Indeed, we can see `Part'` output occurence is related to `Part` input occurence: the former value is an alteration of the latter value.
Therefore, `Whole` attached to `Part` on the input can be conveyed along to `Part'` on the output.
This makes possible to apply `rmap :: Profunctor p => (b -> c) -> p a b -> p a c` on lifted profunctor to turn `(Whole, Part')` into new `Whole'`.

The following function, in turn, for fixed `a`, `b`, `s` and `t`,
```haskell
Profunctor p, Choice p => p a b -> p s t
```
is isomorphic to concrete encoding of prisms which is a pair of functions `review :: a -> s` and `preview :: s -> Either b t`.
Additional constraint, `Choice p`, allows this function to use
```haskell
left :: Choice p => p a b -> p (Either a c) (Either b c)
right :: Choice p => p a b -> p (Either c a) (Either c b)
```
functions, one of which is necessary to encode a prism.

TODO: Choice intuition

The laws of concrete optics encoding (like `get (set s a) == a` for lenses) do not impose additional laws on profunctor encoding over inherent laws of Profunctors, Strong, Choice etc. (like `dimap id id = id`).

Profunctor optics evokes an idea on how to construct optics-based UIs.
For example, in order for UI to support adapters, lenses and prisms as first-class citizens, UI must instantiate Profunctor, Strong and Choice.

```purescript
data MyProfunctorUI = ...

instance Profunctor MyProfunctorUI where ...
instance Strong MyProfunctorUI where ...
instance Choice MyProfunctorUI where ...

numberInput :: MyProfunctor Float
numberInput = ...

data OutdoorConditions = OutdoorConditions
    { temperature :: Temperature
    }

data Temperature = KnownTemperature Celcius | ...

type Celcius = Float

type Fahrenheit = Float

fahrenheitCelsius :: Tuple (Celcius -> Fahrenheit) (Fehrenheit -> Celcius)
fahrenheitCelsius = ...

temperature :: MyProfunctorUI OutdoorConditions
temperature = numberInput # fahrenheitCelsius # knownTemperature # temperature
--                          ---- adapter ----   ---- prism -----   -- lens ---
```


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

---
References

[LAAR] - Joachim Breitner: CIS 194: Introduction to Haskell (Fall 2016) https://www.cis.upenn.edu/~cis1940/fall16/lectures/14-lenses.html

[EXIST-OPTICS] - Marco Perone: "Existential optics" https://www.tweag.io/blog/2022-05-05-existential-optics/

[PO] - Matthew Pickering, Jeremy Gibbons, and Nicolas Wu: "Profunctor Optics. Modular Data Accessors" - https://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/poptics.pdf
