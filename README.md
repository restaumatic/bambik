
# Profunctor User Interfaces

This is a prototype of the idea of *profunctor user interfaces* for Web/Material Design Component-based UIs written in [PureScript](https://www.purescript.org/).

[Read more on the idea](/doc/description300-1000words.md)

> Notice: this section is still work in progress.

## From lenses to optics

Lenses, prisms, traversals etc., under common name of optics, provide a convenient way for navigating through data structures.

Problem: Concrete (explicit) encoding of optics is heterogeneous hence not composable.

## Profunctor optics

Profunctor optics encodes optics in homogeneous thus composable way.

Question: Are profunctor optics expressive enough in navigating through data structures?

## The power of optics

  1. Adapter (Profunctor) and its sub classes:
    1. Iso
    1. Projection and its sub classes:
      1. Constant
  1. Lens (Profunctor+Strong) and its sub classes:
    1. Lens' and its sub classes:
      1. Field
  1. Prism (Profunctor+Choice) and its sub classes:
    1. Prism' and its sub classes:
      1. Constructor
  1. Null (ProfunctorZero)

Optics describes the structure of data without releaving its representation (data constructors, functions etc).

What if we start using optics instead of representation (data constructors, functions etc)?

## More on profunctor optics

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
functions, which are necesseary to encode a lens.

`Strong p` denotes a profunctor `p`, whose specific output occurence, in a way, refers to specific input occurence that "caused" the output value, establishing an implicit, logical link from output occurence to input occurence.

We can think about it as follows: for fixed `p . Profunctor p, Strong p`, and for all `a` and `b`, `p a b` can be lifted to `p (a, c) (b, c)` for an arbitrary `c`.
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
functions, which are necessary to encode a prism.

> Pending: Choice intuition

The laws of concrete optics encoding (like `get (set s a) == a` for lenses) do not impose additional laws on profunctor encoding over inherent laws of Profunctors, Strong, Choice etc. (like `dimap id id = id`).

Profunctor optics evokes an idea on how to construct optics-based UIs.
For example, in order for UI to support adapters, lenses and prisms as first-class citizens, UI must instantiate Profunctor, Strong and Choice.

## Lifting optics

`Lens a b s t` denotes relation of `a` being part of `s` and `b` being modificator of `s` into `t`.
In particular, `Lens a a s s ` denotes `a` "is part of" `s` relation.

"Lifting is a concept which allows you to transform a function into a corresponding function within another (usually more general) setting." - https://wiki.haskell.org/Lifting

Just like functor `F` lifts `a -> b` to `F a -> F b`,
applicative functor `F` lifts `a -> b -> c` to `F a -> F b -> F c`:
profunctor `P` lifts adapter to `P a b -> P s t` function,
strong profunctor `P` lifts lens to `P a b -> P s t` function,
choice profunctor `P` lifts prism to `P a b -> P s t` function.

## Optics for data flow

> PENDING

### Pre-Arrows

Pre-Arrows are monoids in the category of profunctors.
It is simply a restriction of the Arrow class, omitting the `first` operation [NOCM].

```haskell
class Profunctor a => PreArrow a where
arr ::(b → c) → a b c
(≫):: a b c → a c d → a b d
```

The laws that must hold are
```haskell
(a ≫b)≫c = a≫ (b ≫c)
arr f ≫ a = dimap f id a
a ≫arr f = dimap id f a
arr (g ◦ f) = arr f ≫arr g
```

## Profunctor Oculars

Polymorphic profunctor transformers work as optics.
What else profunctors transformers can do?

Profunctor polymorphic transformers (watch the word order) are when we fix an instance of profunctor and it's the transformer itself that is polymorphic.

Let's call then oculars:

```haskell
instance Profunctor AProfunctor where ...

anOcular :: forall a b . AProfunctor a b -> AProfunctor a b
```

Oculars are complementary to optics since they can't change with input/output data because of polymorphic input and output, but can change the way it's processed.
(Optics can change input/output data, but can't change the way they are processed because of polymorphic profunctor).

Optics encode data model.
Oculars encode presentation.

## References

[LAAR] - Joachim Breitner: CIS 194: Introduction to Haskell (Fall 2016) - https://www.cis.upenn.edu/~cis1940/fall16/lectures/14-lenses.html

[EXIST-OPTICS] - Marco Perone: "Existential optics" - https://www.tweag.io/blog/2022-05-05-existential-optics/

[PO] - Matthew Pickering, Jeremy Gibbons, and Nicolas Wu: "Profunctor Optics. Modular Data Accessors" - https://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/poptics.pdf

[NOCM] Exequiel Rivas, Mauro Jaskelioff: Notions of Computation as Monoids - https://arxiv.org/abs/1406.4823


# The library

## Demo

Run the demo locally:

```bash
$ npm install && npm run demo1
```

## TODOs

  1. Full set of first-order MDC components
  1. Validation
  1. Collections and Higher-order components (including MDC)
  1. I18n
  1. Web UI live preview
