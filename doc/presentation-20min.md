---
marp: true
paginate: true
---

<!-- 7 slides
<!-- 70 sentences? -->
<!-- The screen ratio for slides is 16:9  -->

# Profunctor User Interfaces

Eryk Ciepiela

---
# Profunctors

```
class Profunctor p where
  dimap :: (s -> a) -> (b -> t) -> p a b -> p s t

-- such that:
-- dimap id id = id
-- dimap (f'. f) (g . g') = dimap f g . dimap f' g'
```

---
## Profunctors encode optics

Adapter `(s -> a, b -> t)`
isomorphic to `forall a. Profunctor p => p a b -> p s t`

Lens `(s -> a, s -> b -> t)`
isomorphic to `forall a. CartesianProfunctor p => p a b -> p s t`

Prism `(s -> Either t a, b -> t)`
isomorphic to `forall a. CocartesianProfunctor p => p a b -> p s t` etc.

Isomorphism - polymorphic profunctor modifiers are equivalent to optics

Polymorphism - not specific to any profunctor instance

Composition - function composition

> "Profunctor Optics. Modular Data Accessors" by Matthew Pickering, Jeremy Gibbons, and Nicolas Wu

---
### Adapter is a polymorphic profunctor modifier

![height:350px](resources/adapter.svg)

> "Don't fear the profunctor optics" by Jesús López-González, https://github.com/hablapps/DontFearTheProfunctorOptics

---
### Lens is a polymorphic profunctor modifier

<!-- `(s -> a, s -> b -> t)` as
`forall a. CartesianProfunctor p => p a b -> p s t` -->

```
class Profunctor p => CartesianProfunctor p where
  first  :: p a b -> p (a, c) (b, c)

-- such that:
-- dimap (\(x, y) -> x) id = dimap id (\(x, y) -> x) . first
-- dimap (mapSnd f) id . first = dimap id (mapSnd f) . first
-- first . first = dimap assoc unassoc . first
```

![height:350px](resources/lens.svg)

<!-- > "Don't fear the profunctor optics" by Jesús López-González, https://github.com/hablapps/DontFearTheProfunctorOptics -->

---
### Prism is a polymorphic profunctor modifier

```
class Profunctor p => CocartesianProfunctor p where
  left  :: p a b -> p (Either a c) (Either b c)

-- such that:
-- dimap id Left = dimap Left id . left
-- dimap (mapRight f) id . left = dimap id (mapRight f) . left
-- left . left ≡ dimap assoc unassoc . left
```

![height:350px](resources/prism.svg)

---
### Polymorphic profunctor modifiers compose as functions

```
fulfillment . delivery . address . street
  :: forall p.
     CartesianProfunctor p =>
     CocartesianProfunctor p =>
     p Street Street ->
     p Order Order
```
Lenses `fulfillment` and `street` involved
Prisms `delivery` and `address` involved
Constraints of `CartesianProfunctor` and `CocartesianProfunctor` add

---
## Profunctors encode arrow-like computations

For `ArrowChoice` plus `dimap`, minus `arr`, minus `id`

* Introducing `dimap`
  * enables "data plumbing" between arrows
  * excludes some instances - should not be the case for a type denoting a computation?
* Eliminating `arr`
  * disables "data plumbing" as first-class citizen - should not be the case either?
  * includes more instances
* Eliminating `id`
  * disables _no-op_ as first-class citizen - should not be the case either?
  * includes more instances

---
## Profunctors encode arrow-like computations

```
class CartesianProfunctor a, CocartesianProfunctor a, Semigroupoid a =>
  ArrowLike a
```

* disabled power vs. enabled more instances as compared to arrows
* simpler laws as compared to arrows
* `dimap`, `left`, `second`, `.` required
* `lmap`, `rmap`, `second`, `right`, `***`, `+++` derivable

---
## Profunctors are a common fabric for data and computation structure

Arrow-like structures computations

Optics structure data

All based on profunctors

---
## Profunctors as a fabric for optics were discovered late

When optics were first discovered they didn't mention profunctors.
Chronologically, there was
  * concrete (explicit) encoding,
  * van Laarhoven encoding proposed in 2009
  * profunctor encoding published in 2017

> "CPS functional references" by Twan Van Laarhoven https://www.twanvl.nl/blog/haskell/cps-functional-references
> "Profunctor Optics. Modular Data Accessors" by Matthew Pickering, Jeremy Gibbons, and Nicolas

---
## Profunctors as a fabric for arrows is not Haskell mainstream

Haskell `arrows` package from 2001 predates the `profunctors` package founded in 2011.

The `Control.Arrow` module made it to the `base` package in 2005.
`profunctors` is not in the `base` to this day.

That's why the `Arrow` class is not, and is not likely to be in the future, a subclass of the `Profunctor` class in Haskell.

> "Generalising Monads to Arrows" by John Hughes

---
## Profunctor encoding of optics is the default one in Purescript

Profunctors in Haskell were about 10 years late.

They were late in optics and arrows domains.

Optics in PureScript ecosystem, however, are based on profunctors.

> Package `purescript-profunctor-lenses` https://pursuit.purescript.org/packages/purescript-profunctor-lenses

---
## Profunctor encoding of arrows is the default one in Purescript

Luckily, in a sense, PureScript hadn't got arrows earlier than profunctors.

Once it had got profunctors, arrows were deemed no longer necessa
In PureScript ecosystem, cartesian (strong) profunctor category is a synonym for an arrow.

> Package `purescript-profunctor` https://pursuit.purescript.org/packages/purescript-profunctor

---
# Profunctors as a basis for application frameworks in Purescript?

If so, can we build application framework based on profunctors in PureScript?
Given PureScript is a web language - can we build a web framework basedprofunctors?
Can profunctors provide a basis for fully declarative, reactive web application framework?

- only implementations of `dimap`, `left`, `second`, `>>>` required for a building block
- optics polymorphism - sepraration of business data structure and presentation
- data plumbing logic only in optics not in presentation - no `arr`

---
## Plumbing with profunctor optics instead of arrow notation

Arrow (`proc`) notation is not widely used as compared to Monad do notation.
Arrow notation is controversial?
With profunctor categories and profunctor optics we don't need dedicated notation nor syntactic sugar.
What's needed is PureScript's `QualifiedDo` feature (https://jordanmartinez.github.io/purescript-jordans-reference-site/content/11-Syn06-Modifying-Do-Ado-Syntax-Sugar/src/13-Qualified-Do-ps.html).
This enables `do` blocks for `Semigroups`, `Semigroupoids` and `Alt` typeclasses (https://pursuit.purescript.org/packages/purescript-qualified-do/2.2.0).
Necessary plumbing of profunctors is done via profunctor optics.

---
<!--
For data flow suffices that the `Arrow`+`ArrowChoice`-`id`-`arr`+`dimap` which is `Profunctor`+`StrongProfunctor`+`ChoiceProfunctor`+`Semigroupoid`:
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d (from Profunctor)
  first :: a b c -> a (b, d) (c, d) (from StrongProfunctor)
  left :: a b c -> a (Either b d) (Either c d) (from ChoiceProfunctor)
  (>>>) :: a b c -> a c d -> a b d (from Semigroupoid)

Semigroupoing law:
p a b >>> (p b c >>> p c d) ≡ (p a b >>> p b c) >>> p c d

you can get (+++) :: ArrowChoice a => a b c -> a b' c' -> a (Either b b') (Either c c') from left having arr
you can get (+++) :: ProfunctorChoice a => a b c -> a b' c' -> a (Either b b') (Either c c') from left/bimap/>>>
but getting left from (+++) requires id
you can get (***) :: Arrow a => a b c -> a b' c' -> a (b,b') (c,c') from first having arr
you can get (***) :: ProfunctorStrong a => a b c -> a b' c' -> a (b,b') (c,c') from first/bimap/>>>
but getting first from (***) requires id

Or maybe:
For data flow suffices the `Arrow`+`ArrowChoice`-`arr`+`dimap` which is `Profunctor`+`StrongProfunctor`+`ChoiceProfunctor`+`Category`

But do we really need `Category.id`?
We can have an id from pzero:

id :: forall p a. Choice p => Zero p => p a a
id = dimap Right (either absurd identity) (left pzero)

Or we can have an id from pone:

id :: forall p a. Strong p => One p => p a a
id = dimap (\a -> Tuple unit a) (\(Tuple _ a) -> a) (first pone)

So maybe:
For data flow suffices the `Arrow`+`ArrowChoice`-`arr`+`dimap` which is `Profunctor`+`StrongProfunctor`+`ChoiceProfunctor`+`SumProfunctor`+`ZeroProfunctor`
with id as above

So, having `left` makes `id` not needed? With `left` we can do skipping steps. For what else than `left` we would need `id`?
Having `id` on the other hand is dangerous when `mappending` (infinite update loop).

# References

Guillaume Boisseau
St Anne’s Col
University of Oxford
A thesis submitted for the degree of
MSc in Computer Science
Trinity 2017
https://arxiv.org/pdf/2001.11816
- "isomorphism" optics encoding, similar to existentional optics encoding?

Modular data accessors
https://arxiv.org/pdf/1703.10857 -->


<!-- # Notions
Functional References
In functional programming, "functional references" (often called "optics") are abstractions that provide a composable way to access and modify parts of immutable data structures.

Key Characteristics
Immutability-friendly: They work with immutable data, creating new versions rather than modifying in place
Compositional: Can be combined to access deeply nested structures
First-class: Can be passed as arguments, returned from functions, etc.

for fixed p as P it looks like an optic:

a
  a
  b
c -->

