---
marp: true
paginate: true
theme: default
---
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
### Polymorphic profunctor modifiers compose as functions do

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

Once it had got profunctors, arrows were deemed no longer necessary.

In PureScript ecosystem, cartesian (strong) profunctor category is a synonym for an arrow.

> Package `purescript-profunctor` https://pursuit.purescript.org/packages/purescript-profunctor

---
# Are profunctors suitable for UI frameworks?

E.g. PureScript Web UI framework?

- Declarative/reactive owing to composition
- Separating business and presentation owing to optics
- Requiring only a basic building block that supports `dimap`, `left`, `second`, `>>>`, ...

---
## `UI` is the generic basic building block


```
newtype UI m i o = UI (m
  { toUser   :: New i                  -> Effect Unit
  , fromUser :: (New o -> Effect Unit) -> Effect Unit
  })

instance Functor m     => Profunctor   (UI m)
instance Functor m     => Strong       (UI m) -- cartesian profunctor
instance Functor m     => Choice       (UI m) -- cocartesian profunctor
instance Apply m       => Semigroupoid (UI m) -- category sans identity
instance Apply m       => Endo         (UI m) -- see below
instance Apply m       => Sum          (UI m) -- see below
instance Applicative m => Zero         (UI m) -- see below
```

> Prototype of the idea of profunctor user interfaces for PureScript Web UIs  https://github.com/restaumatic/bambik


---
## `Endo`, `Sum` and `Zero` typeclasses are missing in `purescript-profunctor` package but are useful

```
class Profunctor p <= Endo p where
  pendo :: forall a. p a a -> p a a -> p a a
  -- such that `pendo p (pendo q r) == pendo (pendo p q) r`

class Profunctor p <= Sum p where
  psum :: forall a b . p a b -> p a b -> p a b
  -- such that `psum a (psum b c) == psum (psum a b) c

class Sum p <= Zero p where
  pzero :: forall a b. p a b
  -- such that `psum pzero p == p == psum p pzero`
```

> Prototype of the idea of profunctor user interfaces for PureScript Web UIs  https://github.com/restaumatic/bambik

---
## `UI Web` is the basic building block for Web UI

```
newtype Web a = Web (StateT DocumentBuilderState Effect a)

text       :: forall a. UI Web String a

textInput  :: UI Web String String

button     :: forall a. UI Web a a

p          :: forall a b. UI Web a b -> UI Web a b

div        :: forall a b. UI Web a b -> UI Web a b

staticText :: forall a b. String -> UI Web a b
```

> Prototype of the idea of profunctor user interfaces for PureScript Web UIs  https://github.com/restaumatic/bambik

---
## Plain HTML is possible with `UI Web`

```
helloWorld :: forall a b. UI Web a b
helloWorld =
  div Sum.do
    p $ staticText "Hello World!"
    ul Sum.do
      li $ staticText "One"
      li $ staticText "Two"
      li $ staticText "Three"
    a >>> "href" := "https://www.google.com" $ staticText "Search for me!"
    staticHTML "<hr/>"
```

> `Sum.do` is `Sum` profunctor `psum` composition powered by PureScript *qualified do* feature

---
## Material Design Components are possible with `UI Web`

```
filledTextField :: { floatingLabel :: String } -> UI Web String String

containedButton :: forall a. { label :: String } -> UI Web a a

card :: forall a b. UI Web a b -> UI Web a b

caption :: forall a b. UI Web a b -> UI Web a b

submitName :: UI Web String String
submitName = Semigroupoid.do
  card Sum.do
    caption $ staticText "Identifier"
    filledTextField { floatingLabel: "Name" }
  containedButton { label: "Submit" }
```

> `Semigroupoid.do` is `Semigroupoid` composition `>>>` from `purescript-qualified-do` package

---
## Optics are possible with `UI`

```
shortId  :: forall p. Strong p => p String String -> p Order Order
uniqueId :: forall p. Strong p => p String String -> p Order Order

identifierForm :: Web UI Order Order
identifierForm =
  card Endo.do
    caption $ staticText "Identifier"
    shortId $ filledTextField { floatingLabel: "Short ID" }
    orderId $ filledTextField { floatingLabel: "Unique ID" }
```

> `Endo.do` is `Endo` profunctor `pendo` composition powered by PureScript *qualified do* feature

---
## Data flow is possible with `UI`

```
authorization :: forall p :: Strong p => p OrderSummary AuthToken
  -> p Order AuthorizedOrder
orderSubmission :: forall p :: String p => p Boolean Void -
  > p AuthorizedOrder Boolean
orderSubmissionFailed :: forall p :: Choice p => p Unit Void
  -> p Boolean Unit

submitOrder :: Strong p => Choice p => p Order Void
submitOrder = Semigroupoid.do
  containedButton { label: "Submit order" }
  authorization $ simpleDialog { title: "Authorization" } Sum.do
    caption Sum.do
      staticText "Order summary: "
      summary text
    filledTextField { floatingLabel: "Authorization token" }
  orderSubmission indeterminateLinearProgress
  orderSubmissionFailed $ MDC.snackbar $ staticText "Order submission failed"
  MDC.snackbar $ staticText "Order submitted"
```
---
## Business/design separation is possible with optics

Given
```
type Optic p s t a b = p a b -> p s t
```

```
type Lens s t a b  = forall p. Strong p => Optic p s t a b
type Prism s t a b = forall p. Choice p => Optic p s t a b
(...)
```

deal with business, design is transparent

`type Ocular p      = forall a b. Optic p a b a b`

deals with design, business is transparent

---
## Business/design separation is possible with optics

```
module Business where
firstName :: Lens String String Person Person
lastName  :: Lens String String Person Person
```
```
module Design where
textInput :: UI TheDesignSystem String String
panel     :: Ocular (UI TheDesignSystem)
```
```
module UI where
import Business
import Design
personForm :: UI TheDesignSystem Person Person
personForm =
  panel Endo.do
    firstName textInput
    lastName textInput
```

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

