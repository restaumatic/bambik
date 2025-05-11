70 sentences?

# Problem

# State of the Art

FRP - unidirectional binding

# Solution

or:

# Interesting facts

Arrow = Strong Profunctor Category
ArrowChoice = Strong/Choice Profunctor Category

Let's take ArrowChoice:

first :: a b c -> a (b, d) (c, d) (from Arrow)
left :: a b c -> a (Either b d) (Either c d) (from ArrowChoice)
  - gives also because of being semigroupoid: (+++) :: a b c -> a b' c' -> a (Either b b') (Either c c')

For data flow suffices the `Arrow`+`ArrowChoice`-`id`-`arr`+`dimap` which is `Profunctor`+`StrongProfunctor`+`ChoiceProfunctor`+`Semigroupoid`:
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d (from Profunctor)
  first :: a b c -> a (b, d) (c, d) (from StrongProfunctor)
  left :: a b c -> a (Either b d) (Either c d) (from ChoiceProfunctor)
  (>>>) :: a b c -> a c d -> a b d (from Semigroupoid)
with all profunctor laws https://hackage.haskell.org/package/profunctors-5.6.2/docs/Data-Profunctor.html and associativity law

1. Profunctor laws
dimap id id = id
dimap (f' · f) (g · g') = dimap f g · dimap f' g'
=> of course they hold

2. Strong profunctor laws

first ≡ dimap swap swap . second -- ok
lmap fst ≡ rmap fst . first -- ok
lmap (second f) . first ≡ rmap (second f) . first -- no idea what's that
first . first ≡ dimap assoc unassoc . first where -- ok
  assoc ((a,b),c) = (a,(b,c))
  unassoc (a,(b,c)) = ((a,b),c)

second ≡ dimap swap swap . first -- ok
lmap snd ≡ rmap snd . second -- ok
lmap (first f) . second ≡ rmap (first f) . second -- no idea what's that
second . second ≡ dimap unassoc assoc . second where -- ok
  assoc ((a,b),c) = (a,(b,c))
  unassoc (a,(b,c)) = ((a,b),c)

3. choice profunctor laws

left ≡ dimap swapE swapE . right where -- ok
  swapE :: Either a b -> Either b a
  swapE = either Right Left
rmap Left ≡ lmap Left . left -- ok
lmap (right f) . left ≡ rmap (right f) . left -- no idea what's that
left . left ≡ dimap assocE unassocE . left where -- ok
  assocE :: Either (Either a b) c -> Either a (Either b c)
  assocE (Left (Left a)) = Left a
  assocE (Left (Right b)) = Right (Left b)
  assocE (Right c) = Right (Right c)
  unassocE :: Either a (Either b c) -> Either (Either a b) c
  unassocE (Left a) = Left (Left a)
  unassocE (Right (Left b)) = Left (Right b)
  unassocE (Right (Right c)) = Right c

right ≡ dimap swapE swapE . left where -- ok
  swapE :: Either a b -> Either b a
  swapE = either Right Left
rmap Right ≡ lmap Right . right -- ok
lmap (left f) . right ≡ rmap (left f) . right -- no idea what's that
right . right ≡ dimap unassocE assocE . right where -- ok
  assocE :: Either (Either a b) c -> Either a (Either b c)
  assocE (Left (Left a)) = Left a
  assocE (Left (Right b)) = Right (Left b)
  assocE (Right c) = Right (Right c)
  unassocE :: Either a (Either b c) -> Either (Either a b) c
  unassocE (Left a) = Left (Left a)
  unassocE (Right (Left b)) = Left (Right b)
  unassocE (Right (Right c)) = Right c

4. Semiegoupoing laws
p a b >>> (p b c >>> p c d) ≡ (p a b >>> p b c) >>> p c d

you can get (+++) :: ArrowChoice a => a b c -> a b' c' -> a (Either b b') (Either c c') from left having arr
you can get (+++) :: ProfunctorChoice a => a b c -> a b' c' -> a (Either b b') (Either c c') from left/right/bimap/>>>
but getting left from (+++) requires id
you can get (***) :: Arrow a => a b c -> a b' c' -> a (b,b') (c,c') from first having arr
you can get (***) :: ProfunctorStrong a => a b c -> a b' c' -> a (b,b') (c,c') from first/second/bimap/>>>
but getting first from (***) requires id

Or maybe:
For data flow suffices the `Arrow`+`ArrowChoice`-`arr`+`dimap` which is `Profunctor`+`StrongProfunctor`+`ChoiceProfunctor`+`Category`
But do we really need `Category.id` while we can:

id :: forall p a. Choice p => Zero p => p a a
id = dimap Right (either absurd identity) (left pzero)

So maybe:
For data flow suffices the `Arrow`+`ArrowChoice`-`arr`+`dimap` which is `Profunctor`+`StrongProfunctor`+`ChoiceProfunctor`+`SumProfunctor`+`ZeroProfunctor`
and then:

id :: forall p a. Choice p => Zero p => p a a
id = dimap Right (either absurd identity) (left pzero)

it's like an id from pzero.

So, having `left` makes `id` not needed? With `left` we can do skipping steps. For what else than `left` we would need `id`?
Having `id` on the other hand is dangerous when `mappending` (infinite update loop).





Adapter = Profunctor modifier (that converts inputs and outputs)
Lens = Strong Profunctor modifier (that zooms)
Prism = Choice Profunctor modifier (that handles cases)


# References

Guillaume Boisseau
St Anne’s College
University of Oxford
A thesis submitted for the degree of
MSc in Computer Science
Trinity 2017
https://arxiv.org/pdf/2001.11816
- "isomorphism" optics encoding, similar to existentional optics encoding?

Modular data accessors
https://arxiv.org/pdf/1703.10857


# Notions
Functional References
In functional programming, "functional references" (often called "optics") are abstractions that provide a composable way to access and modify parts of immutable data structures.

Key Characteristics
Immutability-friendly: They work with immutable data, creating new versions rather than modifying in place
Compositional: Can be combined to access deeply nested structures
First-class: Can be passed as arguments, returned from functions, etc.
