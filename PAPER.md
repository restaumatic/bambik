# Lifting optics

`Lens a b s t` denotes relation of `a` being part of `s` and `b` being modificator of `s` into `t`.
In particular, `Lens a a s s ` denotes `a` "is part of" `s` relation.

"Lifting is a concept which allows you to transform a function into a corresponding function within another (usually more general) setting." - https://wiki.haskell.org/Lifting

Just like functor `F` lifts `a -> b` to `F a -> F b`,
applicative functor `F` lifts `a -> b -> c` to `F a -> F b -> F c`:
profunctor `P` lifts adapter to `P a b -> P s t` function,
strong profunctor `P` lifts lens to `P a b -> P s t` function,
choice profunctor `P` lifts prism to `P a b -> P s t` function.

# Lifting other things

We look for other things `X` can lift.
... `X a a -> X a a -> X a a` function?
... `X a b -> X b c -> X a c` function?
... `X a b -> X b a -> X a a` function?
... `X a a -> X [a] [a]` function?
... `X a a -> X (Maybe a) (Maybe a)` function?
... `X a a -> X (f a) (f a)` function?
