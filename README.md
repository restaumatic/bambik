# Short story

See [**live demo**](http://erykciepiela.xyz/bambik/demo/1/). To peek under the hood open dev tools console and watch logs.

To run the demo locally:

```bash
$ npm install && npm run demo1
```

## Best practices

  1. Use qualified do for Semigroup and Semigroupoid composition of Widgets so there is one-to-one correspondence of widget and line of code.
  1. In widget definition read `#` as *of*, so `text # mail # customer` can be read as *text of a mail of a customer*.
  1. In widget definition read `>>>` as *followed by*, so `dialog >>> save` can be read as *dialog followed by save*.
  1. General lenses/prisms are less performant than specialied `field`/`constructor`
  1. Avoid using `Show` instances when textualizing data in business module in order to 1) make business module resilient to changes in `Show` instance implementation, 2) provide tailor-made textualizations
  1. View modules should not create Widgets just compose them.

## TODOs

  1. Debounced widget input (example: auto-save)
  1. Full set of first-order MDC components
  1. Validation
  1. Collections and Higher-order components (including MDC)
  1. I18n

# Long story

> (!) Notice: this section is still work in progress.

## 1. From lenses to optics

Lenses, prisms, traversals etc, under common name of optics, provide a convenient way for navigating through data structures.

Problem: Concrete (explicit) encoding of optics is heteregenous hence not composable.

## 2. Profunctor optics

Profunctor optics encodes optics in homogenous thus composable way.

Question: Are profunctor optics expressive enough in navigating through data structures?

## 3. The power of optics

  1. Adapter (ChProfunctor) and its sub classes:
    1. Iso
    1. Projection and its sub classes:
      1. Constant
  1. Lens (ChProfunctor+Strong) and its sub classes:
    1. Lens' and its sub classes:
      1. Field
  1. Prism (ChProfunctor+Choice) and its sub classes:
    1. Prism' and its sub classes:
      1. Constructor
  1. Null (ProfunctorZero)

Optics describes the structure of data without releaving its representation (data constructors, functions etc)

What if we start using optics instead of representation (data constructors, functions etc)?

Before trying this, let's make one observation.

## 4. Invariants - subclass of profunctors

When describing the structure of data, profunctors we use are specific class of profunctors where input and output types are the same.
Invariant optics seems more handy at the cost of expressivity.
Still, in our case, invariant optics represent reasonable trade-off.

Let's pick then invariant optics and use them instead of data representation.

## 5. Representation independence

Suppose we're given with:

```purescript
module Business (Person, name) where
-- data
data Person = Person { name :: String }
-- optics
name :: InvCartesian i => i String -> i Person -- notice no name conflict with name field
```

and using this module we want to implement function:


```purescript
printPersonToConsole :: Person -> Effect Unit
```

Since only `Person` (data type without data constructor) and optics are exported, we only can:
  * pick invariant, arbirary one but necessarily cartesian (1)
  * use `String` as usual (2)

Attempt 1:

```purescript
import Business (Person, name)

printPersonToConsole = Console.log <<< personString

personString :: Person -> String
personString = impossible
```

Attempt 2:

```purescript
import Business (Person, name)

-- introducing invariant:

data ConsolePrint a = ...
instance Invariant ConsolePrint where ...
instance InvCartesian ConsolePrint where ...

consoleStringPrint :: ConsolePrint String
consoleStringPrint = ... -- using String as usual (2)

runConsolePrint :: ConsolePrint a -> a -> Effect Unit
runConsolePrint = ...

-- and then:

printPersonToConsole :: Person -> Effect Unit
printPersonToConsole = runConsolePrint consolePersonPrint -- picking invariant (1)
  where
    consolePersonPrint :: ConsolePrint Person
    consolePersonPrint = name consoleStringPrint

```

Having done that, we decoupled representation from application, with optics being an "API" between the two.
Optics specifies what application can and can not do over representated data: application can not do anything beyond optics over the data.
And vice-versa, data cannot do anything beyond optics over application.
This sounds like a clear separation between the data and application.

And what is this `ConsolePrint a` invariant?
We can interpret it is a representation of `a` in given application: printing to console in this case.
Does it mean that invariant represents data in a given application?
Does it mean that optics represents binding between data and its applications?

## 6. Invariant UI


InvPlusoid and InvPlus.

## 7. Back to profunctors

As we observed before, invariant optics are less expressive than profunctor optics.
A component that doesn't produce any output but still consumes input can
trigger effects on input but no output is made available to other components.

That is not expressible with invariants so let's get back to profunctors.

All invariant optics have their counterparts in profunctor optics (not surprisingly as invariants are special case of profunctors).
Underlying concepts of InvCartesian, InvCocartesian work in the same way as in invariants (they were in fact ported from profunctors to invariants in the first place).

Is this still the case for InvPlusoid and InvPlus? They were not derived from profunctor world so do they have profunctor counterparts?
If they don't then we are not able to build anything beyond single component UIs.

Profunctor can only be combinable if their input and output are of the same type.  

# 8. Lifting optics

`Lens a b s t` denotes relation of `a` being part of `s` and `b` being modificator of `s` into `t`.
In particular, `Lens a a s s ` denotes `a` "is part of" `s` relation.

"Lifting is a concept which allows you to transform a function into a corresponding function within another (usually more general) setting." - https://wiki.haskell.org/Lifting

Just like functor `F` lifts `a -> b` to `F a -> F b`,
applicative functor `F` lifts `a -> b -> c` to `F a -> F b -> F c`:
profunctor `P` lifts adapter to `P a b -> P s t` function,
strong profunctor `P` lifts lens to `P a b -> P s t` function,
choice profunctor `P` lifts prism to `P a b -> P s t` function.

# Lifting other things
We look for other things `X` can lift, e.g.
... `X a a -> X (f a) (f a)` function?

and:
... `X a a -> X a a -> X a a` function?
... `X a b -> X b c -> X a c` function?
... `X a b -> X b a -> X a a` function?
... `X a a -> X [a] [a]` function?
... `X a a -> X (Maybe a) (Maybe a)` function?
