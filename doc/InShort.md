# 1. Lenses generalized to optics

Lenses, prisms etc are useful for navigation through data structures.

But they are heteregenous and not composable.

# 2. Profunctor optics

Profunctor optics encodes optics in homogenous and composable way.

But how powerful profunctor optics really is in navigating through data structures?

# 3. The power of optics

Products, Co-products, isomorphisms, functions...

Optics describes the structure of data without releaving its representation (data constructors, functions etc)

What if we start using optics instead of representation (data constructors, functions etc)?

Before trying this, let's make one observation.

# 4. Invariants - subclass of profunctors

When describing the structure of data, profunctors we use are specific class of profunctors where input and output types are the same...
Invariant optics seems more handy yet equally expressive (in our case) than profuncor optics.

Let's pick invariant optics and use them instead of data representation.

# 5. Representation independence

Suppose we're given with:

```
module Business (Person, name) where
-- data
data Person = Person { name :: String }
-- optics
name :: Cartesian i => i String -> i Person -- notice no name conflict with name field
```

and using this module we want to implement function:


```
printPersonToConsole :: Person -> Effect Unit
```

Since only `Person` (data type without data constructor) and optics are exported, we only can:
  * pick invariant, arbirary one but necessarily cartesian (1)
  * use `String` as usual (2)

Attempt 1:

```
import Business (Person, name)

printPersonToConsole = Console.log <<< personString

personString :: Person -> String
personString = impossible
```

Attempt 2:

```
import Business (Person, name)

-- introducing invariant:

data ConsolePrint a = ...
instance Invariant ConsolePrint where ...
instance Cartesian ConsolePrint where ...

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

# 6. Invariant UI
