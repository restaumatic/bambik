# 1. Lenses generalized to optics

Optics are useful for navigation through data structures

But they are heteregenous and not composable.

# 2. Profunctor optics

Profunctor optics encodes optics in homogenous and composable way.

But how powerful profunctor optics really is in navigating through data structures?

# 3. The power of optics

Products, Co-products, isomorphisms, functions...

Optics describes the structure of data without releaving its representation (data constructors, functions etc)

What if we start using optics instead of representation (data constructors, functions etc)?
Before that, let's make one observation.

# 4. Invariants - subclass of profunctors

When describing the structure of data, the profunctors we use are specific class of profunctors where input and output types are the same...

Invariant optics seems more handy yet equally expressive (in our case) than profuncor optics.
Let's pick invariant optics and use them instead of representation.

# 5. UI independent of representation

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
module UI where

import Business (Person, name)

printPersonToConsole = Console.log <<< personString

personString :: Person -> String
personString = impossible
```

Attempt 2:

```
module UI where

import Business (Person, name)

-- introducing invariant:

data ConsolePrint = ...
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

