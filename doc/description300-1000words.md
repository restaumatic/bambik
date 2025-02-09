Profunctors are well researched abstractions in category theory and are widely used in functional programming.
They turn out to be closely related to optics (lenses, prisms etc) providing them an encoding (profunctor encoding).
They are also close relatives to arrows (which are strong profunctor categories).
They, therefore, form a common basis for both data and computation structure encoding.

This work presents a way in which user interfaces can be entirely expressed declaratively, in terms of both statics and dynamics, by picking a proper profunctor instance as a building block of an interface.
Strong profunctor, choice profunctor, semigroupoid profunctor, sum profunctor and zero profunctor alongside semigroup and monoid instances suffice to enable rich UIs and great UI development experience.

The idea of profunctor user interfaces is prototyped as a PureScript library Bambik (https://github.com/restaumatic/bambik) that allows for creating Web UIs with Material Design Components.
