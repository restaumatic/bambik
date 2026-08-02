{-
The bambik library is compiled from the sibling clone: legacy spago resolves
a Location package's own `sources` glob inside that package's directory
(leading `../` segments are stripped), so bambik's src/ is reached by this
config's glob instead, and the dependency list is inherited from the clone's
spago.dhall — so it never drifts from the library's. Extra app-only
dependencies append: `(../bambik/spago.dhall).dependencies # [ "argonaut" ]`.
-}
{ name = "myapp"
, dependencies = (../bambik/spago.dhall).dependencies
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "../bambik/src/**/*.purs" ]
}
