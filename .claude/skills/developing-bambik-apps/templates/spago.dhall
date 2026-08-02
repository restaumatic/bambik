{ name = "myapp"
, dependencies = [ "bambik", "effect", "prelude", "qualified-do", "variant" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
