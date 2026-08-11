{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "avar"
  , "console"
  , "convertable-options"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "integers"
  , "lists"
  , "maybe"
  , "numbers"
  , "ordered-collections"
  , "newtype"
  , "prelude"
  , "profunctor"
  , "profunctor-lenses"
  , "qualified-do"
  , "random"
  , "record"
  , "refs"
  , "strings"
  , "transformers"
  , "tuples"
  , "unsafe-coerce"
  , "variant"
  ]
, packages = ./packages.dhall
, sources =
  [ "src/**/*.purs"
  , "extras/**/*.purs"
  , "test/**/*.purs"
  , "demo/7guis/**/*.purs"
  , "demo/nguis/**/*.purs"
  ]
}
