-- The demos' holey twins (scripts/holes.mjs, guardrails L18) over the library.
let conf = ./spago.dhall

in  conf // { sources = conf.sources # [ ".holey/src/**/*.purs" ] }
