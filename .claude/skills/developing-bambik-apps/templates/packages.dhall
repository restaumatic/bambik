let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.10-20231023/packages.dhall
        sha256:b9a482e743055ba8f2d65b08a88cd772b59c6e2084d0e5ad854025fa90417fd4

in  upstream
  with variant.repo = "https://github.com/erykciepiela/purescript-variant.git"
  with variant.version = "v8.0.0-prim-variant.1"
  with convertable-options =
    { dependencies = [ "console", "effect", "maybe", "record" ]
    , repo = "https://github.com/natefaubion/purescript-convertable-options.git"
    , version = "v1.0.0"
    }
