{ name = "always"
, dependencies =
  [ "hyrule"
  , "identity"
  , "prelude"
  , "profunctor"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
