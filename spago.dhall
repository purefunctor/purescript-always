{ name = "always"
, dependencies =
  [ "hyrule", "leibniz", "prelude", "profunctor", "unsafe-coerce" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
