{ name = "always"
, dependencies =
  [ "hyrule", "leibniz", "prelude", "profunctor", "st", "unsafe-coerce" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
