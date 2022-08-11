let conf = ./spago.dhall

in      conf
    //  { sources = conf.sources # [ "test/**/*.purs" ]
        , dependencies =
            conf.dependencies # [ "aff", "effect", "refs", "spec", "st" ]
        }
