{ name = "TopHat Visual"
, dependencies =
  [ "arrays"
  , "concur-core"
  , "concur-react"
  , "console"
  , "debug"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "lists"
  , "maybe"
  , "newtype"
  , "numbers"
  , "pprint"
  , "psci-support"
  , "run"
  , "strings"
  , "tuples"
  , "unfoldable"
  , "unordered-collections"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
