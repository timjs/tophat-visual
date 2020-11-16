{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "array-views"
  , "arrays"
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
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
