{ name = "supabase"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "fetch"
  , "fetch-core"
  , "foldable-traversable"
  , "foreign"
  , "functions"
  , "lists"
  , "maybe"
  , "nullable"
  , "prelude"
  , "react-basic-hooks"
  , "record"
  , "record-studio"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "web-file"
  , "yoga-json"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
