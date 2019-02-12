{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "bs"
, dependencies =
    [ "console"
    , "effect"
    , "node-fs"
    , "node-path"
    , "prelude"
    , "psci-support"
    , "test-unit"
    ]
, packages =
    ./packages.dhall
}
