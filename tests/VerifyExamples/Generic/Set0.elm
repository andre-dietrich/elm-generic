module VerifyExamples.Generic.Set0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Generic exposing (..)
import Generic as Gen
import Generic.Json as Json







spec0 : Test.Test
spec0 =
    Test.test "#set: \n\n    \"{\\\"type\\\": [1, [2, \\\"tada\\\"] ]}\"\n        |> Json.decodeString\n        |> Gen.set [Gen.String \"type\", Gen.Int 11, Gen.Int 11] (Gen.Int 9999)\n        |> Json.encode\n        |> Json.toString 0\n        |> (==) \"{\\\"type\\\":[1,[2,\\\"tada\\\"]]}\"\n    --> True" <|
        \() ->
            Expect.equal
                (
                "{\"type\": [1, [2, \"tada\"] ]}"
                    |> Json.decodeString
                    |> Gen.set [Gen.String "type", Gen.Int 11, Gen.Int 11] (Gen.Int 9999)
                    |> Json.encode
                    |> Json.toString 0
                    |> (==) "{\"type\":[1,[2,\"tada\"]]}"
                )
                (
                True
                )