module VerifyExamples.Generic.Get1 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Generic exposing (..)
import Generic.Decoder exposing (decode)







spec1 : Test.Test
spec1 =
    Test.test "#get: \n\n    \"{\\\"type\\\": [1, [2, \\\"tada\\\"] ]}\"\n        |> decode\n        |> Result.withDefault Null\n        |> get [String \"type\", Int 1, Int 1]\n        |> Maybe.andThen toString\n        |> (==) (Just \"tada\")\n    --> True" <|
        \() ->
            Expect.equal
                (
                "{\"type\": [1, [2, \"tada\"] ]}"
                    |> decode
                    |> Result.withDefault Null
                    |> get [String "type", Int 1, Int 1]
                    |> Maybe.andThen toString
                    |> (==) (Just "tada")
                )
                (
                True
                )