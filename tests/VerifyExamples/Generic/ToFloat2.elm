module VerifyExamples.Generic.ToFloat2 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Generic exposing (..)
import Generic as Gen







spec2 : Test.Test
spec2 =
    Test.test "#toFloat: \n\n    Gen.toFloat (String \"33\")\n    --> Just 33" <|
        \() ->
            Expect.equal
                (
                Gen.toFloat (String "33")
                )
                (
                Just 33
                )