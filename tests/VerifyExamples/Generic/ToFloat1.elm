module VerifyExamples.Generic.ToFloat1 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Generic exposing (..)
import Generic as Gen







spec1 : Test.Test
spec1 =
    Test.test "#toFloat: \n\n    Gen.toFloat (String \"33 m\")\n    --> Nothing" <|
        \() ->
            Expect.equal
                (
                Gen.toFloat (String "33 m")
                )
                (
                Nothing
                )