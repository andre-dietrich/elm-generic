module VerifyExamples.Generic.ToFloat5 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Generic exposing (..)
import Generic as Gen







spec5 : Test.Test
spec5 =
    Test.test "#toFloat: \n\n    Gen.toFloat (Float 0.0)\n    --> Just 0.0" <|
        \() ->
            Expect.equal
                (
                Gen.toFloat (Float 0.0)
                )
                (
                Just 0.0
                )