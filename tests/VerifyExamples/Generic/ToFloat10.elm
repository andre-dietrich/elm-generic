module VerifyExamples.Generic.ToFloat10 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Generic exposing (..)
import Generic as Gen







spec10 : Test.Test
spec10 =
    Test.test "#toFloat: \n\n    Gen.toFloat Null\n    --> Nothing" <|
        \() ->
            Expect.equal
                (
                Gen.toFloat Null
                )
                (
                Nothing
                )