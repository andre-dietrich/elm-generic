module VerifyExamples.Generic.ToFloat9 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Generic exposing (..)
import Generic as Gen







spec9 : Test.Test
spec9 =
    Test.test "#toFloat: \n\n    Gen.toFloat (Bool True)\n    --> Just 1.0" <|
        \() ->
            Expect.equal
                (
                Gen.toFloat (Bool True)
                )
                (
                Just 1.0
                )