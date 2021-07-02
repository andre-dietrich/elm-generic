module VerifyExamples.Generic.ToFloat4 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Generic exposing (..)
import Generic as Gen







spec4 : Test.Test
spec4 =
    Test.test "#toFloat: \n\n    Gen.toFloat (Float 3.141592)\n    --> Just 3.141592" <|
        \() ->
            Expect.equal
                (
                Gen.toFloat (Float 3.141592)
                )
                (
                Just 3.141592
                )