module VerifyExamples.Generic.ToBool4 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Generic exposing (..)







spec4 : Test.Test
spec4 =
    Test.test "#toBool: \n\n    toBool (Float 0.0)\n    --> False" <|
        \() ->
            Expect.equal
                (
                toBool (Float 0.0)
                )
                (
                False
                )