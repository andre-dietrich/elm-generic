module VerifyExamples.Generic.ToInt6 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Generic exposing (..)







spec6 : Test.Test
spec6 =
    Test.test "#toInt: \n\n    toInt (Int 0)\n    --> Just 0" <|
        \() ->
            Expect.equal
                (
                toInt (Int 0)
                )
                (
                Just 0
                )