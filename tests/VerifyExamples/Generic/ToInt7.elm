module VerifyExamples.Generic.ToInt7 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Generic exposing (..)







spec7 : Test.Test
spec7 =
    Test.test "#toInt: \n\n    toInt (Bool False)\n    --> Just 0" <|
        \() ->
            Expect.equal
                (
                toInt (Bool False)
                )
                (
                Just 0
                )