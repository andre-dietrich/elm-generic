module VerifyExamples.Generic.ToInt2 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Generic exposing (..)







spec2 : Test.Test
spec2 =
    Test.test "#toInt: \n\n    toInt (String \"\")\n    --> Nothing" <|
        \() ->
            Expect.equal
                (
                toInt (String "")
                )
                (
                Nothing
                )