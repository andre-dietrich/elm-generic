module VerifyExamples.Generic.ToInt9 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Generic exposing (..)







spec9 : Test.Test
spec9 =
    Test.test "#toInt: \n\n    toInt Null\n    --> Nothing" <|
        \() ->
            Expect.equal
                (
                toInt Null
                )
                (
                Nothing
                )