module VerifyExamples.Generic.ToString11 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Generic exposing (..)







spec11 : Test.Test
spec11 =
    Test.test "#toString: \n\n    toString Null\n    --> Nothing" <|
        \() ->
            Expect.equal
                (
                toString Null
                )
                (
                Nothing
                )