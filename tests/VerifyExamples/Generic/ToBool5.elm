module VerifyExamples.Generic.ToBool5 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Generic exposing (..)







spec5 : Test.Test
spec5 =
    Test.test "#toBool: \n\n    toBool (Int -1)\n    --> True" <|
        \() ->
            Expect.equal
                (
                toBool (Int -1)
                )
                (
                True
                )