module VerifyExamples.Generic.TypeOf1 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Generic exposing (..)







spec1 : Test.Test
spec1 =
    Test.test "#typeOf: \n\n    typeOf (Float 1.234)\n    --> \"float\"" <|
        \() ->
            Expect.equal
                (
                typeOf (Float 1.234)
                )
                (
                "float"
                )