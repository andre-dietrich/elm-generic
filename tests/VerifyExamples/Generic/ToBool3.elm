module VerifyExamples.Generic.ToBool3 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Generic exposing (..)







spec3 : Test.Test
spec3 =
    Test.test "#toBool: \n\n    toBool (Float 3.141592)\n    --> True" <|
        \() ->
            Expect.equal
                (
                toBool (Float 3.141592)
                )
                (
                True
                )