module VerifyExamples.Generic.ToString2 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Generic exposing (..)







spec2 : Test.Test
spec2 =
    Test.test "#toString: \n\n    toString (String \"33 m\")\n    --> Just \"33 m\"" <|
        \() ->
            Expect.equal
                (
                toString (String "33 m")
                )
                (
                Just "33 m"
                )