module VerifyExamples.Generic.ToString4 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Generic exposing (..)







spec4 : Test.Test
spec4 =
    Test.test "#toString: \n\n    toString (String \"\")\n    --> Just \"\"" <|
        \() ->
            Expect.equal
                (
                toString (String "")
                )
                (
                Just ""
                )