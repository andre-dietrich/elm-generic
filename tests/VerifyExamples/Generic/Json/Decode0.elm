module VerifyExamples.Generic.Json.Decode0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Generic.Json exposing (..)
import Generic.Json as Json
import Generic as Gen







spec0 : Test.Test
spec0 =
    Test.test "#decode: \n\n    \"\"\"{\n        \"Herausgeber\": \"Xema\",\n        \"Nummer\": \"1234-5678-9012-3456\",\n        \"Deckung\": 2e+6,\n        \"Waehrung\": \"EURO\",\n        \"Inhaber\":\n        {\n            \"Name\": \"Mustermann\",\n            \"Vorname\": \"Max\",\n            \"maennlich\": true,\n            \"Hobbys\": [\"Reiten\", \"Golfen\", \"Lesen\"],\n            \"Alter\": 42,\n            \"Kinder\": [],\n            \"Partner\": null\n        }\n    }\"\"\"\n        |> Json.decode\n        |> Result.withDefault Gen.Null\n        |> Gen.get [Gen.String \"Inhaber\", Gen.String \"maennlich\"]\n        |> Maybe.map (Gen.toBool)\n    --> Just True" <|
        \() ->
            Expect.equal
                (
                """{
                    "Herausgeber": "Xema",
                    "Nummer": "1234-5678-9012-3456",
                    "Deckung": 2e+6,
                    "Waehrung": "EURO",
                    "Inhaber":
                    {
                        "Name": "Mustermann",
                        "Vorname": "Max",
                        "maennlich": true,
                        "Hobbys": ["Reiten", "Golfen", "Lesen"],
                        "Alter": 42,
                        "Kinder": [],
                        "Partner": null
                    }
                }"""
                    |> Json.decode
                    |> Result.withDefault Gen.Null
                    |> Gen.get [Gen.String "Inhaber", Gen.String "maennlich"]
                    |> Maybe.map (Gen.toBool)
                )
                (
                Just True
                )