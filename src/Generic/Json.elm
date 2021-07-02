module Generic.Json exposing
    ( decode, encode, toString
    , decodeString, decodeValue
    )

{-|

@docs decode, encode, toString

@docs decodeString, decodeValue

-}

import Generic
import GenericDict as Dict
import Iso8601
import Json.Decode as JD
import Json.Encode as JE


{-| A simple alias for `Json.Encode.encode` that outputs a String fo every Json value.
-}
toString : Int -> JE.Value -> String
toString =
    JE.encode


{-| Generic decoder function for any kind of Json value.

    import Generic as Gen
    import Generic.Json as Json

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
        --> Just True

-}
decode : String -> Result String Generic.Value
decode =
    JD.decodeString decoder
        >> Result.mapError (always "Not a valid Json")


{-| Generic encoder, which will translate any kind of `Generic.Value` back into a Json value.

    import Generic as Gen
    import Generic.Json as Json

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
        |> Gen.set [Gen.String "Inhaber", Gen.String "maennlich"] (Gen.String "divers")
        |> Json.encode
        |> Json.toString 2
        --> "{\n  \"Deckung\": 2000000,\n  \"Herausgeber\": \"Xema\",\n  \"Inhaber\": {\n    \"Alter\": 42,\n    \"Hobbys\": [\n      \"Reiten\",\n      \"Golfen\",\n      \"Lesen\"\n    ],\n    \"Kinder\": [],\n    \"Name\": \"Mustermann\",\n    \"Partner\": null,\n    \"Vorname\": \"Max\",\n    \"maennlich\": \"divers\"\n  },\n  \"Nummer\": \"1234-5678-9012-3456\",\n  \"Waehrung\": \"EURO\"\n}"

-}
encode : Generic.Value -> JE.Value
encode generic =
    case generic of
        Generic.Null ->
            JE.null

        Generic.Bool duck ->
            JE.bool duck

        Generic.Int duck ->
            JE.int duck

        Generic.Float duck ->
            JE.float duck

        Generic.String duck ->
            JE.string duck

        Generic.List duck ->
            JE.list encode duck

        Generic.Set _ ->
            generic
                |> Generic.toList
                >> Generic.List
                >> encode

        Generic.Dict duck ->
            duck
                |> Dict.toList
                |> List.map (\( key, value ) -> ( Generic.toString key |> Maybe.withDefault "null", encode value ))
                |> JE.object

        Generic.Date duck ->
            Iso8601.encode duck

        Generic.DateTime duck ->
            Iso8601.encode duck


{-| Just a convenience functions, that decodes a Json string. If not possible, the result will be a generic `Null`.
-}
decodeString : String -> Generic.Value
decodeString =
    decode >> Result.withDefault Generic.Null


{-| Just a convenience functions, that decodes a Json value. If not possible, the result will be a generic `Null`.
-}
decodeValue : JD.Value -> Generic.Value
decodeValue =
    JD.decodeValue decoder >> Result.withDefault Generic.Null


decoder : JD.Decoder Generic.Value
decoder =
    JD.oneOf
        [ JD.map (\_ -> Generic.Null) (JD.null False)
        , JD.map Generic.Bool JD.bool
        , JD.map Generic.Int JD.int
        , JD.map Generic.Float JD.float
        , JD.map string JD.string
        , JD.map Generic.List (JD.list (JD.lazy (\_ -> decoder)))
        , JD.map
            (List.map (\( key, value ) -> ( Generic.String key, value ))
                >> Generic.toDict
                >> Generic.Dict
            )
            (JD.keyValuePairs (JD.lazy (\_ -> decoder)))
        ]


string : String -> Generic.Value
string str =
    case Iso8601.toTime str of
        Result.Ok posix ->
            if String.contains "T" str then
                Generic.DateTime posix

            else
                Generic.Date posix

        Result.Err _ ->
            Generic.String str
