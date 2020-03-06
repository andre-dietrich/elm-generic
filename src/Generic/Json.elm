module Generic.Json exposing
    ( decodeString
    , decodeValue
    , decoder
    , encode
    )

import Base64
import Generic as Gen
import GenericDict as Dict
import Iso8601
import Json.Decode as JD
import Json.Encode as JE


decodeString : String -> Gen.Type
decodeString =
    JD.decodeString decoder >> Result.withDefault Gen.Null


decodeValue : JD.Value -> Gen.Type
decodeValue =
    JD.decodeValue decoder >> Result.withDefault Gen.Null


decoder : JD.Decoder Gen.Type
decoder =
    JD.oneOf
        [ JD.map (\_ -> Gen.Null) (JD.null False)
        , JD.map Gen.Bool JD.bool
        , JD.map Gen.Int JD.int
        , JD.map Gen.Float JD.float
        , JD.map string JD.string
        , JD.map Gen.List (JD.list (JD.lazy (\_ -> decoder)))
        , JD.map
            (List.map (\( key, value ) -> ( Gen.String key, value ))
                >> Gen.toDict
                >> Gen.Dict
            )
            (JD.keyValuePairs (JD.lazy (\_ -> decoder)))
        ]


string : String -> Gen.Type
string str =
    case Iso8601.toTime str of
        Result.Ok posix ->
            if String.contains "T" str then
                Gen.DateTime posix

            else
                Gen.Date posix

        Result.Err _ ->
            Gen.String str


encode : Gen.Type -> JE.Value
encode generic =
    case generic of
        Gen.Null ->
            JE.null

        Gen.Bool duck ->
            JE.bool duck

        Gen.Int duck ->
            JE.int duck

        Gen.Float duck ->
            JE.float duck

        Gen.String duck ->
            JE.string duck

        Gen.List duck ->
            JE.list encode duck

        Gen.Set _ ->
            generic
                |> Gen.toList
                >> Gen.List
                >> encode

        Gen.Dict duck ->
            duck
                |> Dict.toList
                |> List.map (\( key, value ) -> ( Gen.toString key |> Maybe.withDefault "null", encode value ))
                |> JE.object

        Gen.Date duck ->
            Iso8601.encode duck

        Gen.DateTime duck ->
            Iso8601.encode duck

        Gen.Binary duck ->
            duck
                |> Base64.fromBytes
                |> Maybe.withDefault ""
                |> JE.string
