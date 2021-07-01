module Generic.Json exposing
    ( decodeString
    , decodeValue
    , decoder
    , encode
    , toString
    )

import Generic
import GenericDict as Dict
import Iso8601
import Json.Decode as JD
import Json.Encode as JE


toString : Int -> JE.Value -> String
toString =
    JE.encode


decodeString : String -> Generic.Value
decodeString =
    JD.decodeString decoder >> Result.withDefault Generic.Null


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
