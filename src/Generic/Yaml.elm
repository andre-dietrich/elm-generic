module Generic.Yaml exposing
    ( decode
    , encode
    , toString
    )

import Dict
import Generic
import GenericDict
import Yaml.Decode as YD
import Yaml.Encode as YE


decode : String -> Result YD.Error Generic.Value
decode =
    YD.fromString decoder


decoder : YD.Decoder Generic.Value
decoder =
    YD.oneOf
        [ YD.map (\_ -> Generic.Null) YD.null
        , YD.map Generic.Bool YD.bool
        , YD.map Generic.Int YD.int
        , YD.map Generic.Float YD.float
        , YD.map Generic.String YD.string
        , YD.map Generic.List (YD.list (YD.lazy (\_ -> decoder)))
        , YD.map
            (Dict.toList
                >> List.map (\( key, value ) -> ( Generic.String key, value ))
                >> Generic.toDict
                >> Generic.Dict
            )
            (YD.dict (YD.lazy (\_ -> decoder)))
        ]


encode : Generic.Value -> YE.Encoder
encode generic =
    case generic of
        Generic.Bool duck ->
            YE.bool duck

        Generic.Int duck ->
            YE.int duck

        Generic.Float duck ->
            YE.float duck

        Generic.String duck ->
            YE.string duck

        Generic.List duck ->
            YE.list encode duck

        Generic.Set _ ->
            generic
                |> Generic.toList
                >> Generic.List
                >> encode

        Generic.Dict duck ->
            duck
                |> GenericDict.toList
                |> List.map (\( key, value ) -> ( Generic.toString key |> Maybe.withDefault "null", value ))
                |> Dict.fromList
                |> YE.dict identity encode

        _ ->
            YE.null


toString : Int -> YE.Encoder -> String
toString =
    YE.toString