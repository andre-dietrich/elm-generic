module Generic.Yaml exposing (decode)

import Base64
import Generic as Gen
import GenericDict as Dict
import Iso8601
import Yaml.Decode as Yaml



--decode : String -> Gen.Type


decode : String -> Result Yaml.Error Gen.Type
decode str =
    Yaml.fromString decoder str


decoder : Yaml.Decoder Gen.Type
decoder =
    Yaml.oneOf
        [ Yaml.map (\_ -> Gen.Null) Yaml.null
        , Yaml.map Gen.Bool Yaml.bool
        , Yaml.map Gen.Int Yaml.int
        , Yaml.map Gen.Float Yaml.float
        , Yaml.map Gen.String Yaml.string
        , Yaml.map Gen.List (Yaml.list (Yaml.lazy (\_ -> decoder)))
        , Yaml.map
            (List.map (\( key, value ) -> ( Gen.String key, value ))
                >> Gen.toDict
                >> Gen.Dict
            )
            (Yaml.keyValuePairs (Yaml.lazy (\_ -> decoder)) |> Debug.log "WWWWWWWWWWWWWWWWWWWWWWW")
        ]
