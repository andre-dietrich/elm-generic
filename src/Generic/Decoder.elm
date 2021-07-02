module Generic.Decoder exposing (decode)

{-|

@docs decode

-}

import Generic
import Generic.Json as Json
import Generic.Xml as Xml
import Generic.Yaml as Yaml


{-| Use this general type of decoder, it will try out at first to parse a JSON format, then XML, and finally YAML

    import Generic

    """
    <verzeichnis>
        <titel>Wikipedia Städteverzeichnis</titel>
        <eintrag>
            <stichwort>Genf</stichwort>
            <eintragstext>Genf ist der Sitz von ...</eintragstext>
        </eintrag>
        <eintrag>
            <stichwort>Köln</stichwort>
            <eintragstext>Köln ist eine Stadt, die ...</eintragstext>
        </eintrag>
    </verzeichnis>
    """
        |> decode
        |> Result.withDefault Generic.Null
        |> Generic.toString
        --> Just "[{\"verzeichnis\":[{\"titel\":\"Wikipedia Städteverzeichnis\"},{\"eintrag\":[{\"stichwort\":\"Genf\"},{\"eintragstext\":\"Genf ist der Sitz von ...\"}]},{\"eintrag\":[{\"stichwort\":\"Köln\"},{\"eintragstext\":\"Köln ist eine Stadt, die ...\"}]}]}]"


    """
    [
        {
            "verzeichnis": [
            {
                "titel": "Wikipedia Städteverzeichnis"
            },
            {
                "eintrag": [
                {
                    "stichwort": "Genf"
                },
                {
                    "eintragstext": "Genf ist der Sitz von ..."
                }
                ]
            },
            {
                "eintrag": [
                {
                    "stichwort": "Köln"
                },
                {
                    "eintragstext": "Köln ist eine Stadt, die ..."
                }
                ]
            }
            ]
        }
    ]
    """
        |> decode
        |> Result.withDefault Generic.Null
        |> Generic.toString
        --> Just "[{\"verzeichnis\":[{\"titel\":\"Wikipedia Städteverzeichnis\"},{\"eintrag\":[{\"stichwort\":\"Genf\"},{\"eintragstext\":\"Genf ist der Sitz von ...\"}]},{\"eintrag\":[{\"stichwort\":\"Köln\"},{\"eintragstext\":\"Köln ist eine Stadt, die ...\"}]}]}]"

-}
decode : String -> Result String Generic.Value
decode str =
    case Json.decode str of
        Ok gen ->
            Ok gen

        Err _ ->
            case Xml.decode str of
                Ok gen ->
                    Ok gen

                Err _ ->
                    case Yaml.decode str of
                        Ok gen ->
                            Ok gen

                        Err _ ->
                            Err "Unable to decode as JSON, XML, and YAML"
