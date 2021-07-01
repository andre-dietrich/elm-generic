module Generic.Decoder exposing (decode)

import Generic
import Generic.Json as Json
import Generic.Xml as Xml
import Generic.Yaml as Yaml
import Json.Decode as JD


decode : String -> Result String Generic.Value
decode str =
    case JD.decodeString Json.decoder str of
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
