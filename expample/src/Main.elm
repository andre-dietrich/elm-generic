module Main exposing (main)

import Generic as Gen
import Generic.Json as Json
import Generic.Xml as Xml
import Generic.Yaml as Yaml
import Html exposing (Html)
import Generic.Decoder exposing (decode)

main : Html msg
main =
    """
    {
"problems": [{
    "Diabetes":[{
        "medications":[{
            "medicationsClasses":[{
                "className":[{
                    "associatedDrug":[{
                        "name":"asprin",
                        "dose":"",
                        "strength":"500 mg"
                    }],
                    "associatedDrug#2":[{
                        "name":"somethingElse",
                        "dose":"",
                        "strength":"500 mg"
                    }]
                }],
                "className2":[{
                    "associatedDrug":[{
                        "name":"asprin",
                        "dose":"",
                        "strength":"500 mg"
                    }],
                    "associatedDrug#2":[{
                        "name":"somethingElse",
                        "dose":"",
                        "strength":"500 mg"
                    }]
                }]
            }]
        }],
        "labs":[{
            "missing_field": "missing_value"
        }]
    }],
    "Asthma":[{}]
}]}

    """
        |> decode
        |> Result.withDefault Gen.Null
        |> Yaml.encode
        |> Yaml.toString 2
        |> Html.text
        |> List.singleton
        |> Html.pre []
