module Main exposing (main)

import Generic as Gen
import Generic.Json as Json
import Generic.Yaml as Yaml
import Html exposing (Html)


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
        |> Json.decodeString
        --  |> Gen.get [ Gen.String "age", Gen.Int 1 ]
        -- (Gen.List [ Gen.Int 333, Gen.String "asadfasda", Gen.Bool True ])
        |> Gen.toString
        |> Maybe.withDefault "ssss"
        |> Html.text
