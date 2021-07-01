# elm-generic

This generic type-system can be used as an intermediate representation (lingua
franca), for JSON, YAML, and XML at the moment. Other formats like TOML, MessagePack,
ProtoBuf, Eon, XML might come later and can be used similary. The basic idea is
to have one general abstraction, that can be used to decode multiple formats and
translate between them. But in most cases you will only want to change some
values or extract them, without developing custom decoders and encoders. Thus,
data might be incorrect or some JSON formats might change, but you simply do not
care and extract the pieces, you are interested in.

``` elm
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
        |> Yaml.encode
        |> Yaml.toString 2
        |> Html.text
        |> List.singleton
        |> Html.pre []
```
