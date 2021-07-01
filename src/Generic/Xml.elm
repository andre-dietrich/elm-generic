module Generic.Xml exposing
    ( decode
    , encode
    , toString
    )

import Generic
import Generic.Json as Json
import Xml
import Xml.Decode as XD
import Xml.Encode as XE


toString : Int -> Xml.Value -> String
toString =
    XE.encode


decode : String -> Result String Generic.Value
decode =
    XD.decodeChildren >> Result.map (Xml.xmlToJson2 >> Json.decodeValue)


encode : Generic.Value -> Xml.Value
encode =
    Json.encode >> Xml.jsonToXml
