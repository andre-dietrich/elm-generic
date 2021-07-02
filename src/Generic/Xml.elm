module Generic.Xml exposing (decode, encode, toString)

{-|

@docs decode, encode, toString

-}

import Generic
import Generic.Json as Json
import Xml
import Xml.Decode as XD
import Xml.Encode as XE


{-| Will return a String representation for any kind of XML value.
-}
toString : Int -> Xml.Value -> String
toString =
    XE.encode


{-| A generic decoder that will translate any XML string into the Generic representation.
-}
decode : String -> Result String Generic.Value
decode =
    XD.decodeChildren >> Result.map (Xml.xmlToJson2 >> Json.decodeValue)


{-| A generic encoder that will translate any generic value into an XML value.
-}
encode : Generic.Value -> Xml.Value
encode =
    Json.encode >> Xml.jsonToXml
