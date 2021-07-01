module Test exposing (main)

import Generic as Gen
import Generic.Json as Json
import Generic.Xml as Xml
import Generic.Yaml as Yaml
import Html exposing (Html)


main : Html msg
main =
    """
    <glossary><title>example glossary</title>
  <GlossDiv><title a="sss">12</title>
   <GlossList>
    <GlossEntry ID="SGML" SortAs="SGML">
     <GlossTerm>Standard Generalized Markup Language</GlossTerm>
     <Acronym>SGML</Acronym>
     <Abbrev>ISO 8879:1986</Abbrev>
     <GlossDef>
      <para>A meta-markup language, used to create markup
languages such as DocBook.</para>
      <GlossSeeAlso OtherTerm="GML" />
      <GlossSeeAlso OtherTerm="XML" />
     </GlossDef>
     <GlossSee OtherTerm="markup" />
    </GlossEntry>
   </GlossList>
  </GlossDiv>
 </glossary>
    """
        |> Xml.decode
        |> Result.withDefault Gen.Null
        |> Xml.encode
        |> Xml.toString 1
        |> Html.text
        |> List.singleton
        |> Html.pre []
