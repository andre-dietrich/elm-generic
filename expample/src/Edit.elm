module Edit exposing (..)

-- A text input for reversing text. Very useful!
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/text_fields.html
--

import Browser
import Generic as Gen
import Generic.Json as Json
import Generic.Xml as Xml
import Generic.Yaml as Yaml
import Html exposing (Attribute, Html)
import Html.Attributes as Attr
import Html.Events as Event



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { generic : Maybe Gen.Value
    , json : String
    , yaml : String
    , xml : String
    }


init : Model
init =
    { generic = Nothing
    , json = ""
    , yaml = ""
    , xml = ""
    }



-- UPDATE


type Msg
    = JSON String
    | YAML String
    | XML String


update : Msg -> Model -> Model
update msg model =
    case msg of
        JSON newContent ->
            case Json.decode newContent of
                Ok generic ->
                    { model
                        | generic = Just generic
                        , json = generic |> Json.encode |> Json.toString 2
                    }

                Err _ ->
                    { model
                        | generic = Nothing
                        , json = newContent
                    }

        XML newContent ->
            case Xml.decode newContent of
                Ok generic ->
                    { model
                        | generic = Just generic
                        , xml = generic |> Xml.encode |> Xml.toString 2
                    }

                Err _ ->
                    { model
                        | generic = Nothing
                        , xml = newContent
                    }

        YAML newContent ->
            case Yaml.decode newContent of
                Ok generic ->
                    { model
                        | generic = Just generic
                        , yaml = generic |> Yaml.encode |> Yaml.toString 2
                    }

                Err _ ->
                    { model
                        | generic = Nothing
                        , yaml = newContent
                    }



-- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.h1 [] [ Html.text "elm-generic" ]
        , Html.div
            [ Attr.style "float" "left"
            , Attr.style "width" "49%"
            , Attr.style "height" "90vh"
            ]
            [ editor "YAML"
                Nothing
                YAML
                (model.generic
                    |> Maybe.map (Yaml.encode >> Yaml.toString 2)
                    |> Maybe.withDefault model.yaml
                )
            , editor "XML"
                Nothing
                XML
                (model.generic
                    |> Maybe.map (Xml.encode >> Xml.toString 2)
                    |> Maybe.withDefault model.xml
                )
            ]
        , Html.div
            [ Attr.style "float" "right"
            , Attr.style "width" "49%"
            , Attr.style "height" "90vh"
            ]
            [ editor "JSON"
                (Just "101%")
                JSON
                (model.generic
                    |> Maybe.map (Json.encode >> Json.toString 2)
                    |> Maybe.withDefault model.json
                )
            ]
        ]


editor title height msg str =
    Html.div
        [ height
            |> Maybe.withDefault "43%"
            |> Attr.style "height"
        ]
        [ Html.h2 [] [ Html.text title ]
        , Html.textarea
            [ Attr.style "width" "100%"
            , Attr.style "height" "80%"
            , Attr.value str
            , Event.onInput msg
            ]
            []
        ]
