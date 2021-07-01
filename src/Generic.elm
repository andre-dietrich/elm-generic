module Generic exposing
    ( Value(..)
    , filter
    , filterMap
    , get
    , map
    , set
    , toBool
    , toDate
    , toDateTime
    , toDict
    , toFloat
    , toInt
    , toList
    , toSet
    , toString
    , toSubString
    , typeOf
    )

import EverySet exposing (EverySet)
import GenericDict as Dict exposing (Dict)
import Iso8601
import Time exposing (Posix)


type Value
    = Null
    | Bool Bool
    | Int Int
    | Float Float
    | String String
    | List (List Value)
    | Set (EverySet Value)
    | Dict (Dict Value Value)
    | Date Posix
    | DateTime Posix


typeOf : Value -> String
typeOf generic =
    case generic of
        Null ->
            "null"

        Bool _ ->
            "bool"

        Int _ ->
            "int"

        Float _ ->
            "float"

        String _ ->
            "string"

        List _ ->
            "list"

        Set _ ->
            "set"

        Dict _ ->
            "dict"

        Date _ ->
            "date"

        DateTime _ ->
            "datetime"


{-| Same as in JavaScript, tries to make a bool from every type

    toBool Null --> False

    toBool (Bool True) --> True

    toBool (Bool False) --> False

    toBool (Int 0) --> False

    toBool (Int -1) --> True

    toBool (Float 0.0) --> False

    toBool (Float 3.141592) --> True

    toBool (String "") --> False

    toBool (String "misc") --> True

    toBool (List []) --> True

-}
toBool : Value -> Bool
toBool generic =
    case generic of
        Null ->
            False

        Bool duck ->
            duck

        Int duck ->
            duck /= 0

        Float duck ->
            duck /= 0

        String duck ->
            duck /= ""

        _ ->
            True


{-| Convert a generic toy to Int, if possible

    toInt Null --> Nothing

    toInt (Bool True) --> Just 1

    toInt (Bool False) --> Just 0

    toInt (Int 0) --> Just 0

    toInt (Int -1) --> Just -1

    toInt (Float 0.0) --> Just 0

    toInt (Float 3.141592) --> Just 3

    toInt (String "") --> Nothing

    toInt (String "33") --> Just 33

    toInt (String "33.33") --> Just 33

-}
toInt : Value -> Maybe Int
toInt generic =
    case generic of
        Bool duck ->
            if duck then
                Just 1

            else
                Just 0

        Int duck ->
            Just duck

        Float duck ->
            Just (round duck)

        String duck ->
            duck
                |> String.toFloat
                |> Maybe.map round

        Date duck ->
            duck
                |> Time.posixToMillis
                |> Just

        DateTime duck ->
            duck
                |> Time.posixToMillis
                |> Just

        _ ->
            Nothing


{-| Convert a generic toy to Float, if possible

    toFloat Null --> Nothing

    toFloat (Bool True) --> Just 1.0

    toFloat (Bool False) --> Just 0.0

    toFloat (Int 2) --> Just 2.0

    toFloat (Int -1) --> Just -1.0

    toFloat (Float 0.0) --> Just 0.0

    toFloat (Float 3.141592) --> Just 3.141592

    toFloat (String "") --> Nothing

    toFloat (String "33") --> Just 33

    toFloat (String "33 m") --> Nothing

    toFloat (String "33.33") --> Just 33.33

-}
toFloat : Value -> Maybe Float
toFloat generic =
    case generic of
        Bool duck ->
            Just <|
                if duck then
                    1

                else
                    0

        Int duck ->
            duck
                |> Basics.toFloat
                >> Just

        Float duck ->
            Just duck

        String duck ->
            String.toFloat duck

        Date duck ->
            duck
                |> Time.posixToMillis
                |> Basics.toFloat
                |> Just

        DateTime duck ->
            duck
                |> Time.posixToMillis
                |> Basics.toFloat
                |> Just

        _ ->
            Nothing


{-| Convert a generic toy to Float, if possible

    toString Null --> Nothing

    toString (Bool True) --> Just "true"

    toString (Bool False) --> Just "false"

    toString (Int 2) --> Just "2"

    toString (Int -1) --> Just "-1"

    toString (Float 0.0) --> Just "0"

    toString (Float 3.141592) --> Just "3.141592"

    toString (String "") --> Just ""

    toString (String "33") --> Just "33"

    toString (String "33 m") --> Just "33 m"

    toString (String "33.33") --> Just "33.33"

    toString (List [ String "33.33", Null ]) --> Just "[\"33.33\",null]"

-}
toString : Value -> Maybe String
toString generic =
    case generic of
        Null ->
            Nothing

        Bool b ->
            Just <|
                if b then
                    "true"

                else
                    "false"

        Int duck ->
            Just <| String.fromInt duck

        Float duck ->
            Just <| String.fromFloat duck

        String duck ->
            Just duck

        List duck ->
            duck
                |> List.map toSubString
                >> seqString "[" "]"
                >> Just

        Set duck ->
            duck
                |> EverySet.toList
                >> List.map toSubString
                >> seqString "set(" ")"
                >> Just

        Dict duck ->
            duck
                |> Dict.toList
                >> List.map (\( key, value ) -> toSubString key ++ ":" ++ toSubString value)
                >> seqString "{" "}"
                >> Just

        Date duck ->
            duck
                |> Iso8601.fromTime
                |> String.split "T"
                |> List.head

        DateTime duck ->
            duck
                |> Iso8601.fromTime
                >> Just


seqString : String -> String -> List String -> String
seqString begin end body =
    begin
        ++ (body
                |> List.intersperse ","
                |> String.concat
           )
        ++ end


toSubString : Value -> String
toSubString generic =
    case generic of
        String duck ->
            "\"" ++ duck ++ "\""

        _ ->
            generic
                |> toString
                |> Maybe.withDefault "null"



{- Tries to turn every type into a list -}


toList : Value -> List Value
toList generic =
    case generic of
        List duck ->
            duck

        Set duck ->
            duck |> EverySet.toList

        Dict duck ->
            Dict.values duck

        _ ->
            [ generic ]


toDict : List ( Value, Value ) -> Dict Value Value
toDict =
    Dict.fromList toSubString


toSet : Value -> EverySet Value
toSet generic =
    case generic of
        List duck ->
            duck |> EverySet.fromList

        Set duck ->
            duck

        Dict duck ->
            duck
                |> Dict.values
                |> EverySet.fromList

        _ ->
            EverySet.singleton generic


toDate : Value -> Maybe Posix
toDate generic =
    generic
        |> toDateTime
        |> Maybe.andThen (DateTime >> toString)
        |> Maybe.andThen (String.split "T" >> List.head)
        |> Maybe.andThen (\x -> x ++ "T00:00:00.000Z" |> Iso8601.toTime >> Result.toMaybe)


toDateTime : Value -> Maybe Posix
toDateTime generic =
    case generic of
        Date duck ->
            Just duck

        DateTime duck ->
            Just duck

        String str ->
            generic
                |> toInt
                |> Maybe.map Time.millisToPosix
                |> (\time ->
                        case time of
                            Nothing ->
                                str
                                    |> Iso8601.toTime
                                    |> Result.toMaybe

                            _ ->
                                time
                   )

        _ ->
            generic
                |> toInt
                |> Maybe.map Time.millisToPosix



{- Generic getter, the first parameter defines a sequense of how the generic
   type should be traversed

      "{'type':[1,[2,'tada']]}"
        |> Generic.Json.decodeString
        |> get [String "type", Int 1, Int 1]
        |> toString -- "tada"
-}


get : List Value -> Value -> Maybe Value
get ids generic =
    case ( ids, generic ) of
        ( [ id ], List duck ) ->
            id
                |> toInt
                >> Maybe.andThen (getList duck)

        ( [ id ], Dict duck ) ->
            Dict.get toSubString id duck

        ( [ id ], Set duck ) ->
            id
                |> toInt
                >> Maybe.andThen (getList (EverySet.toList duck))

        ( [ _ ], String duck ) ->
            duck
                |> String.split ""
                >> List.map String
                >> List
                >> get ids

        ( id :: s, _ ) ->
            generic
                |> get [ id ]
                >> Maybe.andThen (get s)

        _ ->
            Nothing


getList : List Value -> Int -> Maybe Value
getList duck id =
    case duck of
        [] ->
            Nothing

        l :: ist ->
            if id == 0 then
                Just l

            else if id < 0 then
                Nothing

            else
                getList ist (id - 1)



{- Generic setter, the first parameter defines a sequense of how the generic
   type should be traversed and the second the new value

      "{'type':[1,[2,'tada']]}"
        |> Generic.Json.decodeString
        |> set [String "type", Int 1, Int 1] (Int 9999)
        |> toString -- "{\"type\":[1,[2,9999]]}"
-}


set : List Value -> Value -> Value -> Value
set ids value generic =
    case ( ids, generic ) of
        ( [], _ ) ->
            value

        ( head :: tail, List duck ) ->
            head
                |> toInt
                >> Maybe.map (setList duck value tail >> List)
                >> Maybe.withDefault generic

        ( head :: tail, Set duck ) ->
            head
                |> toInt
                >> Maybe.map
                    (setList (EverySet.toList duck)
                        value
                        tail
                        >> EverySet.fromList
                        >> Set
                    )
                >> Maybe.withDefault generic

        ( [ id ], Dict duck ) ->
            duck
                |> Dict.insert toSubString id value
                |> Dict

        ( [ id ], String duck ) ->
            case ( value, toInt id ) of
                ( String char, Just i ) ->
                    if String.length char == 1 then
                        duck
                            |> String.toList
                            |> setString i char
                            |> String.fromList
                            |> String

                    else
                        generic

                _ ->
                    generic

        ( head :: tail, Dict duck ) ->
            Dict.get toSubString head duck
                |> Maybe.map
                    (\d ->
                        duck
                            |> Dict.insert toSubString head (set tail value d)
                            |> Dict
                    )
                |> Maybe.withDefault generic

        _ ->
            generic


setList : List Value -> Value -> List Value -> Int -> List Value
setList duck value ids id =
    case duck of
        [] ->
            []

        head :: tail ->
            if id == 0 then
                set ids value head :: tail

            else if id < 0 then
                duck

            else
                head :: setList tail value ids (id - 1)


setString : Int -> String -> List Char -> List Char
setString id value duck =
    case duck of
        [] ->
            []

        head :: tail ->
            if id == 0 then
                value
                    |> String.uncons
                    |> Maybe.map (\( c, _ ) -> c :: tail)
                    |> Maybe.withDefault duck

            else if id < 0 then
                duck

            else
                head :: setString (id - 1) value tail


map : (Value -> Value) -> Value -> Value
map fn generic =
    case generic of
        List duck ->
            duck
                |> List.map fn
                >> List

        Dict duck ->
            duck
                |> Dict.map (\_ value -> fn value)
                >> Dict

        Set duck ->
            duck
                |> EverySet.map fn
                >> Set

        _ ->
            fn generic


filterMap : (Value -> Maybe Value) -> Value -> Value
filterMap fn generic =
    case generic of
        List duck ->
            duck
                |> List.filterMap fn
                >> List

        Dict duck ->
            duck
                |> Dict.toList
                >> List.filterMap
                    (\( key, value ) ->
                        case fn value of
                            Just val ->
                                Just ( key, val )

                            _ ->
                                Nothing
                    )
                >> Dict.fromList toSubString
                >> Dict

        Set duck ->
            duck
                |> EverySet.toList
                >> List.filterMap fn
                >> EverySet.fromList
                >> Set

        _ ->
            generic


filter : (Value -> Value) -> Value -> Value
filter fn generic =
    case generic of
        List duck ->
            duck
                |> List.filter (fn >> toBool)
                >> List

        Dict duck ->
            duck
                |> Dict.filter (\_ value -> value |> fn >> toBool)
                >> Dict

        Set duck ->
            duck
                |> EverySet.filter (fn >> toBool)
                >> Set

        _ ->
            generic
