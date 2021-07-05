module Generic exposing
    ( Value(..), typeOf
    , get, set
    , toBool, toInt, toFloat, toString, toList, toDict, toSet, toDate, toDateTime
    , dictFromList
    )

{-| This module defines a basic type system, as it is known from JavaScript. The
basic idea is that you can use generic types of decoders that can translate into
this format, so you are not forces anymore to write custom decoders if your are
only interested in some basic values.

The type system should also behave similarly the one we know from JavaScript.


## Types

@docs Value, typeOf


## Getting & Setting

@docs get, set


## Transformations

@docs toBool, toInt, toFloat, toString, toList, toDict, toSet, toDate, toDateTime


## Helpers

@docs dictFromList

-}

import Dict exposing (Dict)
import EverySet exposing (EverySet)
import GenericDict
import Iso8601
import Time exposing (Posix)


{-| Supported basic types. In contrast to JSON types, this covers also types
such as `Set`, `Date`, and `DateTime`.

In the future it would also be interesting to have a general binary type that
can be used with any kind of transformation function, i.e. `toFloat` or for
serializing streams of data (lists, sets, etc.).

`Set`s and `Dict`s use different types than the Base elm-types, so that they
can cope with more complex values that `Int`, `String`, etc. Thus, a
`Generic.Dict` can also have keys of type integer or list, nested ones are
also allowed.

-}
type Value
    = Null
    | Bool Bool
    | Int Int
    | Float Float
    | String String
    | List (List Value)
    | Set (EverySet Value)
    | Dict (GenericDict.Dict Value Value)
    | Date Posix
    | DateTime Posix


{-| Clone of the JavaScript function `typeof` that returns a String:

    typeOf Null --> "null"

    typeOf (Bool True) --> "bool"

    typeOf (Int -99) --> "int"

    typeOf (Float 1.234) --> "float"

    typeOf (String "foo") --> "string"

-}
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


{-| Convert a generic type to Float, if possible

    import Generic as Gen

    Gen.toFloat Null --> Nothing

    Gen.toFloat (Bool True) --> Just 1.0

    Gen.toFloat (Bool False) --> Just 0.0

    Gen.toFloat (Int 2) --> Just 2.0

    Gen.toFloat (Int -1) --> Just -1.0

    Gen.toFloat (Float 0.0) --> Just 0.0

    Gen.toFloat (Float 3.141592) --> Just 3.141592

    Gen.toFloat (String "") --> Nothing

    Gen.toFloat (String "33") --> Just 33

    Gen.toFloat (String "33 m") --> Nothing

    Gen.toFloat (String "33.33") --> Just 33.33

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


{-| Convert a generic type to a string representation. Actually this works for any value, except for `Null`:

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
                |> GenericDict.toList
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


{-| Tries to turn every type into a list

    import Generic as Gen

    List [ Null, Int 12 ] |> toList --> Just [Null, Int 12]

    List [ Null, Int 12, Int 12]
        |> toSet
        |> Set
        |> toList --> Just [Int 12, Null]

-}
toList : Value -> Maybe (List Value)
toList generic =
    case generic of
        List duck ->
            Just duck

        Set duck ->
            duck |> EverySet.toList |> Just

        _ ->
            Nothing


{-| Returns an elm Dict from a Generic.Dict, if possible
-}
toDict : Value -> Maybe (Dict String Value)
toDict generic =
    case generic of
        Dict duck ->
            duck
                |> GenericDict.toList
                |> List.map (Tuple.mapFirst (toString >> Maybe.withDefault "null"))
                |> Dict.fromList
                |> Just

        _ ->
            Nothing


{-| Turns a list of tuples of generic values into a generic dictionary.

    [ ( Int 12, String "foo" ), ( Null, List [ Float 12.0, Null ] ) ]
        |> dictFromList

-}
dictFromList : List ( Value, Value ) -> Value
dictFromList =
    GenericDict.fromList toSubString >> Dict


{-| Turns everything into a set of type `EverySet`, if possible. Passing a `Dict` will result in a set of dict values.
-}
toSet : Value -> EverySet Value
toSet generic =
    case generic of
        List duck ->
            duck |> EverySet.fromList

        Set duck ->
            duck

        Dict duck ->
            duck
                |> GenericDict.values
                |> EverySet.fromList

        _ ->
            EverySet.singleton generic


{-| Try to get a `Date` value in Posix Format.
-}
toDate : Value -> Maybe Posix
toDate generic =
    generic
        |> toDateTime
        |> Maybe.andThen (DateTime >> toString)
        |> Maybe.andThen (String.split "T" >> List.head)
        |> Maybe.andThen (\x -> x ++ "T00:00:00.000Z" |> Iso8601.toTime >> Result.toMaybe)


{-| Try to get a `DateTime` value in Posix Format.
-}
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


{-| Generic getter, the first parameter defines a sequence of how the generic type
should be traversed. The result is a `Maybe Value` that needs to be converted into
your desired elm-type:

    import Generic.Decoder exposing (decode)

    "{\"type\": [1, [2, \"tada\"] ]}"
        |> decode
        |> Result.withDefault Null
        |> get [String "type", Int 1, Int 1]
        |> Maybe.andThen toString
        |> (==) (Just "tada") --> True

    "{\"type\": [1, [2, \"tada\"] ]}"
        |> decode
        |> Result.withDefault Null
        |> get [Int 99]
        |> Maybe.andThen toString
        |> (==) Nothing --> True

-}
get : List Value -> Value -> Maybe Value
get ids generic =
    case ( ids, generic ) of
        ( [ id ], List duck ) ->
            id
                |> toInt
                >> Maybe.andThen (getList duck)

        ( [ id ], Dict duck ) ->
            GenericDict.get toSubString id duck

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

        ( [ _ ], _ ) ->
            Nothing

        ( id :: s, _ ) ->
            generic
                |> get [ id ]
                |> Maybe.andThen (get s)

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


{-| Generic setter, the first parameter defines a sequence of how the generic
type should be traversed and the second the new value

    import Generic.Json as Json
    import Generic as Gen

    "{\"type\": [1, [2, \"tada\"] ]}"
        |> Json.decodeString
        |> Gen.set [Gen.String "type", Gen.Int 1, Gen.Int 1] (Gen.Int 9999)
        |> Json.encode
        |> Json.toString 0
        |> (==) "{\"type\":[1,[2,9999]]}"
        --> True

    "{\"type\": [1, [2, \"tada\"] ]}"
        |> Json.decodeString
        |> Gen.set [Gen.String "type", Gen.Int 11, Gen.Int 11] (Gen.Int 9999)
        |> Json.encode
        |> Json.toString 0
        |> (==) "{\"type\":[1,[2,\"tada\"]]}"
        --> True

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
                |> GenericDict.insert toSubString id value
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
            GenericDict.get toSubString head duck
                |> Maybe.map
                    (\d ->
                        duck
                            |> GenericDict.insert toSubString head (set tail value d)
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
                |> GenericDict.map (\_ value -> fn value)
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
                |> GenericDict.toList
                >> List.filterMap
                    (\( key, value ) ->
                        case fn value of
                            Just val ->
                                Just ( key, val )

                            _ ->
                                Nothing
                    )
                >> GenericDict.fromList toSubString
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
                |> GenericDict.filter (\_ value -> value |> fn >> toBool)
                >> Dict

        Set duck ->
            duck
                |> EverySet.filter (fn >> toBool)
                >> Set

        _ ->
            generic
