module Generic exposing
    ( Type(..)
    , filter
    , filterMap
    , get
    , map
    , set
    , toBinary
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

import Base64
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode
import Bytes.Encode
import EverySet exposing (EverySet)
import GenericDict as Dict exposing (Dict)
import Iso8601
import Time exposing (Posix)


type Type
    = Null
    | Bool Bool
    | Int Int
    | Float Float
    | String String
    | List (List Type)
    | Set (EverySet Type)
    | Dict (Dict Type Type)
    | Date Posix
    | DateTime Posix
    | Binary Bytes


typeOf : Type -> String
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

        Binary _ ->
            "binary"


toBool : Type -> Bool
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

        Binary duck ->
            duck
                |> Bytes.Decode.decode Bytes.Decode.signedInt8
                |> Maybe.map (Int >> toBool)
                |> Maybe.withDefault False

        _ ->
            True


toInt : Type -> Maybe Int
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

        Binary duck ->
            duck
                |> Bytes.Decode.decode (Bytes.Decode.signedInt32 BE)

        _ ->
            Nothing


toFloat : Type -> Maybe Float
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

        Binary duck ->
            duck
                |> Bytes.Decode.decode (Bytes.Decode.float64 BE)

        _ ->
            Nothing


toString : Type -> Maybe String
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

        Binary duck ->
            Bytes.Decode.decode (Bytes.Decode.string (Bytes.width duck)) duck


seqString : String -> String -> List String -> String
seqString begin end body =
    begin
        ++ (body
                |> List.intersperse ","
                |> String.concat
           )
        ++ end


toSubString : Type -> String
toSubString generic =
    case generic of
        String duck ->
            "\"" ++ duck ++ "\""

        Binary duck ->
            duck
                |> Base64.fromBytes
                |> Maybe.withDefault "bytes"

        _ ->
            generic
                |> toString
                |> Maybe.withDefault "null"


toList : Type -> List Type
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


toDict : List ( Type, Type ) -> Dict Type Type
toDict =
    Dict.fromList toSubString


toSet : Type -> EverySet Type
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


toDate : Type -> Maybe Posix
toDate generic =
    generic
        |> toDateTime
        |> Maybe.andThen (DateTime >> toString)
        |> Maybe.andThen (String.split "T" >> List.head)
        |> Maybe.andThen (\x -> x ++ "T00:00:00.000Z" |> Iso8601.toTime >> Result.toMaybe)


toDateTime : Type -> Maybe Posix
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

        Binary duck ->
            duck
                |> Bytes.Decode.decode (Bytes.Decode.signedInt32 BE)
                >> Maybe.map Time.millisToPosix

        _ ->
            generic
                |> toInt
                |> Maybe.map Time.millisToPosix


toBinary : Type -> Bytes
toBinary generic =
    case generic of
        Binary duck ->
            duck

        Bool duck ->
            (if duck then
                1

             else
                0
            )
                |> Bytes.Encode.signedInt8
                >> Bytes.Encode.encode

        Int duck ->
            duck
                |> Bytes.Encode.signedInt32 BE
                >> Bytes.Encode.encode

        Float duck ->
            duck
                |> Bytes.Encode.float64 BE
                >> Bytes.Encode.encode

        String duck ->
            duck
                |> Bytes.Encode.string
                >> Bytes.Encode.encode

        List duck ->
            duck
                |> List.map (toBinary >> Bytes.Encode.bytes)
                >> Bytes.Encode.sequence
                >> Bytes.Encode.encode

        Set duck ->
            duck
                |> EverySet.toList
                >> List.map (toBinary >> Bytes.Encode.bytes)
                >> Bytes.Encode.sequence
                >> Bytes.Encode.encode

        Date duck ->
            duck
                |> Time.posixToMillis
                >> Bytes.Encode.signedInt32 BE
                >> Bytes.Encode.encode

        DateTime duck ->
            duck
                |> Time.posixToMillis
                >> Bytes.Encode.signedInt32 BE
                >> Bytes.Encode.encode

        Dict duck ->
            0
                |> Bytes.Encode.signedInt8
                >> Bytes.Encode.encode

        Null ->
            0
                |> Bytes.Encode.signedInt8
                >> Bytes.Encode.encode



-->> Bytes.Encode.encode


get : List Type -> Type -> Maybe Type
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

        ( [ id ], String duck ) ->
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


getList : List Type -> Int -> Maybe Type
getList list id =
    case list of
        [] ->
            Nothing

        l :: ist ->
            if id == 0 then
                Just l

            else if id < 0 then
                Nothing

            else
                getList ist (id - 1)


set : List Type -> Type -> Type -> Type
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


setList : List Type -> Type -> List Type -> Int -> List Type
setList list value ids id =
    case list of
        [] ->
            []

        head :: tail ->
            if id == 0 then
                set ids value head :: tail

            else if id < 0 then
                list

            else
                head :: setList tail value ids (id - 1)


setString : Int -> String -> List Char -> List Char
setString id value list =
    case list of
        [] ->
            []

        head :: tail ->
            if id == 0 then
                value
                    |> String.uncons
                    |> Maybe.map (\( c, _ ) -> c :: tail)
                    |> Maybe.withDefault list

            else if id < 0 then
                list

            else
                head :: setString (id - 1) value tail


map : (Type -> Type) -> Type -> Type
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


filterMap : (Type -> Maybe Type) -> Type -> Type
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

                            Nothing ->
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


filter : (Type -> Type) -> Type -> Type
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
