module JsonParser exposing (Json, Member, Name, Number(..), Value(..), parse)

import DeadEndsToString exposing (deadEndsToString)
import Parser exposing ((|.), (|=), DeadEnd, Parser, Trailing(..), chompWhile, end, float, getChompedString, keyword, lazy, number, oneOf, run, sequence, spaces, succeed, symbol)


type alias Json =
    Value


type alias Name =
    String


type Number
    = NInt Int
    | NFloat Float


type Value
    = VObject (List Member)
    | VArray (List Value)
    | VString String
    | VNumber Number
    | VTrue
    | VFalse
    | VNull


type alias Member =
    { name : Name, value : Value }


parse : String -> Result String Json
parse input =
    Result.mapError (\e -> deadEndsToString e) (run json input)


json : Parser Json
json =
    succeed identity
        |= value
        |. spaces
        |. end


value : Parser Value
value =
    oneOf
        [ succeed VTrue
            |. keyword "true"
        , succeed VFalse
            |. keyword "false"
        , succeed VNull
            |. keyword "null"
        , succeed VFalse
            |. keyword "false"
        , object
        , array
        , succeed VString
            |= string
        , number
            { int = Just <| \i -> VNumber <| NInt i
            , hex = Nothing
            , octal = Nothing
            , binary = Nothing
            , float = Just <| \f -> VNumber <| NFloat f
            }
        ]


array : Parser Value
array =
    succeed VArray
        |= sequence
            { start = "["
            , separator = ","
            , end = "]"
            , spaces = spaces
            , item = lazy (\_ -> value)
            , trailing = Forbidden
            }


object : Parser Value
object =
    succeed VObject
        |= sequence
            { start = "{"
            , separator = ","
            , end = "}"
            , spaces = spaces
            , item = member
            , trailing = Forbidden
            }


member : Parser Member
member =
    succeed Member
        |. spaces
        |= string
        |. spaces
        |. symbol ":"
        |. spaces
        |= lazy (\_ -> value)


string : Parser String
string =
    getChompedString <|
        succeed ()
            |. symbol "\""
            |. chompWhile (\c -> Char.isAlphaNum c)
            |. symbol "\""
