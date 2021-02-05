module OnTheFlyTest exposing (try)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Set exposing (Set)


type alias Point =
    ( Int, Int )


type alias P2 =
    ( String, ( Int, Int ) )


type TT a
    = Test a


type Beta
    = Meta (TT String)


type NewTest
    = Case1 ( String, Int )
    | Case2 String { a : String, b : List Int, c : ( NewTest, Int ), d : String }


type alias Goal =
    { des : String, test : String, id : Int, newTest : NewTest, nt : NewTest }


try : String
try =
    ""


nt : NewTest -> String
nt n =
    case n of
        Case1 arg ->
            "Case1"

        Case2 s s2 ->
            String.join " - " [ "Case2 ", s, s2.a ]


testString =
    """ { "des" : "testDes", "test" : "testdata", "id" : 32 ,"nt" : "Case1", "newTest" :
     {"Case2" : {"arg0": "test", "arg1" : { "a" : "Avalue", "b" : [300]  }} }} """



-- ENCODERS


type alias SSet a =
    Set a


sSetEncoder : (a -> Encode.Value) -> SSet a -> Encode.Value
sSetEncoder aEncoder sSet =
    Encode.list aEncoder <| Set.toList sSet
