module OnTheFlyTest exposing (try)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode


type alias Point =
    ( Int, Int )


type alias P2 =
    ( String, (Int,Int) )



let

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
    case Decode.decodeString goalDecoder testString of
        Ok goal ->
            "Goal: " ++ goal.des ++ " " ++ goal.test ++ "\n" ++ nt goal.newTest

        Err e ->
            Decode.errorToString e


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


type NewTest2
    = C1
    | C2 String


newTest2Encoder : NewTest2 -> Encode.Value
newTest2Encoder newTest2 =
    case newTest of
        C1 ->
            Encode.string "Case1"

        C2 arg1 ->
            Encode.object
                [ ( "arg1", Encode.string arg1 )
                ]


goalEncoder : Goal -> Encode.Value
goalEncoder goal =
    Encode.object
        [ ( "des", Encode.string goal.des )
        , ( "test", Encode.string goal.test )
        , ( "id", Encode.int goal.id )
        ]



-- TESTCODE COPIES FROM OUTPUT


tTDecoder : Decode.Decoder a -> Decode.Decoder (TT a)
tTDecoder decA =
    Decode.succeed (\a -> Test a)
        |> required "arg0" decA


betaDecoder : Decode.Decoder Beta
betaDecoder =
    Decode.oneOf
        [ Decode.succeed (\a -> a)
            |> required "Meta"
                (Decode.succeed Meta
                    |> required "arg0" (Decode.lazy (\_ -> tTDecoder Decode.string))
                )
        ]


newTestDecoder : Decode.Decoder NewTest
newTestDecoder =
    Decode.oneOf
        [ Decode.succeed (\a -> a)
            |> required "Case1"
                (Decode.succeed Case1
                    |> required "arg0"
                        (Decode.succeed
                            (\item0 item1 -> ( item0, item1 ))
                            |> required "item0" Decode.string
                            |> required "item1" Decode.int
                        )
                )
        , Decode.succeed (\a -> a)
            |> required "Case2"
                (Decode.succeed Case2
                    |> required "arg0" Decode.string
                    |> required "arg1"
                        (Decode.succeed
                            (\a b c d -> { a = a, b = b, c = c, d = d })
                            |> required "a" Decode.string
                            |> required "b" (Decode.list Decode.int)
                            |> required "c"
                                (Decode.succeed
                                    (\item0 item1 -> ( item0, item1 ))
                                    |> required "item0" (Decode.lazy (\_ -> newTestDecoder))
                                    |> required "item1" Decode.int
                                )
                            |> required "d" Decode.string
                        )
                )
        ]


goalDecoder : Decode.Decoder Goal
goalDecoder =
    Decode.succeed Goal
        |> required "des" Decode.string
        |> required "test" Decode.string
        |> required "id" Decode.int
        |> required "newTest" (Decode.lazy (\_ -> newTestDecoder))
        |> required "nt" (Decode.lazy (\_ -> newTestDecoder))
