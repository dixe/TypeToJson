module OnTheFlyTest exposing (try)

import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Decode.Extra
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Json.Encode.Extra
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


type alias Example =
    { result : Result Int String
    , data : Maybe String
    }


exampleEncoder : Example -> Encode.Value
exampleEncoder example =
    Encode.object
        [ ( "result"
          , (\data ->
                Encode.object
                    (case data of
                        Ok ok ->
                            [ ( "Ok", Encode.string ok ) ]

                        Err err ->
                            [ ( "Err", Encode.int err ) ]
                    )
            )
                example.result
          )
        , ( "data", Json.Encode.Extra.maybe Encode.string example.data )
        ]



-- DECODERS


exampleDecoder : Decode.Decoder Example
exampleDecoder =
    Decode.succeed Example
        |> required "result"
            (Decode.oneOf
                [ Decode.succeed Ok
                    |> required "Ok" Decode.string
                , Decode.succeed Err
                    |> required "Err" Decode.int
                ]
            )
        |> required "data" (Decode.maybe Decode.string)
