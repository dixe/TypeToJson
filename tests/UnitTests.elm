module UnitTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import GeneratedTests.AnonymousRecord
import GeneratedTests.AnonymousTuples
import GeneratedTests.CustomType
import GeneratedTests.Record
import GeneratedTests.Tuples
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (..)


suite : Test
suite =
    describe "a == Decode (Encode a) "
        [ test "Simple Record" <|
            \_ ->
                let
                    record : GeneratedTests.Record.TestType
                    record =
                        { stringField = "StringData", intField = 3 }

                    encoded =
                        Encode.encode 0 <| GeneratedTests.Record.testTypeEncoder record

                    decoded =
                        Decode.decodeString GeneratedTests.Record.testTypeDecoder encoded
                in
                Expect.ok decoded
        , test "Anonymous Record" <|
            \_ ->
                let
                    record : GeneratedTests.AnonymousRecord.TestType
                    record =
                        GeneratedTests.AnonymousRecord.C1 { stringField = "StringData", intField = 3 } { field2 = "data2" }

                    encoded =
                        Encode.encode 0 <| GeneratedTests.AnonymousRecord.testTypeEncoder record

                    decoded =
                        Decode.decodeString GeneratedTests.AnonymousRecord.testTypeDecoder encoded
                in
                Expect.ok decoded
        , test "CustomTypes C1" <|
            \_ ->
                Expect.ok <| testCustomType <| GeneratedTests.CustomType.C1
        , test "CustomTypes C2" <|
            \_ ->
                Expect.ok <| testCustomType <| GeneratedTests.CustomType.C2 "stringData"
        , test "AnonymousTuples" <|
            \_ ->
                let
                    data =
                        GeneratedTests.AnonymousTuples.TestType ( 1, 2 )

                    encoded =
                        Encode.encode 0 <| GeneratedTests.AnonymousTuples.testTypeEncoder data

                    decoded =
                        Decode.decodeString GeneratedTests.AnonymousTuples.testTypeDecoder encoded
                in
                Expect.ok decoded
        , test "Tuples" <|
            \_ ->
                let
                    data : GeneratedTests.Tuples.TestType
                    data =
                        ( "TestData", 2 )

                    encoded =
                        Encode.encode 0 <| GeneratedTests.Tuples.testTypeEncoder data

                    decoded =
                        Decode.decodeString GeneratedTests.Tuples.testTypeDecoder encoded
                in
                Expect.ok decoded
        ]


testCustomType data =
    let
        encoded =
            Encode.encode 0 <| GeneratedTests.CustomType.testTypeEncoder data
    in
    Decode.decodeString GeneratedTests.CustomType.testTypeDecoder encoded
