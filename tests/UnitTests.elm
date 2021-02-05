module UnitTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import GeneratedTests.AnonymousRecord
import GeneratedTests.AnonymousTuples
import GeneratedTests.CustomType
import GeneratedTests.Generics
import GeneratedTests.NestedRecord
import GeneratedTests.NestedTuples
import GeneratedTests.Record
import GeneratedTests.Sets
import GeneratedTests.Tuples
import Json.Decode as Decode
import Json.Encode as Encode
import Set
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
        , test "NestedTuples" <|
            \_ ->
                let
                    data : GeneratedTests.NestedTuples.TestType
                    data =
                        ( "TestData", ( 1, 3 ) )

                    encoded =
                        Encode.encode 0 <| GeneratedTests.NestedTuples.testTypeEncoder data

                    decoded =
                        Decode.decodeString GeneratedTests.NestedTuples.testTypeDecoder encoded
                in
                Expect.ok decoded
        , test "NestedRecord" <|
            \_ ->
                let
                    data : GeneratedTests.NestedRecord.TestType
                    data =
                        { stringField = "StringData"
                        , intField =
                            { stringField = "StringDate2"
                            , floatField = 2
                            }
                        }

                    encoded =
                        Encode.encode 0 <| GeneratedTests.NestedRecord.testTypeEncoder data

                    decoded =
                        Decode.decodeString GeneratedTests.NestedRecord.testTypeDecoder encoded
                in
                Expect.ok decoded
        , test "Generics" <|
            \_ ->
                let
                    data : GeneratedTests.Generics.TestType Int String
                    data =
                        GeneratedTests.Generics.C2 1 2 "TestData"

                    encoded =
                        Encode.encode 0 <| GeneratedTests.Generics.testTypeEncoder Encode.int Encode.string data

                    decoded =
                        Decode.decodeString (GeneratedTests.Generics.testTypeDecoder Decode.int Decode.string) encoded
                in
                Expect.ok decoded
        , test "Sets" <|
            \_ ->
                let
                    data : GeneratedTests.Sets.TestType String
                    data =
                        Set.fromList [ "TestData", "genData" ]

                    encoded =
                        Encode.encode 0 <| GeneratedTests.Sets.testTypeEncoder Encode.string data

                    decoded =
                        Decode.decodeString (GeneratedTests.Sets.testTypeDecoder Decode.string) encoded
                in
                Expect.ok decoded
        ]


testCustomType data =
    let
        encoded =
            Encode.encode 0 <| GeneratedTests.CustomType.testTypeEncoder data
    in
    Decode.decodeString GeneratedTests.CustomType.testTypeDecoder encoded
