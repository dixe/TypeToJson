module UnitTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import GeneratedTests.AnonymousRecord
import GeneratedTests.CustomType
import GeneratedTests.Record
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
        ]
