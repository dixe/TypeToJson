module UnitTests exposing (..)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import GeneratedTests.AnonymousRecord
import GeneratedTests.AnonymousTuples
import GeneratedTests.Combined1
import GeneratedTests.CustomType
import GeneratedTests.Dicts
import GeneratedTests.Generics
import GeneratedTests.Maybes
import GeneratedTests.NestedRecord
import GeneratedTests.NestedTuples
import GeneratedTests.Record
import GeneratedTests.Results
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
                    data : GeneratedTests.Record.TestType
                    data =
                        { stringField = "StringData", intField = 3 }

                    encoded =
                        Encode.encode 0 <| GeneratedTests.Record.testTypeEncoder data

                    decoded =
                        Decode.decodeString GeneratedTests.Record.testTypeDecoder encoded
                in
                match data decoded
        , test "Anonymous Record" <|
            \_ ->
                let
                    data : GeneratedTests.AnonymousRecord.TestType
                    data =
                        GeneratedTests.AnonymousRecord.C1 { stringField = "StringData", intField = 3 } { field2 = "data2" }

                    encoded =
                        Encode.encode 0 <| GeneratedTests.AnonymousRecord.testTypeEncoder data

                    decoded =
                        Decode.decodeString GeneratedTests.AnonymousRecord.testTypeDecoder encoded
                in
                match data decoded
        , test "CustomTypes C1" <|
            \_ ->
                testCustomType <| GeneratedTests.CustomType.C1
        , test "CustomTypes C2" <|
            \_ ->
                testCustomType <| GeneratedTests.CustomType.C2 "stringData"
        , test "Maybes_Just" <|
            \_ ->
                testMaybes <| Just "lol"
        , test "Maybes_Nothgin" <|
            \_ ->
                testMaybes <| Nothing
        , test "Dict" <|
            \_ ->
                testDicts
        , test "Result_Err" <|
            \_ ->
                testResultsErr
        , test "Result_ok" <|
            \_ ->
                testResultsOk
        , test "Combined1_1" <|
            \_ ->
                testCombined1_1
        , test "Combined1_2" <|
            \_ ->
                testCombined1_2
        , test "Combined1_3" <|
            \_ ->
                testCombined1_3
        , test "Combined1_4" <|
            \_ ->
                testCombined1_4
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
                match data decoded
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
                match data decoded
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
                match data decoded
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
                match data decoded
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
                match data decoded
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
                match data decoded
        ]


testCombined1_1 =
    let
        data =
            { data = Nothing, result = Err GeneratedTests.Combined1.BadError }

        encoded =
            Encode.encode 0 <| GeneratedTests.Combined1.testTypeEncoder data
    in
    match data <| Decode.decodeString GeneratedTests.Combined1.testTypeDecoder encoded


testCombined1_2 =
    let
        data =
            { data = Nothing, result = Err <| GeneratedTests.Combined1.GoodError "Good" }

        encoded =
            Encode.encode 0 <| GeneratedTests.Combined1.testTypeEncoder data
    in
    match data <| Decode.decodeString GeneratedTests.Combined1.testTypeDecoder encoded


testCombined1_3 =
    let
        data =
            { data = Nothing, result = Err <| GeneratedTests.Combined1.OkError "Ok" }

        encoded =
            Encode.encode 0 <| GeneratedTests.Combined1.testTypeEncoder data

        decoded =
            Decode.decodeString GeneratedTests.Combined1.testTypeDecoder encoded
    in
    match data decoded


testCombined1_4 =
    let
        data =
            { data = Nothing, result = Err GeneratedTests.Combined1.MehError }

        encoded =
            Encode.encode 0 <| GeneratedTests.Combined1.testTypeEncoder data

        decoded =
            Decode.decodeString GeneratedTests.Combined1.testTypeDecoder encoded
    in
    match data decoded


testDicts =
    let
        data =
            Dict.fromList [ ( 2, "testData" ), ( 3, "Test2:" ) ]

        encoded =
            Encode.encode 0 <| GeneratedTests.Dicts.testTypeEncoder data
    in
    match data <| Decode.decodeString GeneratedTests.Dicts.testTypeDecoder encoded


testResultsOk =
    let
        data =
            Ok "Data"

        encoded =
            Encode.encode 0 <| GeneratedTests.Results.testTypeEncoder data
    in
    match data <| Decode.decodeString GeneratedTests.Results.testTypeDecoder encoded


testResultsErr =
    let
        data =
            Err 2

        encoded =
            Encode.encode 0 <| GeneratedTests.Results.testTypeEncoder data
    in
    match data <| Decode.decodeString GeneratedTests.Results.testTypeDecoder encoded


testMaybes data =
    let
        encoded =
            Encode.encode 0 <| GeneratedTests.Maybes.testTypeEncoder data

        decoded =
            Decode.decodeString GeneratedTests.Maybes.testTypeDecoder encoded
    in
    match data decoded


testCustomType data =
    let
        encoded =
            Encode.encode 0 <| GeneratedTests.CustomType.testTypeEncoder data

        decoded =
            Decode.decodeString GeneratedTests.CustomType.testTypeDecoder encoded
    in
    match data decoded


match : a -> Result b a -> Expectation
match data res =
    let
        eq =
            case res of
                Ok a ->
                    data == a

                Err _ ->
                    False
    in
    Expect.true "Is right and equal" <| eq
