module UnitTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (..)
import TestGenerated.Record


suite : Test
suite =
    describe "a == Decode (Encode a) "
        [ test "Simple Record" <|
            \_ ->
                let
                    record : TestGenerated.Record.Rec
                    record =
                        { stringField = String, intField = Int }

                    encoded =
                        Encode.encode 0 <| TestGenerated.Record.recEncoder record

                    decoded =
                        Decode.decodeString TestGenerated.Record.recDecoder encoded
                in
                case decoded of
                    Ok rec ->
                        Expect.equal 0 0

                    Err ->
                        Expect.err "Decodede error"
        ]
