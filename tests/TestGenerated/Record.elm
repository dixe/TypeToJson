module TestInputs.Record exposing (..)


type alias Rec =
    { stringField : String, intField : Int }


import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode

--ENCODERS


recEncoder :  Encode.Value Rec
recEncoder  =
  Encode.object
   ("stringField",  stringField )
   ("intField",  intField )

--DECODERS

recDecoder :  Decode.Decoder Rec
recDecoder  =
  Decode.succeed Rec
    |> required "stringField" Decode.string
    |> required "intField" Decode.int 

