module TypeToJson.Generator exposing (generate)

import TypeToJson.Generator.Decode as DC
import TypeToJson.Generator.Encode as EC
import TypeToJson.Generator.Types exposing (..)
import TypeToJson.Interpolate exposing (..)
import TypeToJson.Types exposing (..)


generate : List ValidType -> String
generate types =
    let
        ctx =
            EC.generate (DC.generate empty types) types
    in
    """{{imports}}

--ENCODERS

{{encoders}}

--DECODERS
{{decoders}}
"""
        |> interpolateAll
            [ ( "imports", String.join "\n" <| List.map (\x -> "import " ++ x) ctx.imports )
            , ( "decoders", String.join "\n" <| List.map DC.decoderDeclaration ctx.decoders )
            , ( "encoders", String.join "\n" <| List.map EC.encoderDeclaration ctx.encoders )
            ]
