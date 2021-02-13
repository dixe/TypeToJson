module Generator.Generator exposing (generate)

import Generator.Decode as DC
import Generator.Encode as EC
import Generator.Types exposing (..)
import Generator.Utilities exposing (..)
import Types exposing (..)


generate : List ValidType -> String
generate types =
    let
        ctx =
            EC.generate (DC.generate empty types) types
    in
    """{{imports}}

-- ENCODERS

{{encoders}}

-- DECODERS
{{decoders}}
"""
        |> interpolateAll
            [ ( "imports", String.join "\n" <| List.map (\x -> "import " ++ x) ctx.imports )
            , ( "decoders", String.join "\n" <| List.map DC.decoderDeclaration ctx.decoders )
            , ( "encoders", String.join "\n" <| List.map EC.encoderDeclaration ctx.encoders )
            ]
