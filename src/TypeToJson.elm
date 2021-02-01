module TypeToJson exposing (generate)

import TypeToJson.Generator as Generator
import TypeToJson.Generator.Types exposing (..)
import TypeToJson.Parser exposing (parse)


generate : String -> Result String String
generate input =
    case parse input of
        Ok ts ->
            Ok <| Generator.generate ts

        Err e ->
            Err e
