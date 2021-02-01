module TypeToJson exposing (generate)

import TypeToJson.Generators.Generator as Generator
import TypeToJson.Generators.Types exposing (..)
import TypeToJson.Parser exposing (parse)


moduleString =
    "module Temp exposing (..)\n"


generate : String -> Result String String
generate input =
    case parse <| moduleString ++ input of
        Ok ts ->
            Ok <| Generator.generate ts

        Err e ->
            Err e
