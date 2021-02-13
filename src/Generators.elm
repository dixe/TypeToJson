module Generators exposing (GeneraterResult, generate, generateFromJson)

import ElmParser as EP exposing (parse)
import Generator.Generator as Generator
import JsonParser exposing (Json, Member(..), Name, Number(..), Value(..))
import Types exposing (..)


type alias GeneraterResult =
    { inputTypes : String
    , inputJson : String
    , output : String
    }


generate : String -> Result String GeneraterResult
generate input =
    let
        r =
            case EP.parse input of
                Ok ts ->
                    Ok <| generateFromValidType ts

                Err e ->
                    Err e
    in
    Result.map (\out -> { inputTypes = input, inputJson = "", output = out }) r


generateFromJson : Json -> Result String GeneraterResult
generateFromJson json =
    Err "Not imeplemtned"


generateFromValidType : List ValidType -> String
generateFromValidType types =
    Generator.generate types
