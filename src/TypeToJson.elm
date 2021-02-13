module TypeToJson exposing (GeneraterResult, generate, generateFromJson)

import Json exposing (Json, Member(..), Name, Number(..), Value(..))
import TypeToJson.Generator as Generator
import TypeToJson.Parser as TP exposing (parse)
import TypeToJson.Types exposing (..)


type alias GeneraterResult =
    { inputTypes : String
    , inputJson : String
    , output : String
    }


generate : String -> Result String GeneraterResult
generate input =
    let
        r =
            case TP.parse input of
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
