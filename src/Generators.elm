module Generators exposing (GeneraterResult, generate, generateFromJson)

import ElmParser as EP exposing (parse)
import Generator.Generator as Generator
import JsonParser exposing (Json, Member, Name, Number(..), Value(..))
import JsonPrettyPrinter exposing (prettyPrint)
import Mapper.FromJson as FromJson
import Mapper.FromTypes as FromTypes
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
                    let
                        json =
                            prettyPrint <| FromTypes.fromTypes ts
                    in
                    Ok <| ( json, generateFromValidType ts )

                Err e ->
                    Err e
    in
    Result.map (\( json, out ) -> { inputTypes = input, inputJson = json, output = out }) r


generateFromJson : Json -> Result String GeneraterResult
generateFromJson json =
    Result.map
        (\types ->
            { inputTypes = Debug.todo "inputTypes"
            , inputJson = Debug.todo " InputJson"
            , output = generateFromValidType types
            }
        )
        (FromJson.fromJson json)


generateFromValidType : List ValidType -> String
generateFromValidType types =
    Generator.generate types
