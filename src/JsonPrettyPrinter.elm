module JsonPrettyPrinter exposing (prettyPrint)

import Generator.Utilities exposing (..)
import JsonParser exposing (..)


prettyPrint : Json -> String
prettyPrint json =
    value 0 json


type alias Indent =
    Int


value : Indent -> Value -> String
value indent val =
    case val of
        VObject members ->
            object indent members

        VArray vals ->
            array indent vals

        VTuple vals ->
            tuple indent vals

        VString string ->
            "\"" ++ string ++ "\""

        VNumber number ->
            case number of
                NInt n ->
                    String.fromInt n

                NFloat f ->
                    String.fromFloat f

        VTrue ->
            "true"

        VFalse ->
            "false"

        VNull ->
            "null"

        VMaybe v ->
            """anyOf [ null,
{{val}}
       ]"""
                |> interpolate "val" (value indent v)

        VCustom name vals ->
            """anyOf [{{options}}
  ]"""
                |> interpolate "options" (String.join ",\n " <| List.map (value indent) vals)


tuple : Indent -> List Value -> String
tuple indent vals =
    array indent vals


array : Indent -> List Value -> String
array indent vals =
    let
        mems =
            String.join ",\n  " <| List.map (value indent) vals
    in
    "[ {{mems}} ] " |> interpolate "mems" mems


object : Indent -> List Member -> String
object indent members =
    let
        mems =
            String.join ",\n  " <| List.map (member indent) members
    in
    "{ {{mems}} } " |> interpolate "mems" mems


member : Indent -> Member -> String
member indent mem =
    "\"{{name}}\" : {{val}}"
        |> interpolateAll
            [ ( "name", mem.name )
            , ( "val", value indent mem.value )
            ]
