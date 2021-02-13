module JsonPrettyPrinter exposing (prettyPrint)

import Generator.Utilities exposing (..)
import JsonParser exposing (..)


prettyPrint : Json -> String
prettyPrint json =
    let
        v =
            value 0 json

        d =
            Debug.log "PrettyPrint" json
    in
    v


type alias Indent =
    Int


value : Indent -> Value -> String
value indent val =
    case val of
        VObject members ->
            object indent members

        VArray vals ->
            array indent vals

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


array : Indent -> List Value -> String
array indent vals =
    let
        mems =
            String.join ",\n  " <| List.map (\v -> value indent v) vals
    in
    "[ {{mems}} ] " |> interpolate "mems" mems


object : Indent -> List Member -> String
object indent members =
    let
        mems =
            String.join ",\n  " <| List.map (\m -> member indent m) members
    in
    "{ {{mems}} } " |> interpolate "mems" mems


member : Indent -> Member -> String
member indent mem =
    "\"{{name}}\" : {{val}}"
        |> interpolateAll
            [ ( "name", mem.name )
            , ( "val", value indent mem.value )
            ]
