module Generator.Utilities exposing (indent, indentWith, interpolate, interpolateAll, mapGenerics)

import Types exposing (GenericsAnnotation, TypeDef(..))


interpolateAll : List ( String, String ) -> String -> String
interpolateAll replacements orig =
    case replacements of
        [] ->
            orig

        ( key, replace ) :: rest ->
            interpolateAll rest <| interpolate key replace orig


interpolate : String -> String -> String -> String
interpolate key replace orig =
    String.replace ("{{" ++ key ++ "}}") replace orig


spaces : Int
spaces =
    4


indent : String -> String
indent =
    indentWith spaces


indentWith : Int -> String -> String
indentWith space s =
    s
        |> String.lines
        |> List.map (\x -> String.repeat space " " ++ x)
        |> String.join "\n"


mapGenerics : TypeDef -> GenericsAnnotation -> GenericsAnnotation
mapGenerics td generics =
    case td of
        Type t args ->
            case t of
                "Set" ->
                    [ "comparable" ]

                _ ->
                    generics

        _ ->
            generics
