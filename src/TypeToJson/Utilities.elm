module TypeToJson.Utilities exposing (indent, indentWith, interpolate, interpolateAll)


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
