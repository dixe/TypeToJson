module TypeToJson.Interpolate exposing (interpolate, interpolateAll)


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
