module DeadEndsToString exposing (deadEndsToString)

import Parser


deadEndsToString : List Parser.DeadEnd -> String
deadEndsToString de =
    case de of
        [] ->
            ""

        d :: ds ->
            let
                posString =
                    " at (col, row) : (" ++ String.fromInt d.row ++ ", " ++ String.fromInt d.col ++ ")"

                errMsg =
                    case d.problem of
                        Parser.Expecting err ->
                            "Expecting: " ++ err ++ posString

                        Parser.ExpectingInt ->
                            "ExpectingInt" ++ posString

                        Parser.ExpectingHex ->
                            "ExpectingHex" ++ posString

                        Parser.ExpectingOctal ->
                            "ExpectingOctal" ++ posString

                        Parser.ExpectingBinary ->
                            "ExpectingBinary" ++ posString

                        Parser.ExpectingFloat ->
                            "ExpectingFloat" ++ posString

                        Parser.ExpectingNumber ->
                            "ExpectingNumber" ++ posString

                        Parser.ExpectingVariable ->
                            "ExpectingVariable" ++ posString

                        Parser.ExpectingSymbol err ->
                            "ExpectedSymbol: " ++ err ++ posString

                        Parser.ExpectingKeyword err ->
                            "ExpectingKeyword: " ++ err ++ posString

                        Parser.ExpectingEnd ->
                            "ExpectingEnd" ++ posString

                        Parser.UnexpectedChar ->
                            "UnexpectedChar" ++ posString

                        Parser.Problem err ->
                            "Problem: " ++ err ++ posString

                        Parser.BadRepeat ->
                            "BadRepeat" ++ posString
            in
            errMsg ++ "\n" ++ deadEndsToString ds
