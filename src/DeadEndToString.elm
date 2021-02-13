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
                        BaseParser.Expecting err ->
                            "Expecting: " ++ err ++ posString

                        BaseParser.ExpectingInt ->
                            "ExpectingInt" ++ posString

                        BaseParser.ExpectingHex ->
                            "ExpectingHex" ++ posString

                        BaseParser.ExpectingOctal ->
                            "ExpectingOctal" ++ posString

                        BaseParser.ExpectingBinary ->
                            "ExpectingBinary" ++ posString

                        BaseParser.ExpectingFloat ->
                            "ExpectingFloat" ++ posString

                        BaseParser.ExpectingNumber ->
                            "ExpectingNumber" ++ posString

                        BaseParser.ExpectingVariable ->
                            "ExpectingVariable" ++ posString

                        BaseParser.ExpectingSymbol err ->
                            "ExpectedSymbol: " ++ err ++ posString

                        BaseParser.ExpectingKeyword err ->
                            "ExpectingKeyword: " ++ err ++ posString

                        BaseParser.ExpectingEnd ->
                            "ExpectingEnd" ++ posString

                        BaseParser.UnexpectedChar ->
                            "UnexpectedChar" ++ posString

                        BaseParser.Problem err ->
                            "Problem: " ++ err ++ posString

                        BaseParser.BadRepeat ->
                            "BadRepeat" ++ posString
            in
            errMsg ++ "\n" ++ deadEndsToString ds
