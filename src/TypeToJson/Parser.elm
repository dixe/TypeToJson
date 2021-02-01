module TypeToJson.Parser exposing (parse)

import Elm.Parser as Parser
import Elm.Processing
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.File
import Elm.Syntax.Node as Node
import Elm.Syntax.Type
import Elm.Syntax.TypeAlias as Tal
import Elm.Syntax.TypeAnnotation as Tan
import Parser as BaseParser
import TypeToJson.Types exposing (..)


type Error
    = NotSupported String
    | Other String


type alias Model =
    Result Error (List ValidType)


type alias ParseResult a =
    Result (List Error) a


parse : String -> Result String (List ValidType)
parse input =
    case Parser.parse input of
        Ok p ->
            case getDeclarations <| Elm.Processing.process Elm.Processing.init p of
                Ok types ->
                    Ok types

                Err err ->
                    Err <| String.join "\n" <| List.map errorToString err

        Err errs ->
            Err <| deadEndsToString errs


errorToString : Error -> String
errorToString e =
    case e of
        NotSupported s ->
            "NotSupported: " ++ s

        Other s ->
            "Error: " ++ s


deadEndsToString : List BaseParser.DeadEnd -> String
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


getDeclarations : Elm.Syntax.File.File -> ParseResult (List ValidType)
getDeclarations file =
    flattenParseResults <| List.map getDeclaration file.declarations


getDeclaration : Node.Node Declaration.Declaration -> ParseResult ValidType
getDeclaration node =
    case Node.value node of
        Declaration.AliasDeclaration al ->
            getAlias al

        Declaration.CustomTypeDeclaration c ->
            getType c

        n ->
            Err [ NotSupported "Only Type and Type alias is supported" ]


getConstructor : Elm.Syntax.Type.ValueConstructor -> ParseResult Constructor
getConstructor vc =
    let
        pArgs =
            flattenParseResults <| List.map (\a -> argument <| Node.value a) <| vc.arguments
    in
    case pArgs of
        Ok args ->
            Ok
                { name = Node.value vc.name
                , arguments = args
                }

        Err e ->
            Err e


argument : Tan.TypeAnnotation -> ParseResult TypeAnnotation
argument vs =
    typeAnnotation vs


getType : Elm.Syntax.Type.Type -> ParseResult ValidType
getType t =
    let
        consts : ParseResult (List Constructor)
        consts =
            flattenParseResults <|
                List.map
                    (\x -> getConstructor <| Node.value x)
                    t.constructors

        generics =
            List.map Node.value t.generics
    in
    case consts of
        Ok pcs ->
            case pcs of
                [] ->
                    Err [ Other "No custom type constructors" ]

                c :: cs ->
                    Ok <| CustomType (Node.value t.name) generics c cs

        Err es ->
            Err es


getAlias : Tal.TypeAlias -> ParseResult ValidType
getAlias ta =
    case typeAnnotation <| Node.value ta.typeAnnotation of
        Ok anno ->
            Ok <| TypeAlias (Node.value ta.name) (List.map Node.value ta.generics) anno

        Err n ->
            Err n


typeAnnotation : Tan.TypeAnnotation -> ParseResult TypeAnnotation
typeAnnotation anno =
    case anno of
        Tan.Record r ->
            map Record (getFields r)

        Tan.Typed mod arguments ->
            let
                ( _, name ) =
                    Node.value mod

                parsedArgs =
                    flattenParseResults <| List.map (\x -> x |> Node.value |> typeAnnotation) arguments
            in
            mapPr
                (\args ->
                    case name of
                        -- handle builtins like lists
                        "List" ->
                            case args of
                                a :: [] ->
                                    Ok <| Typed <| ListDef a

                                _ ->
                                    Err <| List.map Other [ "List with " ++ (String.fromInt <| List.length args) ++ " arguments" ]

                        n ->
                            Ok <| Typed <| Type n
                )
                parsedArgs

        Tan.Tupled arguments ->
            let
                parsedArgs =
                    flattenParseResults <| List.map (\x -> x |> Node.value |> typeAnnotation) arguments
            in
            map Tuple parsedArgs

        Tan.GenericType t ->
            Ok <| Typed <| Type t

        Tan.Unit ->
            Err <| [ NotSupported "Unit" ]

        Tan.GenericRecord _ _ ->
            Err <| [ NotSupported "GeneriRecord" ]

        Tan.FunctionTypeAnnotation _ _ ->
            Err <| [ NotSupported "Functions" ]


getFields : Tan.RecordDefinition -> ParseResult RecordDefinition
getFields rec =
    let
        rs =
            List.map (\n -> Node.value n) rec

        ls : ParseResult RecordDefinition
        ls =
            flattenParseResults <|
                List.map
                    (\( n, t ) ->
                        case typeAnnotation <| Node.value t of
                            Ok anno ->
                                Ok { name = Node.value n, anno = anno }

                            Err e ->
                                Err e
                    )
                    rs
    in
    ls



-- ParseResult HELPERS


flattenParseResults : List (ParseResult a) -> ParseResult (List a)
flattenParseResults prs =
    case prs of
        [] ->
            Ok []

        p :: pr ->
            combine p <| flattenParseResults pr


combine : ParseResult a -> ParseResult (List a) -> ParseResult (List a)
combine pr prs =
    case ( pr, prs ) of
        ( Ok a, Ok al ) ->
            Ok (a :: al)

        ( Err e, Ok _ ) ->
            Err e

        ( Ok _, Err e ) ->
            Err e

        ( Err e1, Err e2 ) ->
            Err (e1 ++ e2)


map : (a -> b) -> ParseResult a -> ParseResult b
map f r =
    case r of
        Ok a ->
            Ok <| f a

        Err e ->
            Err e


mapPr : (a -> ParseResult b) -> ParseResult a -> ParseResult b
mapPr f r =
    case r of
        Ok a ->
            f a

        Err e ->
            Err e
