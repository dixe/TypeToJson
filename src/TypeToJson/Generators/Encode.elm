module TypeToJson.Generators.Encode exposing (encoderDeclaration, generate)

import List.Extra
import String.Extra exposing (decapitalize)
import TypeToJson.Generators.Types exposing (..)
import TypeToJson.Interpolate exposing (..)
import TypeToJson.Types exposing (..)


imports =
    [ "Json.Encode as Encode"
    ]


generate : Ctx -> List ValidType -> Ctx
generate ctx types =
    let
        generated : Ctx
        generated =
            List.foldl validType ctx types
    in
    { generated | imports = ctx.imports ++ imports }


encoderDeclaration : Coder -> String
encoderDeclaration d =
    let
        typeArgs =
            String.join " " <| List.map (\x -> "Encode.Encoder " ++ x ++ "->") d.generics

        args =
            String.join " " <| List.map (\x -> x ++ "Encoder") d.generics
    in
    """
{{name}}Encoder : {{typeArgs}} Encode.Encoder {{Name}}
{{name}}Encoder {{args}} =
{{impl}}"""
        |> interpolateAll
            [ ( "name", decapitalize d.typeName )
            , ( "Name", d.typeName )
            , ( "impl", indent d.implementation )
            , ( "typeArgs", typeArgs )
            , ( "args", args )
            ]


spaces : Int
spaces =
    2


indent : String -> String
indent =
    indentWith spaces


indentWith : Int -> String -> String
indentWith space s =
    s
        |> String.lines
        |> List.map (\x -> String.repeat space " " ++ x)
        |> String.join "\n"



-- CTX functions


addEncoder : Ctx -> Coder -> Ctx
addEncoder ctx encoder =
    { ctx | encoders = ctx.encoders ++ [ encoder ] }



--generated


validType : ValidType -> Ctx -> Ctx
validType t ctx =
    case t of
        TypeAlias name generics anno ->
            typeAlias name generics anno ctx

        CustomType name generics c cs ->
            customType name generics (c :: cs) ctx


customType : Name -> GenericsAnnotation -> List Constructor -> Ctx -> Ctx
customType name generics consts ctx =
    let
        gen =
            constructors consts
    in
    addEncoder ctx { typeName = name, implementation = gen, generics = generics }


constructors : List Constructor -> String
constructors consts =
    let
        cs =
            "[ {{cs}}"
                ++ "\n]"
                |> interpolateAll
                    [ ( "cs", String.join "\n, " (List.map constructor consts) )
                    ]

        gen =
            """Encode.oneOf
{{gen}}

"""
                |> interpolateAll
                    [ ( "gen", indent cs )
                    ]
    in
    gen


caseHelper : Name -> String
caseHelper name =
    """(\\s ->
   if s == "{{name}}" then
      Encode.succeed {{name}}
   else
      Encode.fail ""
)""" |> interpolate "name" name


constructorNoArgument name =
    let
        base =
            """Encode.string
    |> Encode.andThen"""

        gen =
            "{{caseHelper}}" |> interpolate "caseHelper" (caseHelper name)
    in
    (base ++ "\n{{gen}}") |> interpolate "gen" (indentWith 8 <| gen)


constructorWithArgument : Name -> List TypeAnnotation -> String
constructorWithArgument name args =
    """Encode.succeed (\\a -> a)
  |> required "{{name}}"
    (Encode.succeed {{name}}
{{args}}
    ) """
        |> interpolateAll
            [ ( "name", name )
            , ( "args", String.join "\n" <| arguments 0 args )
            ]


arguments : Int -> List TypeAnnotation -> List String
arguments index tas =
    case tas of
        [] ->
            []

        t :: ts ->
            indentWith 8
                (String.join " "
                    [ "|> required \"arg{{index}}\"" |> interpolate "index" (String.fromInt index)
                    , typeAnnotation t
                    ]
                )
                :: arguments (index + 1) ts


constructor : Constructor -> String
constructor c =
    if c.arguments == [] then
        constructorNoArgument c.name

    else
        constructorWithArgument c.name c.arguments


typeAlias : Name -> GenericsAnnotation -> TypeAnnotation -> Ctx -> Ctx
typeAlias name generics anno =
    case anno of
        Record rec ->
            record name generics rec

        Tuple args ->
            identity

        --            tuple name generics args
        Typed td ->
            identity



--            typed td generics


typeAnnotation : TypeAnnotation -> String
typeAnnotation anno =
    case anno of
        Record rec ->
            indentWith 8 <| anonymousRecord rec

        Tuple def ->
            ""

        --            indentWith 8 <| anonymousTuple def
        Typed td ->
            ""



--            typeDef td


record : Name -> GenericsAnnotation -> RecordDefinition -> Ctx -> Ctx
record name generics def ctx =
    let
        rows =
            List.map recordField def

        impl =
            """Encode.object =
{{rows}}"""
                |> interpolateAll
                    [ ( "rows", String.join "\n" rows )
                    ]
    in
    addEncoder ctx { typeName = name, implementation = impl, generics = generics }


recordField : { name : String, anno : TypeAnnotation } -> String
recordField { name, anno } =
    """ ("{{fieldName}}", {{encoder}} {{fieldName}}  """
        |> interpolateAll
            [ ( "fieldName", name )
            , ( "encoder", typeAnnotation anno )
            ]


anonymousRecord : RecordDefinition -> String
anonymousRecord def =
    String.join "\n" <| List.map recordField def
