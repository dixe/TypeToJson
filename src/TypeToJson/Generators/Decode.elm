module TypeToJson.Generators.Decode exposing (decoderDeclaration, generate)

import List.Extra
import String.Extra exposing (decapitalize)
import TypeToJson.Generators.Types exposing (..)
import TypeToJson.Interpolate exposing (..)
import TypeToJson.Types exposing (..)


imports =
    [ "Json.Decode as Decode"
    , "Json.Decode.Pipeline exposing (required)"
    ]


generate : Ctx -> List ValidType -> Ctx
generate ctx types =
    let
        generated : Ctx
        generated =
            List.foldl validType ctx types
    in
    { generated | imports = ctx.imports ++ imports }


decoderDeclaration : Coder -> String
decoderDeclaration d =
    let
        typeArgs =
            String.join " " <| List.map (\x -> "Decode.Decoder " ++ x ++ "->") d.generics

        args =
            String.join " " <| List.map (\x -> x ++ "Decoder") d.generics
    in
    """
{{name}}Decoder : {{typeArgs}} Decode.Decoder {{Name}}
{{name}}Decoder {{args}} =
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


addDecoder : Ctx -> Coder -> Ctx
addDecoder ctx decoder =
    { ctx | decoders = ctx.decoders ++ [ decoder ] }



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
    addDecoder ctx { typeName = name, implementation = gen, generics = generics }


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
            """Decode.oneOf
{{gen}}

"""
                |> interpolateAll
                    [ ( "gen", indent cs )
                    ]

        d =
            l "CONSTRUCT" gen
    in
    gen


caseHelper : Name -> String
caseHelper name =
    """(\\s ->
   if s == "{{name}}" then
      Decode.succeed {{name}}
   else
      Decode.fail ""
)""" |> interpolate "name" name


constructorNoArgument name =
    let
        base =
            """Decode.string
    |> Decode.andThen"""

        gen =
            "{{caseHelper}}" |> interpolate "caseHelper" (caseHelper name)
    in
    (base ++ "\n{{gen}}") |> interpolate "gen" (indentWith 8 <| gen)


constructorWithArgument : Name -> List TypeAnnotation -> String
constructorWithArgument name args =
    """Decode.succeed identity(\\a -> a)
  |> required "{{name}}"
    (Decode.succeed {{name}}
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
            tuple name generics args

        Typed td ->
            typed td generics


typed : TypeDef -> GenericsAnnotation -> Ctx -> Ctx
typed td generics ctx =
    let
        d =
            l "Typed" td
    in
    ctx


typeAnnotation : TypeAnnotation -> String
typeAnnotation anno =
    case anno of
        Record rec ->
            indentWith 8 <| anonymousRecord rec

        Tuple def ->
            indentWith 8 <| anonymousTuple def

        Typed td ->
            typeDef td


anonymousRecord : RecordDefinition -> String
anonymousRecord def =
    let
        constString =
            """{{fieldName}} = {{fieldName}}"""

        const =
            "{ {{const}} }"
                |> interpolate "const"
                    (String.join ", " <|
                        List.map (\x -> constString |> interpolate "fieldName" x.name) def
                    )

        reqString =
            """|> required "{{fieldName}}" {{decoder}}"""

        requires =
            String.join "\n" <|
                List.map
                    (\x ->
                        reqString
                            |> interpolate "fieldName" x.name
                            |> interpolate "decoder" (typeAnnotation x.anno)
                    )
                    def
    in
    anonType { requires = requires, construction = const, names = List.map (\x -> x.name) def }


anonymousTuple : List TypeAnnotation -> String
anonymousTuple def =
    let
        withNames =
            List.map (\( index, anno ) -> { name = "item" ++ String.fromInt index, anno = anno }) <| List.Extra.zip (List.range 0 (List.length def)) def

        constString =
            """{{fieldName}}"""

        const =
            "( {{const}} )"
                |> interpolate "const"
                    (String.join ", " <|
                        List.map (\x -> constString |> interpolate "fieldName" x.name) withNames
                    )

        reqString =
            """|> required "{{fieldName}}" {{decoder}}"""

        requires =
            String.join "\n" <|
                List.map
                    (\x ->
                        reqString
                            |> interpolate "fieldName" x.name
                            |> interpolate "decoder" (typeAnnotation x.anno)
                    )
                    withNames
    in
    anonType { requires = requires, construction = const, names = List.map (\x -> x.name) withNames }


anonType : { names : List String, requires : String, construction : String } -> String
anonType { requires, construction, names } =
    let
        args =
            String.join " " names
    in
    """
(Decode.succeed
    (\\{{args}} -> {{const}} )
{{requires}}
)"""
        |> interpolateAll
            [ ( "args", args )
            , ( "const", construction )
            , ( "requires", indentWith 4 requires )
            ]


tuple : Name -> GenericsAnnotation -> List TypeAnnotation -> Ctx -> Ctx
tuple name generics args ctx =
    ctx


record : Name -> GenericsAnnotation -> RecordDefinition -> Ctx -> Ctx
record name generics def ctx =
    let
        gen =
            String.join "\n" <| recordDefinition def

        impl =
            """Decode.succeed {{Name}}
{{gen}} """
                |> interpolateAll
                    [ ( "Name", name )
                    , ( "gen", indent gen )
                    , ( "name", decapitalize name )
                    ]
    in
    addDecoder ctx { typeName = name, implementation = impl, generics = generics }


recordDefinition : RecordDefinition -> List String
recordDefinition rec =
    case rec of
        [] ->
            []

        rd :: tail ->
            recordField rd.name rd.anno :: recordDefinition tail


recordField : Name -> TypeAnnotation -> String
recordField name td =
    """|> required "{{name}}" {{decoder}}"""
        |> interpolateAll
            [ ( "name", name )
            , ( "decoder", typeAnnotation td )
            ]


typeDef : TypeDef -> String
typeDef td =
    case td of
        Type t ->
            case t of
                "String" ->
                    "Decode.string"

                "Int" ->
                    "Decode.int"

                n ->
                    "(Decode.lazy (\\_ -> {{name}}Decoder))" |> interpolate "name" (decapitalize n)

        ListDef arg ->
            "(Decode.list {{decoder}})" |> interpolate "decoder" (typeAnnotation arg)



--DEBUG


l =
    Debug.log


ll =
    Debug.log "decode"
