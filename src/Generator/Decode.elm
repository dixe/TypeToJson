module Generator.Decode exposing (decoderDeclaration, generate)

import Generator.Types exposing (..)
import Generator.Utilities exposing (..)
import Json.Decode as Decode
import Json.Decode.Extra
import List.Extra
import Set
import String.Extra exposing (decapitalize)
import Types exposing (..)


imports =
    [ "Json.Decode as Decode"
    , "Json.Decode.Pipeline exposing (required)"
    , "Set exposing(Set)"
    , "Dict exposing(Dict)"
    , "Json.Decode.Extra"
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
        --TODO make generics into comparable where needed, like set and
        typeArgs =
            String.join " " <| List.map (\x -> "Decode.Decoder " ++ x ++ "->") d.mappedGenerics

        args =
            String.join " " <| List.map (\x -> x ++ "Decoder") d.generics
    in
    """
{{name}}Decoder : {{typeArgs}} Decode.Decoder ({{Name}} {{generics}})
{{name}}Decoder {{args}} =
{{impl}}"""
        |> interpolateAll
            [ ( "name", decapitalize d.typeName )
            , ( "Name", d.typeName )
            , ( "impl", indent d.implementation )
            , ( "typeArgs", typeArgs )
            , ( "args", args )
            , ( "generics", String.join " " d.mappedGenerics )
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
    addDecoder ctx { typeName = name, implementation = gen, generics = generics, mappedGenerics = generics }


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
    """Decode.succeed identity
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
            typed name td generics


typed : Name -> TypeDef -> GenericsAnnotation -> Ctx -> Ctx
typed name td generics ctx =
    let
        args =
            String.join " " <| List.map (\g -> g ++ "Decoder") generics

        impl =
            "{{decoder}} {{args}}"
                |> interpolateAll
                    [ ( "decoder", typeDef td )
                    , ( "args", args )
                    ]

        mappedGenerics =
            mapGenerics td generics
    in
    addDecoder ctx { typeName = name, implementation = impl, generics = generics, mappedGenerics = mappedGenerics }


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
    let
        rows : Int -> List TypeAnnotation -> List String
        rows index tas =
            case tas of
                [] ->
                    []

                t :: ts ->
                    ("""|> required "item{{index}}" {{encoder}}"""
                        |> interpolateAll
                            [ ( "index", String.fromInt index )
                            , ( "encoder", typeAnnotation t )
                            ]
                    )
                        :: rows (index + 1) ts

        gen =
            String.join "\n" <| rows 0 args

        impl : String
        impl =
            anonymousTuple args
    in
    addDecoder ctx { typeName = name, implementation = impl, generics = generics, mappedGenerics = generics }


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
                    ]
    in
    addDecoder ctx { typeName = name, implementation = impl, generics = generics, mappedGenerics = generics }


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
        Type t args ->
            case t of
                "String" ->
                    "Decode.string"

                "Int" ->
                    "Decode.int"

                "Float" ->
                    "Decode.float"

                "Set" ->
                    "Json.Decode.Extra.set"

                "Maybe" ->
                    "Decode.maybe"

                n ->
                    "(Decode.lazy (\\_ -> {{name}}Decoder {{args}}))"
                        |> interpolateAll
                            [ ( "name", decapitalize n )
                            , ( "args", String.join "," <| List.map typeAnnotation args )
                            ]

        MaybeDef ta ->
            "(Decode.maybe {{decoder}})" |> interpolate "decoder" (typeAnnotation ta)

        ListDef ta ->
            "(Decode.list {{decoder}})" |> interpolate "decoder" (typeAnnotation ta)

        DictDef k v ->
            "Json.Decode.Extra.dict2 {{keyDecoder}} {{valDecoder}}"
                |> interpolateAll
                    [ ( "keyDecoder", typeAnnotation k )
                    , ( "valDecoder", typeAnnotation v )
                    ]

        ResultDef err ok ->
            """(Decode.oneOf [ Decode.succeed Ok
    |> required "Ok" {{okDecoder}}
, Decode.succeed Err
    |> required "Err" {{errDecoder}}
])
"""
                |> interpolateAll
                    [ ( "okDecoder", typeAnnotation ok )
                    , ( "errDecoder", typeAnnotation err )
                    ]
