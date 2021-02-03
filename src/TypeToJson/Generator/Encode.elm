module TypeToJson.Generator.Encode exposing (encoderDeclaration, generate)

import List.Extra
import String.Extra exposing (decapitalize)
import TypeToJson.Generator.Types exposing (..)
import TypeToJson.Types exposing (..)
import TypeToJson.Utilities exposing (..)


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
    """
{{name}}Encoder : {{typeArgs}} -> Encode.Value
{{name}}Encoder {{args}} =
{{impl}}"""
        |> interpolateAll
            [ ( "name", decapitalize d.typeName )
            , ( "Name", d.typeName )
            , ( "impl", indent d.implementation )
            , ( "typeArgs", d.typeName )
            , ( "args", decapitalize d.typeName )
            ]



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
            constructors name consts
    in
    addEncoder ctx { typeName = name, implementation = gen, generics = generics }


constructors : Name -> List Constructor -> String
constructors name consts =
    let
        cs =
            String.join "\n" (List.map constructor consts)

        gen =
            """case {{name}} of
{{constructors}}
"""
                |> interpolateAll
                    [ ( "constructors", indent cs )
                    , ( "name", decapitalize name )
                    ]
    in
    gen


constructorNoArgument : Name -> String
constructorNoArgument name =
    """{{name}} -> Encode.string "{{name}}" """ |> interpolate "name" name


constructorWithArgument : Name -> List TypeAnnotation -> String
constructorWithArgument name args =
    let
        rows =
            []
    in
    """{{name}} {{args}} ->
    Encode.object [ ("{{name}}" , Encode.object
{{argRows}}
           )
       ]
 """
        |> interpolateAll
            [ ( "name", name )
            , ( "args", String.join " " <| argumentsList (List.length args) )
            , ( "argRows", indentWith 12 <| "[" ++ (String.join "\n, " <| arguments 0 args) ++ "\n]" )
            ]


argumentsList : Int -> List String
argumentsList count =
    List.map (\x -> "arg" ++ String.fromInt x) <| List.range 0 (count - 1)


arguments : Int -> List TypeAnnotation -> List String
arguments index tas =
    case tas of
        [] ->
            []

        t :: ts ->
            ("""("arg{{index}}", {{encoder}} arg{{index}})"""
                |> interpolateAll
                    [ ( "index", String.fromInt index )
                    , ( "encoder", typeAnnotation t )
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

        --            tuple name generics args
        Typed td ->
            identity


tuple : Name -> GenericsAnnotation -> List TypeAnnotation -> Ctx -> Ctx
tuple name generics types ctx =
    let
        rows =
            []

        impl =
            """Encode.object
       [
{{rows}}
       ]"""
                |> interpolateAll
                    [ ( "rows", indentWith 8 <| "make rows" )
                    ]
    in
    addEncoder ctx { typeName = name, implementation = impl, generics = generics }


record : Name -> GenericsAnnotation -> RecordDefinition -> Ctx -> Ctx
record name generics def ctx =
    let
        rows =
            List.map (recordField name) def

        impl =
            """Encode.object
       [
{{rows}}
      ]"""
                |> interpolateAll
                    [ ( "rows", indentWith 8 <| String.join "\n," rows )
                    ]
    in
    addEncoder ctx { typeName = name, implementation = impl, generics = generics }


typeAnnotation : TypeAnnotation -> String
typeAnnotation anno =
    case anno of
        Record rec ->
            indentWith 2 <| anonymousRecord rec

        Tuple def ->
            anonymoustuple def

        --            indentWith 8 <| anonymousTuple def
        Typed td ->
            typeDef td


recordField : Name -> { name : String, anno : TypeAnnotation } -> String
recordField recordName { name, anno } =
    """ ("{{fieldName}}", {{encoder}} {{recordName}}{{fieldName}} )"""
        |> interpolateAll
            [ ( "fieldName", name )
            , ( "encoder", typeAnnotation anno )
            , ( "recordName"
              , case recordName of
                    "" ->
                        ""

                    n ->
                        decapitalize n ++ "."
              )
            ]


anonymoustuple : List TypeAnnotation -> String
anonymoustuple types =
    """(\\ ({{args}})->
    Encode.object [
{{rows}}
    ])"""
        |> interpolateAll
            [ ( "args", String.join ", " <| List.map (\x -> "item" ++ String.fromInt x) <| List.range 0 (List.length types - 1) )
            , ( "rows", indentWith 8 <| String.join "\n," <| tupleRows 0 types )
            ]


tupleRows : Int -> List TypeAnnotation -> List String
tupleRows index tas =
    case tas of
        [] ->
            []

        t :: ts ->
            let
                row =
                    """("item{{index}}", {{encoder}} item{{index}}) """
                        |> interpolateAll
                            [ ( "index", String.fromInt index )
                            , ( "encoder", typeAnnotation t )
                            ]
            in
            row :: tupleRows (index + 1) ts


anonymousRecord : RecordDefinition -> String
anonymousRecord def =
    """(\\ {{args}} ->
Encode.object [
{{rows}}
])"""
        |> interpolateAll
            [ ( "rows", indentWith 4 <| String.join "\n, " <| List.map (recordField "") def )
            , ( "args", "{ " ++ (String.join ", " <| List.map (\d -> d.name) def) ++ " }" )
            ]


typeDef : TypeDef -> String
typeDef td =
    case td of
        Type t ->
            case t of
                "String" ->
                    "Encode.string"

                "Int" ->
                    "Encode.int"

                n ->
                    "{{name}}Encoder" |> interpolate "name" (decapitalize n)

        ListDef arg ->
            "Encode.list {{encoder}}" |> interpolate "encoder" (typeAnnotation arg)
