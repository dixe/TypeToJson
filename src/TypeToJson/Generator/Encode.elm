module TypeToJson.Generator.Encode exposing (encoderDeclaration, generate)

import List.Extra
import String.Extra exposing (decapitalize)
import TypeToJson.Generator.Types exposing (..)
import TypeToJson.Types exposing (..)
import TypeToJson.Utilities exposing (..)


type alias Depth =
    Int


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
        genericTypeArgs =
            String.join " " <| List.map (\x -> "( " ++ x ++ " -> Encode.Value) " ++ " ->") d.generics

        genericArgs =
            String.join " " <| List.map (\x -> x ++ "Encoder") d.generics
    in
    """
{{name}}Encoder : {{genericTypeArgs}} {{typeArgs}} {{generics}} -> Encode.Value
{{name}}Encoder {{genericsArgs}} {{args}} =
{{impl}}"""
        |> interpolateAll
            [ ( "name", decapitalize d.typeName )
            , ( "Name", d.typeName )
            , ( "impl", indent d.implementation )
            , ( "typeArgs", d.typeName )
            , ( "generics", String.join " " d.generics )
            , ( "args", decapitalize d.typeName )
            , ( "genericsArgs", genericArgs )
            , ( "genericTypeArgs", genericTypeArgs )
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


constructorWithArgument : Depth -> Name -> List TypeAnnotation -> String
constructorWithArgument depth name args =
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
            , ( "argRows", indentWith 12 <| "[" ++ (String.join "\n, " <| arguments depth 0 args) ++ "\n]" )
            ]


argumentsList : Int -> List String
argumentsList count =
    List.map (\x -> "arg" ++ String.fromInt x) <| List.range 0 (count - 1)


arguments : Depth -> Int -> List TypeAnnotation -> List String
arguments depth index tas =
    case tas of
        [] ->
            []

        t :: ts ->
            ("""("arg{{index}}", {{encoder}} arg{{index}})"""
                |> interpolateAll
                    [ ( "index", String.fromInt index )
                    , ( "encoder", typeAnnotation depth t )
                    ]
            )
                :: arguments depth (index + 1) ts


constructor : Constructor -> String
constructor c =
    if c.arguments == [] then
        constructorNoArgument c.name

    else
        constructorWithArgument 0 c.name c.arguments


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
        rows : Int -> List TypeAnnotation -> List String
        rows index tas =
            case tas of
                [] ->
                    []

                t :: ts ->
                    ("""("item{{index}}", {{encoder}} item{{index}})"""
                        |> interpolateAll
                            [ ( "index", String.fromInt index )
                            , ( "encoder", typeAnnotation 0 t )
                            ]
                    )
                        :: rows (index + 1) ts

        impl =
            """(\\ ({{args}}) ->
    Encode.object
           [
{{rows}}
           ]) {{name}} """
                |> interpolateAll
                    [ ( "rows", indentWith 12 <| String.join "\n," (rows 0 types) )
                    , ( "name", decapitalize name )
                    , ( "args", String.join ", " <| List.map (\x -> "item" ++ String.fromInt x) <| List.range 0 (List.length types - 1) )
                    ]
    in
    addEncoder ctx { typeName = name, implementation = impl, generics = generics }


record : Name -> GenericsAnnotation -> RecordDefinition -> Ctx -> Ctx
record name generics def ctx =
    let
        rows =
            List.map (recordField 0 name) def

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


typeAnnotation : Depth -> TypeAnnotation -> String
typeAnnotation depth anno =
    case anno of
        Record rec ->
            indentWith 2 <| anonymousRecord depth rec

        Tuple def ->
            anonymoustuple (depth + 1) def

        Typed td ->
            typeDef depth td


recordField : Depth -> Name -> { name : String, anno : TypeAnnotation } -> String
recordField depth recordName { name, anno } =
    """ ("{{fieldName}}", {{encoder}} {{recordName}}{{fieldName}} )"""
        |> interpolateAll
            [ ( "fieldName", name )
            , ( "encoder", typeAnnotation depth anno )
            , ( "recordName"
              , case recordName of
                    "" ->
                        ""

                    n ->
                        decapitalize n ++ "."
              )
            ]


anonymoustuple : Depth -> List TypeAnnotation -> String
anonymoustuple depth types =
    let
        itemsWithDepth =
            "item" ++ String.fromInt depth ++ "_"
    in
    """(\\ ({{args}}) ->
    Encode.object [
{{rows}}
    ])"""
        |> interpolateAll
            [ ( "args", String.join ", " <| List.map (\x -> itemsWithDepth ++ String.fromInt x) <| List.range 0 (List.length types - 1) )
            , ( "rows", indentWith 8 <| String.join "\n," <| tupleRows itemsWithDepth 0 types )
            ]


tupleRows : String -> Int -> List TypeAnnotation -> List String
tupleRows itemWithDepth index tas =
    case tas of
        [] ->
            []

        t :: ts ->
            let
                row =
                    """("item{{index}}", {{encoder}} {{itemWithDepth}}{{index}}) """
                        |> interpolateAll
                            [ ( "index", String.fromInt index )
                            , ( "encoder", typeAnnotation 0 t )
                            , ( "itemWithDepth", itemWithDepth )
                            ]
            in
            row :: tupleRows itemWithDepth (index + 1) ts


anonymousRecord : Depth -> RecordDefinition -> String
anonymousRecord depth def =
    """(\\ {{args}} ->
Encode.object [
{{rows}}
])"""
        |> interpolateAll
            [ ( "rows", indentWith 4 <| String.join "\n, " <| List.map (recordField depth "") def )
            , ( "args", "{ " ++ (String.join ", " <| List.map (\d -> d.name) def) ++ " }" )
            ]


typeDef : Depth -> TypeDef -> String
typeDef depth td =
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
            "Encode.list {{encoder}}" |> interpolate "encoder" (typeAnnotation depth arg)
