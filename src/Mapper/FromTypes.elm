module Mapper.FromTypes exposing (fromTypes)

import JsonParser exposing (Json, Member, Name, Number(..), Value(..), parse)
import Types exposing (..)


type NameModifier
    = MaybeMod
    | None


fromTypes : List ValidType -> Json
fromTypes types =
    VObject <|
        List.map
            (\t ->
                let
                    d =
                        Debug.log "t From " t
                in
                { name =
                    case t of
                        TypeAlias name _ _ ->
                            name

                        CustomType name _ _ _ ->
                            name
                , value = validType t
                }
            )
            types


validType : ValidType -> Json
validType tv =
    case tv of
        TypeAlias name generics ta ->
            typeAnnotation ta

        CustomType name generics c cs ->
            VCustom name <| List.map constructor (c :: cs)


constructor : Constructor -> Json
constructor const =
    case const.arguments of
        [] ->
            VString const.name

        args ->
            constWithArgs const.name args


constWithArgs : String -> List TypeAnnotation -> Json
constWithArgs name args =
    let
        body =
            VObject <| arguments 0 args
    in
    VObject [ { name = name, value = body } ]


arguments : Int -> List TypeAnnotation -> List Member
arguments index args =
    case args of
        [] ->
            []

        a :: rest ->
            { name = "arg" ++ String.fromInt index, value = typeAnnotation a } :: arguments (index + 1) rest


typeAnnotation : TypeAnnotation -> Json
typeAnnotation ta =
    case ta of
        Record defs ->
            VObject <| List.map recordDefinition defs

        Typed td ->
            typeDef td

        Tuple tas ->
            let
                members =
                    arguments 0 tas
            in
            VObject members


recordDefinition : { name : String, anno : TypeAnnotation } -> Member
recordDefinition def =
    Member def.name (typeAnnotation def.anno)


typeDef : TypeDef -> Json
typeDef td =
    case td of
        Type base tas ->
            baseTypeName base

        ListDef ta ->
            VArray [ typeAnnotation ta ]

        MaybeDef ta ->
            VMaybe <| typeAnnotation ta

        DictDef key val ->
            let
                d =
                    Debug.log "key" key

                keyName =
                    case key of
                        Typed t ->
                            case t of
                                Type base _ ->
                                    valueName <| baseTypeName base

                                _ ->
                                    "UnsupportedDictKeyType Record"

                        Tuple _ ->
                            "UnsupportedDictKeyType Tuple"

                        Record _ ->
                            "UnsupportedDictKeyType Record"
            in
            VObject [ { name = keyName, value = typeAnnotation val } ]

        --            VObject [ ( typeAnnotation key, typeAnnotation val ) ]
        ResultDef err ok ->
            unimplemented "resDef"


valueName : Value -> String
valueName val =
    case val of
        VString s ->
            "StringValue"

        VObject _ ->
            "ObjectValue"

        VArray _ ->
            "ArrayValue"

        VNumber nType ->
            case nType of
                NFloat _ ->
                    "FloatValue"

                NInt _ ->
                    "IntValue"

        VTrue ->
            "BoolValue"

        VFalse ->
            "BoolValue"

        VNull ->
            "NullValue"

        VMaybe _ ->
            "MaybeValue"

        VCustom name _ ->
            name

        VTuple _ ->
            "TupleValue"


baseTypeName : BaseType -> Json
baseTypeName base =
    case base of
        TString ->
            VString "StringValue"

        TInt ->
            VNumber <| NInt 0

        TFloat ->
            VNumber <| NFloat 0.0

        TBool ->
            VTrue

        TOther n ->
            VCustom n []


unimplemented t =
    VObject [ { name = t, value = VString "Unimpelemted" } ]
