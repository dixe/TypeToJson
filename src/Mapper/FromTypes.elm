module Mapper.FromTypes exposing (fromTypes)

import JsonParser exposing (Json, Member, Name, Number(..), Value(..), parse)
import Types exposing (..)


type NameModifier
    = MaybeMod
    | None


fromTypes : List ValidType -> Json
fromTypes types =
    let
        d =
            Debug.log "FromTypes " types
    in
    case types of
        t :: [] ->
            validType t

        ls ->
            VArray <| List.map validType ls


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
            unimplemented <| "tuple"


recordDefinition : { name : String, anno : TypeAnnotation } -> Member
recordDefinition def =
    Member def.name (typeAnnotation def.anno)


typeDef : TypeDef -> Json
typeDef td =
    case td of
        Type name tas ->
            case name of
                "String" ->
                    VString "StringValue"

                "Int" ->
                    VNumber <| NInt 23

                n ->
                    unimplemented <| "Type - " ++ n

        ListDef ta ->
            unimplemented "List"

        MaybeDef ta ->
            VMaybe <| typeAnnotation ta

        DictDef key val ->
            unimplemented "Dict"

        ResultDef err ok ->
            unimplemented "resDef"


unimplemented t =
    VObject [ { name = t, value = VString "Unimpelemted" } ]
