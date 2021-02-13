module Mapper.FromTypes exposing (fromTypes)

import JsonParser exposing (Json, Member, Name, Number(..), Value(..), parse)
import Types exposing (..)


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
            unimplemented <| "Custom"


typeAnnotation : TypeAnnotation -> Json
typeAnnotation ta =
    case ta of
        Record defs ->
            VObject <| List.map (\def -> Member def.name (typeAnnotation def.anno)) defs

        Typed td ->
            typeDef td

        Tuple tas ->
            unimplemented <| "tuple"


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
                    unimplemented <| "type " ++ n

        ListDef ta ->
            unimplemented "List"

        MaybeDef ta ->
            unimplemented "Maybe"

        DictDef key val ->
            unimplemented "Dict"

        ResultDef err ok ->
            unimplemented "resDef"


unimplemented t =
    VObject [ { name = t, value = VString "Unimpelemted" } ]
