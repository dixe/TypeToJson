module TypeToJson.Generator.Types exposing (..)

import TypeToJson.Types exposing (..)


type alias Ctx =
    { decoders : List Coder
    , encoders : List Coder
    , imports : List String
    }


empty : Ctx
empty =
    { decoders = []
    , encoders = []
    , imports = []
    }


type alias Coder =
    { typeName : String
    , implementation : String
    , generics : GenericsAnnotation
    , mappedGenerics : GenericsAnnotation
    }
