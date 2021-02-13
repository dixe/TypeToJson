module Generator.Types exposing (..)

import Types exposing (..)


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
