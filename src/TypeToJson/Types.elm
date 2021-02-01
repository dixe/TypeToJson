module TypeToJson.Types exposing (..)


type alias Name =
    String


type ValidType
    = TypeAlias Name GenericsAnnotation TypeAnnotation
      --    | CustomType      {nameString Constructor (List Construc2tor)
    | CustomType Name GenericsAnnotation Constructor (List Constructor)


type alias GenericsAnnotation =
    List String


type alias Constructor =
    { name : String
    , arguments : List TypeAnnotation
    }


type TypeAnnotation
    = Record RecordDefinition
    | Typed TypeDef
      --    | Generic (List TypeAnnotation)
    | Tuple (List TypeAnnotation)


type TypeDef
    = Type Name
    | ListDef TypeAnnotation


type alias RecordDefinition =
    List { name : String, anno : TypeAnnotation }
