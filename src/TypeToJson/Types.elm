module TypeToJson.Types exposing (..)


type alias Name =
    String


type ValidType
    = TypeAlias Name GenericsAnnotation TypeAnnotation
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
    | Tuple (List TypeAnnotation)


type TypeDef
    = Type Name
    | ListDef TypeAnnotation
    | MaybeDef TypeAnnotation


type alias RecordDefinition =
    List { name : String, anno : TypeAnnotation }
