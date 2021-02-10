module TestInputs.Combined1 exposing (..)


type Error a
    = BadError
    | GoodError String
    | OkError String
    | MehError
    | MyError a


type alias TestType =
    { result : Result (Error Int) String
    , data : Maybe String
    }
