module TestInputs.Combined1 exposing (..)


type Error
    = BadError
    | GoodError String
    | OkError String
    | MehError


type alias TestType =
    { result : Result Error String
    , data : Maybe String
    }
