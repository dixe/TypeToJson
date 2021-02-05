module TestInputs.NestedRecord exposing (..)


type alias TestType =
    { stringField : String, intField : { stringField : String, floatField : Float } }
