module Mapper.MappedJson exposing (..)

import JsonParser exposing (Json, Member, Number(..), Value(..), parse)


type MappedJson
    = Valid
    | Invalid


process : Json -> MappedJson
process js =
    Invalid
