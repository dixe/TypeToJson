module Mapper.FromJson exposing (fromJson)

import JsonParser exposing (Json)
import Mapper.MappedJson as MJ
import Types exposing (ValidType(..))


fromJson : Json -> Result String (List ValidType)
fromJson js =
    let
        mapped =
            MJ.process js
    in
    Err "Unimplemented"
