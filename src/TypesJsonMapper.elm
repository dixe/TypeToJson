module TypesJsonMapper exposing (fromJson, fromTypes)

import JsonParser as Json exposing ((Json, Member(..), Name, Number(..), Value(..), parse))
import Types


fromJson : Json -> Result String (List ValidTypes)
fromJson json =
    Err "Unimplemented FromJson"


fromTypes : List ValidTypes        -> Result String Json
fromTypes types = Err "Unimplemented FromTypes"
