port module Worker exposing (..)

import TypeToJson


type alias Model =
    String


port stdin : (String -> msg) -> Sub msg


port stdout : String -> Cmd msg


main =
    Platform.worker
        { init = \() -> ( "", Cmd.none )
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = Stdin String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Stdin s ->
            let
                output =
                    case TypeToJson.generate s of
                        Ok res ->
                            res

                        Err err ->
                            err
            in
            ( s, stdout <| s ++ "\n\n" ++ output ++ "\n" )


subscriptions : Model -> Sub Msg
subscriptions _ =
    stdin Stdin
