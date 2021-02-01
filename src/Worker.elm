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

                typesAndModules =
                    formatModuleString s
            in
            ( s
            , stdout <|
                typesAndModules.moduleString
                    ++ "\n\n"
                    ++ output
                    ++ "\n"
                    ++ typesAndModules.types
            )


type alias TypesAndModules =
    { types : String, moduleString : String }


formatModuleString : String -> TypesAndModules
formatModuleString modules =
    { moduleString =
        String.replace "module TestInputs" "module TestGenerated" <|
            Maybe.withDefault "" (List.head (String.lines modules))
    , types =
        String.replace "module TestInputs" "module TestGenerated" <|
            String.join "\n" <|
                Maybe.withDefault [] (List.tail (String.lines modules))
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    stdin Stdin
