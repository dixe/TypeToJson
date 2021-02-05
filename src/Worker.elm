port module Worker exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import TypeToJson


type alias Model =
    List TestFile


type alias TestFile =
    { fileName : String, content : String }


port stdin : (Encode.Value -> msg) -> Sub msg


port stdout : Encode.Value -> Cmd msg


main =
    Platform.worker
        { init = \() -> ( [], Cmd.none )
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = Stdin Encode.Value


updateStdIn : Encode.Value -> Model -> ( Model, Cmd Msg )
updateStdIn val model =
    case Decode.decodeValue testFileDecoder val of
        Err err ->
            ( model, stdout <| Encode.string <| Decode.errorToString err )

        --TODO maybe send a msg on stdOutPort
        Ok ({ fileName, content } as m) ->
            let
                output =
                    case TypeToJson.generate content of
                        Ok res ->
                            res

                        Err err ->
                            err

                typesAndModules =
                    formatModuleString content

                tf =
                    { fileName = fileName
                    , content =
                        typesAndModules.moduleString
                            ++ "\n\n"
                            ++ output
                            ++ "\n"
                            ++ typesAndModules.types
                    }
            in
            ( model ++ [ m ]
            , stdout <| testFileEncoder tf
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Stdin val ->
            updateStdIn val model


type alias TypesAndModules =
    { types : String, moduleString : String }


formatModuleString : String -> TypesAndModules
formatModuleString modules =
    { moduleString =
        String.replace "module TestInputs" "module GeneratedTests" <|
            Maybe.withDefault "" (List.head (String.lines modules))
    , types =
        String.join "\n" <|
            List.filter (\x -> not (String.startsWith "import" x)) <|
                Maybe.withDefault [] (List.tail (String.lines modules))
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    stdin Stdin



--DECODER


testFileDecoder : Decode.Decoder TestFile
testFileDecoder =
    Decode.succeed TestFile
        |> required "fileName" Decode.string
        |> required "content" Decode.string



--ENCODER


testFileEncoder : TestFile -> Encode.Value
testFileEncoder testFile =
    Encode.object
        [ ( "fileName", Encode.string testFile.fileName )
        , ( "content", Encode.string testFile.content )
        ]
