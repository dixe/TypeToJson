module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button, labelAbove, labelHidden, multiline, placeholder)
import FontAwesome.Attributes as FAA
import FontAwesome.Brands exposing (github)
import FontAwesome.Icon as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import Generators exposing (generate, generateFromJson)
import Html exposing (Html)
import JsonParser as Json
import OnTheFlyTest



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type ViewOption
    = Row
    | Column


type alias Model =
    { input : String, output : String, inputJson : String, jsonResult : String, viewOption : ViewOption }


example =
    """type Error a = BadError
 | GoodError (Dict Int String)
 | OkError String
 | MyError a

type alias Example =
    { result : Result (Error Int) String
    , data : Maybe String
    }"""


example2 =
    """type alias Example =
    { result : String
    , data : Int
    }"""


moduleHeader : String
moduleHeader =
    "module Test exposing(..)\n"


init : () -> ( Model, Cmd Msg )
init _ =
    ( parseFromType { input = example, output = "", inputJson = "", jsonResult = "", viewOption = Column }, Cmd.none )


parseFromType : Model -> Model
parseFromType model =
    case generate <| moduleHeader ++ model.input of
        Ok ts ->
            { model | output = ts.output, inputJson = ts.inputJson }

        Err e ->
            { model | output = e }


parseFromJson : Json.Json -> Model -> Model
parseFromJson json model =
    case generateFromJson json of
        Ok out ->
            { model | output = out.output }

        Err out ->
            { model | output = out }



-- UPDATE


type Msg
    = Parse
    | WriteCode String
    | WriteJson String
    | WriteOutput String
    | Example
    | ChangeView ViewOption


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Parse ->
            ( parseFromType model, Cmd.none )

        WriteCode s ->
            ( parseFromType { model | input = s }, Cmd.none )

        WriteOutput s ->
            ( { model | output = s }, Cmd.none )

        WriteJson s ->
            let
                parsed =
                    Json.parse s

                m =
                    case parsed of
                        -- TODO update inputTypes and parse
                        Ok json ->
                            parseFromJson json model

                        Err errs ->
                            { model | output = "JsonError: \n" ++ errs }

                d =
                    Debug.log "" parsed
            in
            ( { m | inputJson = s }, Cmd.none )

        Example ->
            ( parseFromType { model | input = example }, Cmd.none )

        ChangeView viewOption ->
            ( { model | viewOption = viewOption }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    layout [ Font.size 20, Background.color background ]
        (el
            [ Background.color white
            , spacing 10
            , centerX
            , width (fill |> maximum 1200)
            , paddingXY 10 40
            ]
            (column [ centerX, width <| fillPortion 1, width fill ]
                [ infoPannel model
                , dataPannels model
                , aboutPannel
                ]
            )
        )


infoPannel : Model -> Element Msg
infoPannel model =
    column [ width fill ]
        [ el [ Font.size 30, centerX ] (text "Elm type to Json Encoder and Decoders")
        , column [ spacing 10, paddingXY 40 20 ]
            [ paragraph [ Font.size 20 ]
                [ text "Type or copy your elm types into the input field."
                ]
            , paragraph [ Font.size 20 ]
                [ text " As you type the output field will update and show the Encoders and Decoders along with the imports being used."
                ]
            ]
        ]


dataPannels : Model -> Element Msg
dataPannels model =
    let
        viewMode =
            case model.viewOption of
                Row ->
                    column

                Column ->
                    row
    in
    row [ width fill ]
        [ el [ alignLeft, alignTop, paddingXY 0 45 ] (changeViewButton model)
        , column [ centerX, width fill ]
            [ viewMode [ spacingXY 10 30, paddingXY 10 20, centerX, width fill ]
                [ el [ width <| fillPortion 1, height fill ] (inputPannel model)
                , multiline [ width <| fillPortion 1, height fill ]
                    { onChange = WriteOutput
                    , text = model.output
                    , label = labelAbove [ Font.bold ] (text "Encoder and Decoders")
                    , placeholder = Nothing
                    , spellcheck = False
                    }
                ]
            , if model.input == "" then
                button exampleButton { label = text "Give me an example", onPress = Just Example }

              else
                Element.none
            ]
        ]


inputPannel : Model -> Element Msg
inputPannel model =
    column [ alignTop, width fill, spacingXY 0 10 ]
        [ multiline [ alignTop, height (shrink |> minimum 200) ]
            { onChange = WriteCode
            , text = model.input
            , label = labelAbove [ Font.bold ] (text "Input elm types")
            , placeholder = Just <| placeholder [] (text "Put your elm types here to get Encoders and Decoders")
            , spellcheck = False
            }
        , multiline [ alignTop, height (shrink |> minimum 200) ]
            { onChange = WriteJson
            , text = model.inputJson
            , label = labelAbove [ Font.bold ] (text "Input json")
            , placeholder = Just <| placeholder [] (text "Put your json here to get Encoders and Decoders")
            , spellcheck = False
            }
        ]


aboutPannel : Element msg
aboutPannel =
    column [ width fill, spacing 10, paddingXY 40 20 ]
        [ el [ Font.bold ] (text "About")
        , column [ spacing 10, paddingXY 0 10 ]
            [ paragraph [ Font.size 20 ]
                [ text "This project was inspired by NoRedInks "
                , newTabLink [ Font.color linkBlue ]
                    { url = "https://noredink.github.io/json-to-elm/"
                    , label = text " elm-to-json"
                    }
                , text ". I found working from json types was annoying and wanted to try to work directly from elm types, since you already have them."
                ]
            , paragraph [ Font.size 20 ]
                [ text "Use of this project is mostly to get a base for Encoders and Decoder that can be tweaked to your own json. Or for project where the json is controlled by you. E.g. storing to local storage or sending things out in ports to your own javascript. The Encoders/Decoders can also be used as a base and tweeked to suit the actual json you have."
                ]
            , paragraph [ Font.size 20 ]
                [ text "This project is build using the "
                , newTabLink [ Font.color linkBlue ]
                    { url = "https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/"
                    , label = text "elm-syntax"
                    }
                , text " package to parse the input"
                ]
            ]
        , newTabLink [ centerX ]
            { url = "https://github.com/dixe/TypeToJson/"
            , label =
                row [ Font.size 20 ]
                    [ html <| Html.div [] [ Icon.css, Icon.viewIcon github ]
                    , el [ Font.color linkBlue ] (text " Github Link")
                    ]
            }
        ]


changeViewButton : Model -> Element Msg
changeViewButton model =
    let
        ( icon, changeTo ) =
            case model.viewOption of
                Row ->
                    ( Icon.columns, Column )

                Column ->
                    ( Icon.bars, Row )
    in
    button [] { label = html <| Icon.viewIcon icon, onPress = Just <| ChangeView changeTo }



--exampleButton : List Attribute


exampleButton =
    [ Background.color <| rgb255 50 200 25, centerX, padding 5, Border.rounded 5 ]



--COLORS


background : Color
background =
    rgb255 200 200 200


linkBlue : Color
linkBlue =
    rgb255 6 69 173


white : Color
white =
    rgb255 250 250 250
