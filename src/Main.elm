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
import FontAwesome.Styles as Icon
import Html exposing (Html)
import OnTheFlyTest
import TypeToJson exposing (generate)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { input : String, output : String }


example =
    """type Error a = BadError
 |  GoodError String
 | OkError String
 | MyError a

type alias Example =
    { result : Result (Error Int) String
    , data : Maybe String
    }"""


moduleHeader : String
moduleHeader =
    "module Test exposing(..)\n"


init : () -> ( Model, Cmd Msg )
init _ =
    ( parse { input = example, output = "" }, Cmd.none )


parse : Model -> Model
parse model =
    case generate <| moduleHeader ++ model.input of
        Ok ts ->
            { model | output = ts }

        Err e ->
            { model | output = e }



-- UPDATE


type Msg
    = Parse
    | WriteCode String
    | WriteOutput String
    | Example


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WriteCode s ->
            ( parse { model | input = s }, Cmd.none )

        Parse ->
            ( parse model, Cmd.none )

        WriteOutput s ->
            ( { model | output = s }, Cmd.none )

        Example ->
            ( parse { model | input = example }, Cmd.none )



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
                , inputPannels model
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


inputPannels : Model -> Element Msg
inputPannels model =
    column [ centerX, width fill ]
        [ column [ spacing 40, paddingXY 10 20, centerX, width fill ]
            [ multiline [ alignTop, height (fill |> minimum 400) ]
                { onChange = WriteCode
                , text = model.input
                , label = labelAbove [ Font.bold ] (text "Input elm types")
                , placeholder = Just <| placeholder [] (text "Put your elm types here to get Encoders and Decoders")
                , spellcheck = False
                }
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
