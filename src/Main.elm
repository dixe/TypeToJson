module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Element exposing (..)
import Element.Font as Font
import Element.Input exposing (button, labelHidden, multiline)
import Html exposing (Html)
import Http
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


test =
    """

type TestType a b
    = C1 a
    | C2 String a
    | C3 b


type Point = P (Int,(String,Int))

type alias P2 = (String, (Int,(Int,(String,Int))))

type Generic = Test { stringField : String, intField : Int }

type Beta = Meta (TT String)




type alias Goal =
    { des : String, test : String, id : Int, newTest : NewTest, nt : NewTest }

"""


moduleHeader : String
moduleHeader =
    "module Test exposing(..)\n"


init : () -> ( Model, Cmd Msg )
init _ =
    ( parse { input = test, output = "" }, Cmd.none )


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WriteCode s ->
            ( parse { model | input = s }, Cmd.none )

        Parse ->
            ( parse model, Cmd.none )

        WriteOutput s ->
            ( { model | output = s }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    layout [ Font.size 20 ]
        (el [ spacing 10, padding 10, centerX, width fill ]
            (column [ centerX, width <| fillPortion 1, width <| fill ]
                [ button [] { label = text "Generate", onPress = Just Parse }
                , row [ spacing 10, padding 10, centerX, width fill ]
                    [ multiline [ alignTop ]
                        { onChange = WriteCode
                        , text = model.input
                        , label = labelHidden "input"
                        , placeholder = Nothing
                        , spellcheck = False
                        }
                    , multiline [ width <| fillPortion 1 ]
                        { onChange = WriteOutput
                        , text = model.output
                        , label = labelHidden "output"
                        , placeholder = Nothing
                        , spellcheck = False
                        }
                    ]
                ]
            )
        )
