port module Main exposing (..)

import Html.Events exposing (onClick)
import Html exposing (Html, button, div, h1, section, text)
import Window
import Task
import Mouse


-- ports


port audio : Model -> Cmd msg



-- messages


type Msg
    = IncrementGain
    | DecrementGain
    | IncrementFrequency
    | DecrementFrequency
    | UpdateDimensions { width : Int, height : Int }
    | UpdateMouse { x : Int, y : Int }
    | NoOp



-- model


type alias Model =
    { gain : Float
    , frequency : Float
    , windowWidth : Int
    , windowHeight : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { gain = 0.001
      , frequency = 3000
      , windowWidth = 100
      , windowHeight = 100
      }
    , getInitialWindowSize
    )



-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        IncrementGain ->
            let
                newModel =
                    { model | gain = model.gain + 0.001 }
            in
                ( newModel, audio newModel )

        DecrementGain ->
            let
                newModel =
                    { model | gain = model.gain - 0.001 }
            in
                ( newModel, audio newModel )

        IncrementFrequency ->
            let
                newModel =
                    { model | frequency = model.frequency + 100 }
            in
                ( newModel, audio newModel )

        DecrementFrequency ->
            let
                newModel =
                    { model | frequency = model.frequency - 100 }
            in
                ( newModel, audio newModel )

        UpdateDimensions { width, height } ->
            let
                newModel =
                    { model
                        | windowWidth = width
                        , windowHeight = height
                    }
            in
                ( newModel, Cmd.none )

        UpdateMouse { x, y } ->
            let
                -- gain is the percentage you are across the screen, from left to right, mapped from 0 to 0.03
                newGain =
                    ((toFloat x) / (toFloat model.windowWidth)) * 0.03

                -- frequency is the percentage you are vertically down the screen, mapped from 0 to 6000
                newFrequency =
                    ((toFloat y) / (toFloat model.windowHeight)) * 6000.0

                newModel =
                    { model | frequency = newFrequency, gain = newGain }
            in
                ( newModel, audio newModel )

        NoOp ->
            ( model, Cmd.none )



-- view


view : Model -> Html Msg
view model =
    div []
        [ section []
            [ h1 [] [ text "Gain" ]
            , button [ onClick IncrementGain ] [ text "+" ]
            , text (toString model.gain)
            , button [ onClick DecrementGain ] [ text "-" ]
            ]
        , section []
            [ h1 [] [ text "Frequency" ]
            , button [ onClick IncrementFrequency ] [ text "+" ]
            , text (toString model.frequency)
            , button [ onClick DecrementFrequency ] [ text "-" ]
            ]
        , section []
            [ h1 [] [ text "Window dimensions" ]
            , text ("width: " ++ (toString model.windowWidth))
            , text ("height: " ++ (toString model.windowHeight))
            ]
        ]



-- subscription


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes UpdateDimensions
        , Mouse.moves UpdateMouse
        ]



-- main


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- stuff


getInitialWindowSize : Cmd Msg
getInitialWindowSize =
    Task.perform UpdateDimensions Window.size
