module Main exposing (..)

-- https://www.dailydrip.com/topics/elm/drips/debug

import Time exposing (Time, second)
import AnimationFrame
import Animation exposing (..)
import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick)
import Keyboard.Extra as Keyboard


type alias Model =
    { shape : Shape
    , color : Color
    , position : ( Int, Int )
    , keyboardState : Keyboard.State
    }


type Msg
    = KeyboardExtraMsg Keyboard.Msg
    | Tick Time


init : ( Model, Cmd Msg )
init =
    ( { position = ( 0, 0 )
      , keyboardState = Keyboard.initialState
      , color = red
      , shape = rect 20 20
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    let
        x =
            Tuple.first model.position

        y =
            Tuple.second model.position
    in
        collage 800
            800
            [ model.shape |> filled model.color |> move ( toFloat x, toFloat y ) ]
            |> Element.toHtml


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- case Debug.log "msg" msg of
        KeyboardExtraMsg keyMsg ->
            let
                newModel =
                    { model | keyboardState = Keyboard.update keyMsg model.keyboardState }
                        |> updateColor
            in
                ( newModel
                , Cmd.none
                )

        Tick _ ->
            let
                { x, y } =
                    Keyboard.arrows model.keyboardState

                ( oldX, oldY ) =
                    model.position

                newPosition =
                    ( oldX + x, oldY + y )
            in
                { model | position = newPosition } ! []


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardExtraMsg Keyboard.subscriptions
        , AnimationFrame.diffs Tick
        ]



-- implementation


updateColor : Model -> Model
updateColor model =
    case Keyboard.isPressed Keyboard.Space model.keyboardState of
        True ->
            { model | color = blue }

        False ->
            { model | color = red }
