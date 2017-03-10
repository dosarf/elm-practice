module Main exposing (..)

import Color
import Collage
import Element
import Keyboard
import Keyboard.Extra
import Html exposing (Html, div)
import Html.Events exposing (onClick)
import Time exposing (Time, second)
import AnimationFrame
import Animation exposing (..)


-- types


type alias Point =
    ( Int, Int )



-- model


type alias Model =
    { points : List Point
    , head : Point
    , keyboardState : Keyboard.Extra.State
    , clock : Time
    , animation : Animation
    }


init : ( Model, Cmd Msg )
init =
    ( { points = List.range -20 0 |> List.map (\i -> ( i, 0 ))
      , head = ( 0, 0 )
      , keyboardState = Keyboard.Extra.initialState
      , clock =
            0
            -- , animation = shakeAnimation 0
      , animation = static 0
      }
    , Cmd.none
    )



-- message


type Msg
    = KeyboardExtraMsg Keyboard.Extra.Msg
    | Tick Time
    | Shake



-- view


view : Model -> Html Msg
view model =
    let
        angle =
            (animate model.clock model.animation) / 360 * 2 * pi
    in
        div []
            [ Collage.collage 800
                800
                [ (drawLine model.points) |> Collage.rotate angle ]
                |> Element.toHtml
            , shakeButton
            ]



-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyboardExtraMsg keyboardMessage ->
            let
                keyboardState =
                    Keyboard.Extra.update keyboardMessage model.keyboardState
            in
                ( { model
                    | keyboardState = keyboardState
                  }
                , Cmd.none
                )

        Tick dt ->
            let
                delta =
                    Keyboard.Extra.arrows model.keyboardState

                ( oldHeadX, oldHeadY ) =
                    model.head

                newHead =
                    ( oldHeadX + delta.x, oldHeadY + delta.y )

                headHasChanged =
                    (delta.x /= 0) || (delta.y /= 0)

                newPoints =
                    if headHasChanged then
                        newHead :: model.points
                    else
                        model.points
            in
                ( { model
                    | head = newHead
                    , points = newPoints
                    , clock = model.clock + dt
                  }
                , Cmd.none
                )

        Shake ->
            ( { model
                | points = []
                , animation = shakeAnimation model.clock
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardExtraMsg Keyboard.Extra.subscriptions
          -- , Time.every (1 / 20 * second) Tick
        , AnimationFrame.diffs Tick
        ]



-- Main


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- helpers


drawLine : List Point -> Collage.Form
drawLine points =
    let
        intPointToFloatPoint : Point -> ( Float, Float )
        intPointToFloatPoint ( x, y ) =
            ( toFloat x, toFloat y )

        shape =
            List.map intPointToFloatPoint points |> Collage.path
    in
        shape
            |> Collage.traced (Collage.solid Color.red)


shakeButton : Html Msg
shakeButton =
    Html.button [ onClick Shake ] [ Html.text "Shake it good" ]


shakeAnimation : Time -> Animation
shakeAnimation t =
    animation t
        |> from 0
        |> to 360
        |> duration (4 * Time.second)
