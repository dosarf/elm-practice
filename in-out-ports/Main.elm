port module Main exposing (..)

import Html exposing (button, div, Html, text)
import Html.Events exposing (onClick)


-- outbound port


port sendCount : Int -> Cmd msg



-- inbount port


port incrementCount : (Int -> msg) -> Sub msg


type alias Model =
    { count : Int
    }


initialModel : ( Model, Cmd Msg )
initialModel =
    ( { count = 0
      }
    , Cmd.none
    )


type Msg
    = SendCount
    | IncrementCount Int


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        SendCount ->
            ( model, sendCount model.count )

        IncrementCount number ->
            ( { model | count = model.count + 1 }, Cmd.none )


view : Model -> Html Msg
view model =
    div
        []
        [ text ("Count: " ++ toString (model.count))
        , button
            [ onClick SendCount ]
            [ text "Send count to JS" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    incrementCount portNumberToMsg


main =
    Html.program
        { init = initialModel
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


portNumberToMsg : Int -> Msg
portNumberToMsg number =
    IncrementCount number
