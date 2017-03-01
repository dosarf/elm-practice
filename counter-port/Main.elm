port module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)


type alias Model =
    { count : Int
    , increment : Int
    , decrement : Int
    }



-- inbound ports


port jsMsgs : (Int -> msg) -> Sub msg


port storageInput : (Model -> msg) -> Sub msg



-- outbound ports


port increment : Int -> Cmd msg


port decrement : Int -> Cmd msg


port storage : Model -> Cmd msg


type Msg
    = Increment
    | Decrement
    | NoOp
    | Set Model


initialModel : Model
initialModel =
    { count = 0
    , increment = 0
    , decrement = 0
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            let
                newModel =
                    { model
                        | count = model.count + 1
                        , increment = model.increment + 1
                    }
            in
                ( newModel, storage newModel )

        Decrement ->
            let
                newModel =
                    { model
                        | count = model.count - 1
                        , decrement = model.decrement + 1
                    }
            in
                ( newModel, storage newModel )

        NoOp ->
            ( model, Cmd.none )

        Set storedModel ->
            ( storedModel, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (toString model.count) ]
        , button [ onClick Increment ] [ text "+" ]
        , h3 [] [ text ("- clicked " ++ (toString model.decrement) ++ " times") ]
        , h3 [] [ text ("+ clicked " ++ (toString model.increment) ++ " times") ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ jsMsgs mapJsMsg
        , storageInput Set
        ]


main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


mapJsMsg : Int -> Msg
mapJsMsg int =
    case int of
        1 ->
            Increment

        (-1) ->
            Decrement

        _ ->
            NoOp
