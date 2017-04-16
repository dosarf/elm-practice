module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import About
import Route


type alias Model =
    { route : Route.Model
    }


type Msg
    = NoOp


init : ( Model, Cmd Msg )
init =
    { route = Route.init (Just Route.Home)
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    let
        body =
            case model.route of
                Just (Route.Home) ->
                    About.view

                Just (Route.Topics) ->
                    text "topics view goes here"

                Nothing ->
                    text "Not found!"
    in
        div []
            [ navigationView model
            , body
            ]


navigationView : Model -> Html Msg
navigationView model =
    nav []
        [ ul []
            [ li [] [ a [ href "#" ] [ text "Home" ] ]
            , li [] [ a [ href "#" ] [ text "Topics" ] ]
            ]
        ]


currentPageView : Model -> Html Msg
currentPageView model =
    text "home page"


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
