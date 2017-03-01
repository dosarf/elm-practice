port module Main exposing (..)

import Html exposing (button, div, figure, header, main_, img, text, Html)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Http
import Json.Decode
import Json.Decode.Pipeline
import Task
import Time exposing (Time)
import Date exposing (Date)
import Date.Extra.Format


-- basic types


type alias Joke =
    { id : String
    , icon_url : String
    , value : String
    }


initialJoke : Joke
initialJoke =
    { id = "0"
    , icon_url = "/pram.jpg"
    , value = "TODO Chuck Norris"
    }



-- model


type alias Model =
    { joke : Joke
    , requestTimestamp : Time
    }


initialModel : Model
initialModel =
    { joke = initialJoke
    , requestTimestamp = 0.0
    }



-- messages


type Msg
    = NextJoke
    | GotJoke (Result Http.Error Joke)
    | NewTimestamp Time



-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NextJoke ->
            ( model
            , Cmd.batch
                [ getNextJoke
                , Task.perform NewTimestamp Time.now
                ]
            )

        GotJoke (Ok joke) ->
            ( { model | joke = joke }, Cmd.none )

        GotJoke (Err httpError) ->
            let
                _ =
                    Debug.log "HTTP error: " httpError
            in
                ( model
                , explainHttpError httpError |> alertHttpError
                )

        NewTimestamp newTimestamp ->
            ( { model | requestTimestamp = newTimestamp }, Cmd.none )



-- view


view : Model -> Html Msg
view model =
    div
        []
        [ div
            []
            [ button
                [ onClick NextJoke ]
                [ text "Kickme" ]
            ]
        , header
            []
            [ text ("Joke id: " ++ model.joke.id) ]
        , main_
            []
            [ text model.joke.value
            , text ("Requested at: " ++ (formatTimestamp model.requestTimestamp))
            ]
        , figure
            []
            [ img
                [ src model.joke.icon_url ]
                []
            ]
        ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- ports


port alertHttpError : String -> Cmd msg



-- main


main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- implementations


getNextJoke : Cmd Msg
getNextJoke =
    let
        request =
            Http.get "https://api.chucknorris.io/jokes/random" decodeJokeJson
    in
        Http.send GotJoke request


decodeJokeJson : Json.Decode.Decoder Joke
decodeJokeJson =
    Json.Decode.Pipeline.decode Joke
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "icon_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "value" Json.Decode.string



{-
   Json.Decode.map3 Joke
       (Json.Decode.field "id" Json.Decode.string)
       (Json.Decode.field "icon_url" Json.Decode.string)
       (Json.Decode.field "value" Json.Decode.string)
-}


explainHttpError : Http.Error -> String
explainHttpError httpError =
    case httpError of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Connectiong timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus response ->
            "Bad status: " ++ (toString response.status.code) ++ " " ++ response.status.message

        Http.BadPayload explanation response ->
            "Bad payload: " ++ explanation ++ " " ++ (toString response.status.code) ++ " " ++ response.status.message


formatTimestamp : Time -> String
formatTimestamp timestamp =
    let
        date =
            Date.fromTime timestamp
    in
        Date.Extra.Format.isoString date
