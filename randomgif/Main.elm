module Main exposing (..)

-- We'll need to import some modules for later

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode


-- We'll define the model.  Our RandomGif component just takes a topic and the
-- url for the gif we're displaying.


type alias Model =
    { topic : String
    , gifUrl : String
    }



-- We'll define an `init` function to create our initial data - we're
-- introducing a new concept here.
-- We'll also reference a gif on the internet for our `waiting` state, which
-- happens initially before we've requested anything.


init : String -> ( Model, Cmd Msg )
init topic =
    let
        waitingUrl =
            "https://i.imgur.com/i6eXrfS.gif"
    in
        ( Model topic waitingUrl
        , getRandomGif topic
        )



-- UPDATE
-- Our messages will be either to `RequestMore`, which asks for a new gif,
-- or `NewGif`, which is what we get when the http request completes.
-- It's a Result type, which I don't think we've covered before, so I've linked
-- to the documentation for Result in the resources.
-- Just to reiterate - when we send a Cmd, we're asking the Elm runtime to do
-- some work for us.  When that work is completed, we'll be sent a new Msg that
-- tells us what happened, and that's what our NewGif type describes.


type Msg
    = RequestMore
    | NewGif (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- When we request another gif, we won't update the model but we'll add a new
        -- Cmd that we want to take place - another call to `getRandomGif`
        RequestMore ->
            ( model, getRandomGif model.topic )

        NewGif (Ok url) ->
            ( { model | gifUrl = url }, Cmd.none )

        NewGif (Err _) ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text model.topic ]
        , div [] [ img [ src model.gifUrl ] [] ]
        , button [ onClick RequestMore ] [ text "More, better..." ]
        ]


main =
    Html.program
        { init = init "cats"
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


getRandomGif : String -> Cmd Msg
getRandomGif topic =
    let
        -- We need a URL to get - we'll use the topic to construct a Giphy API
        -- request.
        -- NOTE: This includes a public, shared beta API key
        url =
            "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic

        -- Then we'll create an Http request with `Http.get`, passing it our URL
        -- as well as a Json.Decoder.  I'll explain Decoders a bit more
        -- momentarily.
        request =
            Http.get url decodeGifUrl
    in
        -- Finally, we `send` the request, along with a function that will
        -- handle the returned data - in this case, that function is the
        -- function that builds a `NewGif` message.  This implies that the
        -- return value of the `Http.send` function is a `Result`.
        Http.send NewGif request



-- Now we get to the `decodeGifUrl` function.  This returns a Json.Decoder for a
-- String.  We use a few functions from the `Json.Decode` module, which let us dig
-- into a Json value and extract a string from it.  This results in a decoder
-- that, when provided with a JSON value representing an object with a `data` key
-- which contains an object with an `image_url` key, will return the value in
-- the `image_url` key as a String type.


decodeGifUrl : Decode.Decoder String
decodeGifUrl =
    Decode.at [ "data", "image_url" ] Decode.string
